package vexiiriscv.test

import rvls.spinal.{TraceBackend, TraceIo}
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.database.Element
import vexiiriscv.Global.PC_WIDTH
import vexiiriscv._
import vexiiriscv.decode.Decode
import vexiiriscv.execute.lsu._
import vexiiriscv.fetch.FetchPipelinePlugin
import vexiiriscv.memory.{MmuPlugin, PmpPlugin, PmpService}
import vexiiriscv.misc.{PerformanceCounterPlugin, PrivilegedPlugin}
import vexiiriscv.riscv.FloatRegFile
//import vexiiriscv.execute.LsuCachelessPlugin
import vexiiriscv.fetch.Fetch
import vexiiriscv.riscv.{IntRegFile, Riscv, RiscvPlugin}
import vexiiriscv.test.konata.{Comment, Flush, Retire, Spawn, Stage}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object VexiiRiscvProbe {
  def amoWriteValue(operation: Int, bytes: Int, operand: Long, oldMemoryValue: Long): Long = {
    import LsuCachelessBusAmo._

    val bits = bytes match {
      case 1 | 2 | 4 | 8 => bytes * 8
      case _ => throw new IllegalArgumentException(s"Unsupported AMO width $bytes")
    }
    val mask = if(bits == 64) -1L else (1L << bits) - 1
    def truncated(value: Long) = value & mask
    def signed(value: Long) = if(bits == 64) value else truncated(value) << (64 - bits) >> (64 - bits)

    val rf = truncated(operand)
    val mem = truncated(oldMemoryValue)
    val result = operation match {
      case AMOSWAP => rf
      case AMOADD  => rf + mem
      case AMOXOR  => rf ^ mem
      case AMOAND  => rf & mem
      case AMOOR   => rf | mem
      case AMOMIN  => if(signed(rf) < signed(mem)) rf else mem
      case AMOMAX  => if(signed(rf) > signed(mem)) rf else mem
      case AMOMINU => if(java.lang.Long.compareUnsigned(rf, mem) < 0) rf else mem
      case AMOMAXU => if(java.lang.Long.compareUnsigned(rf, mem) > 0) rf else mem
      case _ => throw new IllegalArgumentException(f"Unsupported AMO operation 0x$operation%X")
    }
    truncated(result)
  }
}

/**
 * VexiiRiscvProbe can be used in a simulation to probe the activities of VexiiRiscv
 * and notifies a list of TraceBackend with what happened (ex commit, memory load, memory store, trap, ...)
 *
 * There is a few useful backends :
 * - RVLS to check that the simulated VexiiRiscv CPU is doing things right
 * - A file backend, to keep a text file trace of what happened (instead of having to look into a waveform)
 *
 * It also keep a trace of various performance metrics, as the IPC, branch miss rate, ...
 */
class VexiiRiscvProbe(cpu : VexiiRiscv, kb : Option[konata.Backend], var withRvls : Boolean = true) {
  var enabled = true
  var trace = true
  var checkLiveness = true
  var livenessThreshold = 32000l
  var backends = ArrayBuffer[TraceBackend]()
  val commitsCallbacks = ArrayBuffer[(Int, Long) => Unit]()
  val autoStoreBroadcast = cpu.host.get[LsuCachelessPlugin].nonEmpty
  val nativeCacheless = cpu.host.get[LsuCachelessPlugin].exists(p => !p.getLsuCachelessBus().cmd.valid.isDirectionLess)

  val hartsIds = cpu.host.get[PrivilegedPlugin].get.hartIds

  def get[T](e : Element[T]) = cpu.database(e)
  val xlen = get(Riscv.XLEN)
  val floatOr = get(Riscv.RVD).mux(0, 0xFFFFFFFF00000000l)
  val hartsCount = get(Global.HART_COUNT)
  val fetchIdWidth = get(Fetch.ID_WIDTH)
  val decodeIdWidth = get(Decode.DOP_ID_WIDTH)
  val microOpIdWidth = get(Decode.UOP_ID_WIDTH)
  val microOpIdMask = (1 << microOpIdWidth)-1
  val withFetch = true

  val disass = try {
    if(!withRvls) 0 else rvls.jni.Frontend.newDisassemble(xlen)
  } catch {
    case e : Throwable => withRvls = false; 0
  }

  val harts = hartsIds.map(new HartCtx(_)).toArray
  val wbp = cpu.host[WhiteboxerPlugin].logic.get
  val proxies = new wbp.Proxies(){
    override def interrupt(hartId: Int, intId: Int, value: Boolean): Unit = {
      backends.foreach(_.setInterrupt(hartsIds(hartId), intId, value))
    }
  }

  def add(tracer: TraceBackend): this.type = {
    backends += tracer
    harts.foreach(_.add(tracer))
    proxies.interrupts.sync()
    this
  }

  def addHead(tracer: TraceBackend): this.type = {
    backends.insert(0, tracer)
    harts.foreach(_.add(tracer))
    proxies.interrupts.sync()
    this
  }

  // Figure out the PMA (Physical Memory Attributes) from the plugins themselves and notify the backends.
  def autoRegions(): Unit = {
    cpu.host.services.foreach {
      case p: LsuCachelessPlugin => p.regions.foreach { region =>
        backends.foreach { b =>
          region.mapping match {
            case SizeMapping(base, size) => for(hartId <- hartsIds) b.addRegion(hartId, region.isIo.toInt, base.toLong, size.toLong)
          }
        }
      }
      case p: LsuPlugin => p.ioRegions.foreach { region =>
        backends.foreach { b =>
          region.mapping match {
            case SizeMapping(base, size) => for(hartId <- hartsIds) b.addRegion(hartId, region.isIo.toInt, base.toLong, size.toLong)
          }
        }
      }
      case p: LsuL1Plugin => p.regions.foreach { region =>
        backends.foreach { b =>
          region.mapping match {
            case SizeMapping(base, size) => for(hartId <- hartsIds) b.addRegion(hartId, region.isIo.toInt, base.toLong, size.toLong)
          }
        }
      }
      case p =>
    }
  }

  def flush(): Unit = {

  }

  def close(): Unit = {
    harts.foreach(_.close())
    if(withRvls) rvls.jni.Frontend.deleteDisassemble(disass)
  }

  def clearStats(): Unit = {
    for (hart <- harts) {
      hart.jbStats.clear()
      hart.branchStats.clear()
      hart.commits = 0
    }
    statsCycleOffset = cycle
  }

  var statsCycleOffset = 0l
  def getStats(): String = {
    val str = new StringBuilder()
    str ++= "### Stats ###\n"
    for (hart <- harts) {
      val stats = hart.jbStats.toArray.sortBy(_._2.failed)
      val total = new JbStats()
      for ((pc, data) <- stats) {
        total.count += data.count
        total.failed += data.failed
        total.taken += data.taken
      }
//      for ((pc, data) <- stats) {
//        str ++= f"- 0x${pc}%08X : ${data.toString()}\n"
//      }
      str ++= f"kind :  miss / times     miss  taken\n"
      str ++= f"J/B  : ${total.toString()}\n"
      str ++= f"  B  : ${hart.branchStats.toString()}\n"
    }

    def cycleRatio(times: Long) = {
      val rate = (1000f * times / (cycle-statsCycleOffset)).toInt
      f"${times}%7d / ${cycle-statsCycleOffset}%7d ${rate / 10}%3d.${rate % 10}%%"
    }

    for ((hw, i) <- wbp.perf.dispatchFeedCounters.zipWithIndex) {
      str ++= f"Dispatch  $i   : ${cycleRatio(hw.toLong)}\n"
    }
    for ((hw, i) <- wbp.perf.candidatesCountCounters.zipWithIndex) {
      str ++= f"Candidate $i   : ${cycleRatio(hw.toLong)}\n"
    }
    str ++= f"Dispatch halt : ${cycleRatio(wbp.perf.dispatchHazardsCounter.toLong)}\n"
    str ++= f"Execute  halt : ${cycleRatio(wbp.perf.executeFreezedCounter.toLong)}\n"
    str ++= f"IPC           : ${cycleRatio(harts.map(_.commits).sum)}\n"
    str.toString()
  }

  onSimEnd(close())

  def pcExtends(pc: Long) = if (xlen == 32) (pc << 32) >> 32 else pc
  def xlenExtends(value: Long) = if (xlen == 32) (value << 32) >> 32 else value

  class JbStats(){
    var count = 0l
    var failed = 0l
    var taken = 0l

    def clear(): Unit = {
      count = 0
      failed = 0
      taken = 0
    }

    override def toString(): String ={
      val rate = (1000f*failed/count).toInt
      val rate2 = (1000f*taken/count).toInt
      f"${failed}%5d / ${count}%5d ${rate/10}%3d.${rate%10}%% ${rate2/10}%3d.${rate2%10}%%"
    }
  }

  class HartCtx(val hartId : Int) {
    val fetch = Array.fill(1 << fetchIdWidth)(new FetchCtx())
    val decode = Array.fill(1 << decodeIdWidth)(new DecodeCtx())
    val microOp = Array.fill(1 << microOpIdWidth)(new MicroOpCtx())
    var lastUopId = -1l

    var microOpRetirePtr, microOpAllocPtr = 0
    var lastCommitAt = 0l

    var commits = 0l

    val pendingTraps = mutable.Queue[(Boolean, Int, Int)]()

    val jbStats = mutable.HashMap[Long, JbStats]()
    val branchStats = new JbStats()

    val konataThread = kb.map(_.newThread())

    def add(tracer: TraceBackend): Unit = {
      for (hartId <- hartsIds) {
        val csrp = cpu.host.get[PrivilegedPlugin] match {
          case Some(x) => "M" + x.p.withSupervisor.mux("S", "") + x.p.withUser.mux("U", "")
          case None => "M"
        }
        val riscvPlugin = cpu.host[RiscvPlugin]
        val isa = s"RV${xlen}${ExtensionManager.getIsaStr(riscvPlugin.isa)}"
        tracer.newCpuMemoryView(hartId, 16, 1 << Decode.STORE_ID_WIDTH)
        tracer.newCpu(
          hartId = hartId,
          isa = isa,
          priv = csrp,
          physWidth = get(Global.PHYSICAL_WIDTH),
          pmpNum = cpu.host.get[PmpService].map(_.getPmpNum).getOrElse(0),
          triggerCount = cpu.host.get[PrivilegedPlugin].map(_.p.debugTriggers).getOrElse(0),
          asidWidth = cpu.host.get[MmuPlugin].map(_.asidWidth).getOrElse(0),
          memoryViewId = hartId
        )
        val pc = if(xlen == 32) 0x80000000l else 0x80000000l
        tracer.setPc(hartId, pc)
      }
    }

    def close(): Unit = {
      konataThread.foreach(_.cycleLock = Long.MaxValue)
    }
  }

  class FetchCtx() {
    var spawnAt = 1l
  }

  class DecodeCtx() {
    var fetchId = -1
    var spawnAt = 0l
    var fireAt = 0l
    var pc = 0l
  }


  class MicroOpCtx() {
    var fetchId = -1
    var decodeId = -1
    var spawnAt = -1l
    var issueAt = -1l
    var executeAt = -1l
    var completionAt = -1l
    var flushAt = -1l
    var retireAt = -1l
    var instruction = -1l

    def spawned = spawnAt != -1
    def done = !spawned || retireAt != -1 || flushAt != -1
    def didCommit = done && commit

    var trap, commit = false

    var integerWriteValid = false
    var integerWriteData = -1l
    var floatWriteValid = false
    var floatWriteData = -1l
    var floatFlags = -1

    var csrValid = false
    var csrWriteDone = false
    var csrReadDone = false
    var csrAddress = -1
    var csrWriteData = -1l
    var csrReadData = -1l

    var lsuAddress = -1l
    var lsuLen = 0
    var lsuAmo = false
    var storeValid = false
    var storeData = -1l
    var storeSqId = -1
    var loadValid = false
    var loadLqId = 0
    var loadData = -1l

    var isSc = false
    var scFailure = false


    def traceT2s(cycle : Long) = cycle match {
      case 0l => ""
      case v => v.toString
    }
    def toKonata(hart : HartCtx): Unit = {
      if(!trace) return
      val fetch = hart.fetch(fetchId)
      val decode = hart.decode(decodeId)
      val instruction = if(withRvls) rvls.jni.Frontend.disassemble(disass, this.instruction) else "? rvls disabled ?"

      val i = new konata.Instruction()
      if (fetch.spawnAt != -1) {
        i += new Spawn(fetch.spawnAt, hart.hartId)
        i += new Stage(fetch.spawnAt, "A")
        if(withFetch) i += new Stage(fetch.spawnAt+1, "F")
      }
      if (decode.spawnAt != -1) {
        i += new Comment(decode.spawnAt, f"${decode.pc}%X : $instruction")
      }
      if(decode.fireAt != -1){
        i += new Stage(decode.fireAt+1, "D")
      }
      if (executeAt != -1) i += new Stage(executeAt, "E")
      if (didCommit) {
        i += new Retire(retireAt)
      } else {
        i += new Flush(flushAt max retireAt)
      }
      kb.foreach(_.insert(i))
    }

    def clear() {
      fetchId = -1
      decodeId = -1
      spawnAt = -1l
      issueAt = -1l
      executeAt = -1l
      completionAt = -1l
      flushAt = -1l
      retireAt = -1l

      trap = false
      commit = false
      integerWriteValid = false;
      floatWriteValid = false;
      csrValid = false;
      csrWriteDone = false;
      csrReadDone = false;
      floatFlags = 0;
      loadValid = false
      storeValid = false
      isSc = false
      lsuAmo = false
    }

    clear()
  };


  val sizeMask = Array(0xFFl, 0xFFFFl, 0xFFFFFFFFl, -1l)

  val lsuClpb = cpu.host.get[LsuCachelessBusProvider].map(_.getLsuCachelessBus())
  val pendingLsu = lsuClpb.map(bus => Array.fill[ProbeTraceLsu](bus.p.pendingMax)(null))

  class ProbeTraceLsu extends TraceIo {
    var sizel2 = 0
    var io = false
    var fromHart = false
    var threadId = 0
    var hartId = 0
    var uopId = 0
    var amoEnable = false
    var amoOp = 0
  }


  def checkPipelines(): Unit = {
    import proxies._

    if (proxies.fetch.fire.toBoolean) {
      val hart = harts(fetch.hartd.toInt)
      val fetchId = fetch.id.toInt
      hart.fetch(fetchId).spawnAt = cycle
    }

    for(decode <-decodes) {
      val spawn = decode.spawn.toBoolean
      val fire = decode.fire.toBoolean

      if (spawn || fire) {
        val hart = harts(decode.hartId.toInt)
        val decodeId = decode.decodeId.toInt
        val ctx = hart.decode(decodeId)
        if(spawn){
          val fetchId = decode.fetchId.toInt
          ctx.pc = decode.pc.toLong
          ctx.fetchId = fetchId
          ctx.spawnAt = cycle
          ctx.fireAt = -1
        }
        if(fire){
          ctx.fireAt = cycle
        }
      }
    }

    for(serialized <- wbp.serializeds) if (serialized.fire.toBoolean) {
      val hart = harts(serialized.hartId.toInt)
      val decodeId = serialized.decodeId.toInt
      val microOpId = serialized.microOpId.toInt
      val ctx = hart.microOp(microOpId)
      val decodeCtx = hart.decode(decodeId)
      ctx.clear()
      ctx.fetchId = decodeCtx.fetchId
      ctx.decodeId = decodeId
      ctx.instruction = serialized.microOp.toLong
      ctx.spawnAt = cycle
      hart.microOpAllocPtr = (microOpId+1) & microOpIdMask
    }

    for (dispatch <- dispatches) if (dispatch.fire.toBoolean) {
      val hart = harts(dispatch.hartId.toInt)
      val ctx = hart.microOp(dispatch.microOpId.toInt)
      ctx.issueAt = cycle
    }

    for (execute <- executes) if (execute.fire.toBoolean) {
      val hart = harts(execute.hartId.toInt)
      val ctx = hart.microOp(execute.microOpId.toInt)
      ctx.executeAt = cycle
    }

    if (loadExecute.fire.toBoolean) {
      val hartId = loadExecute.hartId.toInt
      val hart = harts(hartId)
      val uopId = loadExecute.uopId.toInt
      val uop = hart.microOp(uopId)
      val address = loadExecute.address.toLong
      val bytes = 1 << loadExecute.size.toInt
      uop.loadValid = true
      uop.loadData = loadExecute.data.toLong
      uop.loadLqId = uopId & 0xF
      if(!nativeCacheless) backends.foreach(_.loadExecute(hart.hartId, uop.loadLqId, address, bytes, uop.loadData))
    }

    if (storeCommit.fire.toBoolean) {
      val hartId = storeCommit.hartId.toInt
      val uopId = storeCommit.uopId.toInt
      val hart = harts(hartId)
      val uop = hart.microOp(uopId)
      val address = storeCommit.address.toLong
      val bytes = 1 << storeCommit.size.toInt
      uop.storeValid = true
      uop.storeData = storeCommit.data.toLong
      uop.storeSqId = storeCommit.storeId.toInt
      uop.lsuAddress = address
      uop.lsuLen = bytes
      uop.lsuAmo = storeCommit.amo.toBoolean
      if(!uop.lsuAmo) {
        backends.foreach(_.storeExecute(hart.hartId, uop.storeSqId, address, bytes, uop.storeData))
      }
    }

    if (storeConditional.fire.toBoolean) {
      val hartId = storeConditional.hartId.toInt
      val uopId = storeConditional.uopId.toInt
      val hart = harts(hartId)
      val uop = hart.microOp(uopId)
      val miss = storeConditional.miss.toBoolean
      if (miss) uop.storeValid = false
      uop.isSc = true
      uop.scFailure = miss
    }




    csr.foreach (csr => if (csr.valid.toBoolean) {
      val hartId = csr.hartId.toInt
      val uopId = csr.uopId.toInt
      val hart = harts(hartId)
      val uop = hart.microOp(uopId)
      uop.csrValid = true
      uop.csrAddress = csr.address.toInt
      uop.csrWriteDone = csr.writeDone.toBoolean
      uop.csrReadDone = csr.readDone.toBoolean
      uop.csrWriteData = csr.write.toLong
      uop.csrReadData = csr.read.toLong
    })

    for (port <- rfWrites) if (port.valid.toBoolean) {
      val hart = harts(port.hartId.toInt)
      val ctx = hart.microOp(port.uopId.toInt)
      port.port.rfSpec match {
        case IntRegFile => {
          ctx.integerWriteValid = true
          ctx.integerWriteData = xlenExtends(port.data.toLong)
        }
        case FloatRegFile => {
          ctx.floatWriteValid = true
          ctx.floatWriteData = port.data.toLong | floatOr
        }
      }
    }


    for(bus <- lsuClpb) {
      if(bus.cmd.valid.toBoolean && bus.cmd.ready.toBoolean){
        val trace = new ProbeTraceLsu
        trace.sizel2 = bus.cmd.size.toInt
        trace.write = bus.cmd.write.toBoolean
        trace.address = bus.cmd.address.toLong
        trace.size = 1 << trace.sizel2
        trace.io = bus.cmd.io.toBoolean
        trace.fromHart = bus.cmd.fromHart.toBoolean
        trace.threadId = bus.cmd.hartId.toInt
        if(trace.fromHart) trace.hartId = hartsIds(trace.threadId)
        trace.uopId = bus.cmd.uopId.toInt
        trace.amoEnable = bus.cmd.amoEnable != null && bus.cmd.amoEnable.toBoolean
        trace.amoOp = if(bus.cmd.amoOp != null) bus.cmd.amoOp.toInt else 0
        trace.data = bus.cmd.data.toLong
        val transactionId = bus.cmd.id.toInt
        assert(pendingLsu.get(transactionId) == null)
        pendingLsu.get(transactionId) = trace
      }

      if (bus.rsp.valid.toBoolean) {
        val transactionId = bus.rsp.id.toInt
        val trace = pendingLsu.get(transactionId)
        assert(trace != null)
        pendingLsu.get(transactionId) = null
        if(trace.fromHart && trace.io){
          if(!trace.write){
            trace.data = bus.rsp.data.toLong
          }
          val offset = trace.address.toInt & (bus.p.dataWidth/8 - 1)
          trace.data = (trace.data >> offset*8) & sizeMask(trace.sizel2)
          trace.error = bus.rsp.error.toBoolean
          backends.foreach(_.ioAccess(trace.hartId, trace))
        } else if(nativeCacheless && trace.fromHart && !bus.rsp.error.toBoolean) {
          import LsuCachelessBusAmo._
          val hart = harts(trace.threadId)
          val uop = hart.microOp(trace.uopId)
          val offset = trace.address.toInt & (bus.p.dataWidth/8 - 1)
          val loadData = (bus.rsp.data.toLong >> offset*8) & sizeMask(trace.sizel2)
          val writeData = (trace.data >> offset*8) & sizeMask(trace.sizel2)
          def loadExecute() = backends.foreach(_.loadExecute(trace.hartId, trace.uopId & 0xF, trace.address, trace.size, loadData))
          def storeExecute(data: Long) = backends.foreach(_.storeExecute(trace.hartId, uop.storeSqId, trace.address, trace.size, data))
          def storeBroadcast() = backends.foreach(_.storeBroadcast(trace.hartId, uop.storeSqId))
          if(!trace.amoEnable) {
            if(trace.write) storeBroadcast() else loadExecute()
          } else trace.amoOp match {
            case LR => loadExecute()
            case SC => if(!bus.rsp.scMiss.toBoolean) {
              storeExecute(writeData)
              storeBroadcast()
            }
            case amoOp =>
              loadExecute()
              // rsp.data carries the old memory value returned to rd; the native TestBench response has no field for the post-AMO memory value, so recompute it here with the shared pure helper.
              val finalWrite = VexiiRiscvProbe.amoWriteValue(amoOp, trace.size, writeData, loadData)
              storeExecute(finalWrite)
              storeBroadcast()
          }
        }
      }
    }

    for(learn <- learns) if(learn.valid.toBoolean){
      val hart = harts(learn.hartId.toInt)
      val ctx = hart.microOp(learn.uopId.toInt)
      val isBranch = learn.isBranch.toBoolean
      val wasWrong = learn.wasWrong.toBoolean.toInt
      val taken = learn.taken.toBoolean.toInt

      val stats = hart.jbStats.getOrElseUpdate(learn.pcOnLastSlice.toLong, new JbStats)
      stats.count += 1
      stats.failed += wasWrong
      stats.taken += taken
      if (isBranch) {
        hart.branchStats.count += 1
        hart.branchStats.failed += wasWrong
        hart.branchStats.taken += taken
      }
    }

    for(port <- completions) if(port.valid.toBoolean){
      val hart = harts(port.hartId.toInt)
      val microOpId = port.uopId.toInt
      val microOp = hart.microOp(microOpId)
      if(microOp.spawned) {
        microOp.completionAt = cycle
        microOp.retireAt = cycle + 1
        microOp.trap = port.trap.toBoolean
        microOp.commit = port.commit.toBoolean
      }
    }

    for(port <- flushes) if(port.valid.toBoolean){
      val hart = harts(port.hartId.toInt)
      if(port.withUopId){
        val self = port.self.toBoolean
        val uopId = port.uopId.toInt
        var ptr = (uopId + (!self).toInt) & microOpIdMask
        val until = hart.microOpAllocPtr
        while (ptr != until) {
          val opCtx = hart.microOp(ptr)
          if (opCtx.spawned && opCtx.flushAt == -1) opCtx.flushAt = cycle
          opCtx.commit = false
          ptr = (ptr + 1) & microOpIdMask
        }
      }
    }
    interrupts.check()
  }


  def checkCommits(): Unit = {
    val wfi = proxies.wfi.toInt
    for((hart, localHartId) <- harts.zipWithIndex) {
      if(((wfi >> localHartId) & 1) != 0){
        hart.lastCommitAt = cycle
      }
      if (checkLiveness && hart.lastCommitAt + livenessThreshold < cycle) {
        val status = if (hart.microOpAllocPtr != hart.microOpRetirePtr) f"waiting on uop 0x${hart.microOpRetirePtr}%X" else f"last uop id 0x${hart.lastUopId}%X"
        simFailure(f"Vexii hasn't committed anything for too long, $status")
      }

      def flushDueTraps(): Unit = {
        while (hart.pendingTraps.nonEmpty && hart.pendingTraps.head._3 == hart.microOpRetirePtr) {
          val (interrupt, cause, _) = hart.pendingTraps.dequeue()
          backends.foreach(_.trap(hart.hartId, interrupt, cause))
        }
      }
      flushDueTraps()
      while (hart.microOpRetirePtr != hart.microOpAllocPtr && hart.microOp(hart.microOpRetirePtr).done) {
        import hart._
        val uopId = hart.microOpRetirePtr
        val uop = hart.microOp(uopId)
        if (uop.spawned) {
          val fetch = hart.fetch(uop.fetchId)
          val decode = hart.decode(uop.decodeId)

          hart.lastUopId = uopId
          hart.konataThread.foreach(_.cycleLock = fetch.spawnAt)

          uop.toKonata(hart)
          if (uop.didCommit) {
            lastCommitAt = cycle
            hart.commits += 1
            if (uop.loadValid) {
              backends.foreach(_.loadCommit(hartId, uop.loadLqId))
            }
            if (uop.isSc) {
              backends.foreach(_.storeConditional(hartId, uop.scFailure))
            }
            if (uop.storeValid) {
              if(uop.lsuAmo && !nativeCacheless){
                backends.foreach(_.storeExecute(hart.hartId, uop.storeSqId, uop.lsuAddress, uop.lsuLen, uop.storeData))
              }
              backends.foreach(_.storeCommit(hartId, uop.storeSqId))
            }
            if (uop.integerWriteValid) {
              backends.foreach(_.writeRf(hartId, 0, 32, uop.integerWriteData))
            }
            if (uop.floatWriteValid) {
              backends.foreach(_.writeRf(hartId, 1, 32, uop.floatWriteData))
            }
            if (uop.csrValid) {
              if (uop.csrReadDone) backends.foreach(_.readRf(hartId, 4, uop.csrAddress, uop.csrReadData))
              if (uop.csrWriteDone) backends.foreach(_.writeRf(hartId, 4, uop.csrAddress, uop.csrWriteData))
            }
            if(uop.commit) backends.foreach(_.commit(hartId, decode.pc, uop.instruction))
            if (autoStoreBroadcast && !nativeCacheless && uop.storeValid) {
              backends.foreach(_.storeBroadcast(hartId, uop.storeSqId))
            }
            if(!uop.trap) commitsCallbacks.foreach(_(hartId, decode.pc))
          }
          uop.clear()
        }
        hart.microOpRetirePtr = (hart.microOpRetirePtr + 1) & microOpIdMask
        flushDueTraps()
      }
    }
  }

  def checkBroadcasts(): Unit = {
    import proxies.storeBroadcast
    if (storeBroadcast.fire.toBoolean) {
      val hartId = storeBroadcast.hartId.toInt
      val hart = harts(hartId)
      val sqId = storeBroadcast.storeId.toInt
      backends.foreach(_.storeBroadcast(hart.hartId, sqId))
    }
  }

  def checkTraps(): Unit = {
    for(trap <- proxies.trap) if(trap.fire.toBoolean){
      val hart = harts(trap.hartId)
      if (hart.microOpRetirePtr == hart.microOpAllocPtr && hart.pendingTraps.isEmpty) {
        backends.foreach(_.trap(hartsIds(trap.hartId), trap.interrupt.toBoolean, trap.cause.toInt))
      } else {
        hart.pendingTraps.enqueue((trap.interrupt.toBoolean, trap.cause.toInt, hart.microOpAllocPtr))
      }
    }
  }

  var cycle = 1l
  cpu.clockDomain.onSamplings {
    if(enabled) {
      checkPipelines()
      checkCommits()
      if(!autoStoreBroadcast) checkBroadcasts()
      checkTraps()
    }
    cycle += 1l
    if ((cycle & 0x3FFFl) == 0) flush()
  }
}
