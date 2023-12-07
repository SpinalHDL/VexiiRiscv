package vexiiriscv.test

import rvls.spinal.{TraceBackend, TraceIo}
import spinal.core._
import spinal.core.sim._
import vexiiriscv._
import vexiiriscv.decode.Decode
import vexiiriscv.execute.LsuCachelessPlugin
//import vexiiriscv.execute.LsuCachelessPlugin
import vexiiriscv.fetch.Fetch
import vexiiriscv.riscv.{IntRegFile, Riscv}
import vexiiriscv.test.konata.{Comment, Flush, Retire, Spawn, Stage}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class VexiiRiscvProbe(cpu : VexiiRiscv, kb : Option[konata.Backend], withRvls : Boolean){
  var enabled = true
  var trace = true
  var backends = ArrayBuffer[TraceBackend]()
  val commitsCallbacks = ArrayBuffer[(Int, Long) => Unit]()

  val hartsIds = List(0)

  val xlen = cpu.database(Riscv.XLEN)
  val hartsCount = cpu.database(Global.HART_COUNT)
  val fetchIdWidth = cpu.database(Fetch.ID_WIDTH)
  val decodeIdWidth = cpu.database(Decode.DOP_ID_WIDTH)
  val microOpIdWidth = cpu.database(Decode.UOP_ID_WIDTH)
  val microOpIdMask = (1 << microOpIdWidth)-1

  val disass = withRvls generate rvls.jni.Frontend.newDisassemble(xlen)
  val harts = hartsIds.map(new HartCtx(_)).toArray
  val wbp = cpu.host[WhiteboxerPlugin].logic.get
  val proxies = new wbp.Proxies()

  def add(tracer: TraceBackend): this.type = {
    backends += tracer
    harts.foreach(_.add(tracer))
    this
  }

  def flush(): Unit = {

  }

  def close(): Unit = {
    harts.foreach(_.close())
    if(withRvls) rvls.jni.Frontend.deleteDisassemble(disass)
  }

  def getStats(): String = {
    val str = new StringBuilder()
    str ++= "### Stats ###\n"
    for (hart <- harts) {
      val stats = hart.jbStats.toArray.sortBy(_._2.failed)
      val total = new JbStats()
      for ((pc, data) <- stats) {
        total.count += data.count
        total.failed += data.failed
      }
//      for ((pc, data) <- stats) {
//        str ++= f"- 0x${pc}%08X : ${data.toString()}\n"
//      }
      str ++= f"kind :  miss / times   miss-rate\n"
      str ++= f"J/B  : ${total.toString()}\n"
      str ++= f"  B  : ${hart.branchStats.toString()}\n"
    }

    def cycleRatio(times: Long) = {
      val rate = (1000f * times / cycle).toInt
      f"${times}%7d / ${cycle}%7d ${rate / 10}%3d.${rate % 10}%%"
    }

//    for ((hw, i) <- wbp.perf.dispatchFeedCounters.zipWithIndex) {
//      str ++= f"Dispatch  $i   : ${cycleRatio(hw.toLong)}\n"
//    }
//    for ((hw, i) <- wbp.perf.candidatesCountCounters.zipWithIndex) {
//      str ++= f"Candidate $i   : ${cycleRatio(hw.toLong)}\n"
//    }
//    str ++= f"Dispatch halt : ${cycleRatio(wbp.perf.dispatchHazardsCounter.toLong)}\n"
//    str ++= f"Execute  halt : ${cycleRatio(wbp.perf.executeFreezedCounter.toLong)}\n"
    str ++= f"IPC           : ${cycleRatio(harts.map(_.commits).sum)}\n"
    str.toString()
  }

  onSimEnd(close())

  def pcExtends(pc: Long) = if (xlen == 32) (pc << 32) >> 32 else pc
  def xlenExtends(value: Long) = if (xlen == 32) (value << 32) >> 32 else value

  class JbStats(){
    var count = 0l
    var failed = 0l

    override def toString(): String ={
      val rate = (1000f*failed/count).toInt
      f"${failed}%5d / ${count}%5d ${rate/10}%3d.${rate%10}%%"
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


    val jbStats = mutable.HashMap[Long, JbStats]()
    val branchStats = new JbStats()

    val konataThread = kb.map(_.newThread())

    def add(tracer: TraceBackend): Unit = {
      for (hartId <- hartsIds) {
        tracer.newCpuMemoryView(hartId, 16, 16) //TODO readIds writeIds
        tracer.newCpu(hartId, s"RV${xlen}IMA", "MSU", 32, hartId)
        val pc = pcExtends(0x80000000l)
        tracer.setPc(hartId, pc)
        this
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
    def didCommit = done && flushAt == -1

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
    var storeValid = false
    var storeData = -1l
    var storeSqId = -1
    var loadValid = false
    var loadLqId = 0
    var loadData = -1l

    var isSc = false
    var scFailure = false

    var isBranch = false
    var isJumpBranch = false
    var predictionWasWrong = false


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
        i += new Stage(fetch.spawnAt+1, "F")
      }
      if (decode.spawnAt != -1) {
        i += new Stage(decode.spawnAt, "D")
        i += new Comment(decode.spawnAt, f"${decode.pc}%X : $instruction")
      }
      if (issueAt != -1) i += new Stage(issueAt, "I")
      if (executeAt != -1) i += new Stage(executeAt, "E")
//      if (completionAt != -1) i += new Stage(completionAt, "C")
      if (didCommit) {
        i += new Retire(retireAt)
      } else {
        i += new Flush(flushAt)
      }
      kb.foreach(_.insert(i))

//        f.write(f"O3PipeView:fetch:${fetch.spawnAt}:0x${decode.pc}%08x:0:${opCounter}:$instruction\n")
//        f.write(f"O3PipeView:decode:${traceT2s(decode.spawnAt)}\n")
//        f.write(f"O3PipeView:dispatch:${traceT2s(spawnAt)}\n")
//        f.write(f"O3PipeView:issue:${traceT2s(executeAt)}\n")
//        f.write(f"O3PipeView:complete:${traceT2s(completionAt)}\n")
//        f.write(f"O3PipeView:retire:${traceT2s(completionAt+1)}:store:\n")
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

      integerWriteValid = false;
      floatWriteValid = false;
      csrValid = false;
      csrWriteDone = false;
      csrReadDone = false;
      floatFlags = 0;
      loadValid = false
      storeValid = false
      isSc = false
      isJumpBranch = false
      predictionWasWrong = false
    }

    clear()
  };


  val sizeMask = Array(0xFFl, 0xFFFFl, 0xFFFFFFFFl, -1l)

  val lsuClpb = cpu.host.get[LsuCachelessPlugin].map(_.logic.get.bus)
  val pendingIo = mutable.Queue[ProbeTraceIo]()

  class ProbeTraceIo extends TraceIo {
    var sizel2 = 0
    var io = false
    var hartId = 0
  }


  def checkPipelines(): Unit = {
    import proxies._

    if (proxies.fetch.fire.toBoolean) {
      val hart = harts(fetch.hartd.toInt)
      val fetchId = fetch.id.toInt
      hart.fetch(fetchId).spawnAt = cycle - 1
    }

    for(decode <-decodes) if (decode.fire.toBoolean) {
      val hart = harts(decode.hartId.toInt)
      val fetchId = decode.fetchId.toInt
      val decodeId = decode.decodeId.toInt
      val ctx = hart.decode(decodeId)
      ctx.pc = pcExtends(decode.pc.toLong)
      ctx.fetchId = fetchId
      ctx.spawnAt = cycle
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
      backends.foreach(_.loadExecute(hartId, uop.loadLqId, address, bytes, uop.loadData))
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
      uop.storeSqId = uopId & 0xF
      uop.lsuAddress = address
      uop.lsuLen = bytes
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
      }
    }

//
    for(bus <- lsuClpb) {
      if(bus.cmd.valid.toBoolean && bus.cmd.ready.toBoolean){
        val trace = new ProbeTraceIo
        trace.sizel2 = bus.cmd.size.toInt
        trace.write = bus.cmd.write.toBoolean
        trace.address = bus.cmd.address.toLong
        trace.size = 1 << trace.sizel2
        trace.io = bus.cmd.io.toBoolean
        trace.hartId = bus.cmd.hartId.toInt
        val offset = trace.address.toInt & (trace.size - 1)
        trace.data = bus.cmd.data.toLong
        pendingIo += trace
      }

      if (bus.rsp.valid.toBoolean) {
        val trace = pendingIo.dequeue()
        if(trace.io){
          if(!trace.write){
            trace.data = bus.rsp.data.toLong
          }
          val offset = trace.address.toInt & (trace.size - 1)
          trace.data = (trace.data >> offset*8) & sizeMask(trace.sizel2)
          backends.foreach(_.ioAccess(trace.hartId, trace))
        }
      }
    }

    if(learn.valid.toBoolean){
      val hart = harts(learn.hartId.toInt)
      val ctx = hart.microOp(learn.uopId.toInt)
      ctx.isJumpBranch = true
      ctx.isBranch = learn.isBranch.toBoolean
      ctx.predictionWasWrong = learn.wasWrong.toBoolean
    }

    for(port <- completions) if(port.valid.toBoolean){
      val hart = harts(port.hartId.toInt)
      val microOpId = port.uopId.toInt
      val microOp = hart.microOp(microOpId)
      if(microOp.spawned) {
        microOp.completionAt = cycle
        microOp.retireAt = cycle + 1
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
          ptr = (ptr + 1) & microOpIdMask
        }
      }
    }
  }


  def checkCommits(): Unit = {
    for(hart <- harts) {
      if (hart.lastCommitAt + 100l < cycle) {
        val status = if (hart.microOpAllocPtr != hart.microOpRetirePtr) f"waiting on uop 0x${hart.microOpRetirePtr}%X" else f"last uop id 0x${hart.lastUopId}%X"
        simFailure(f"Vexii didn't commited anything since too long, $status")
      }

      while (hart.microOpRetirePtr != hart.microOpAllocPtr && hart.microOp(hart.microOpRetirePtr).done) {
        import hart._
        val uopId = hart.microOpRetirePtr
        val uop = hart.microOp(uopId)
        if (uop.spawned) {
          val fetch = hart.fetch(uop.fetchId)
          val decode = hart.decode(uop.decodeId)

          hart.lastUopId = uopId

          hart.konataThread.foreach(_.cycleLock = fetch.spawnAt)
          lastCommitAt = cycle

          uop.toKonata(hart)
          if (uop.didCommit) {
            hart.commits += 1
            if(uop.isJumpBranch){
              val stats = jbStats.getOrElseUpdate(decode.pc, new JbStats)
              stats.count += 1
              stats.failed += uop.predictionWasWrong.toInt
              if(uop.isBranch){
                branchStats.count += 1
                branchStats.failed += uop.predictionWasWrong.toInt
              }
            }
            if (uop.loadValid) {
              backends.foreach(_.loadCommit(hartId, uop.loadLqId))
            }
            if (uop.isSc) {
              backends.foreach(_.storeConditional(hartId, uop.scFailure))
            }
            if (uop.storeValid) {
              backends.foreach(_.storeCommit(hartId, uop.storeSqId, uop.lsuAddress, uop.lsuLen, uop.storeData))
            }
            if (uop.integerWriteValid) {
              backends.foreach(_.writeRf(hartId, 0, 32, uop.integerWriteData))
            }
            if (uop.csrValid) {
              if (uop.csrReadDone) backends.foreach(_.readRf(hartId, 4, uop.csrAddress, uop.csrReadData))
              if (uop.csrWriteDone) backends.foreach(_.writeRf(hartId, 4, uop.csrAddress, uop.csrWriteData))
            }
            backends.foreach(_.commit(hartId, decode.pc))
            if (uop.storeValid) {
              backends.foreach(_.storeBroadcast(hartId, uopId & 0xF))
            }
            commitsCallbacks.foreach(_(hartId, decode.pc))
          }
          uop.clear()
        }
        hart.microOpRetirePtr = (hart.microOpRetirePtr + 1) & microOpIdMask
      }
    }
  }

  var cycle = 1l
  cpu.clockDomain.onSamplings {
    if(enabled) {
      checkPipelines()
      checkCommits()
    }
    cycle += 1l
    if ((cycle & 0x3FFFl) == 0) flush()
  }
}
