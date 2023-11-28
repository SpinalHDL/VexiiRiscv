package vexiiriscv.misc

import org.apache.commons.io.FileUtils
import rvls.spinal.TraceBackend
import vexiiriscv._
import vexiiriscv.riscv.{FloatRegFile, IntRegFile, Riscv}
import spinal.core.sim._
import spinal.core._
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.Fetch
import vexiiriscv.misc.konata.{Comment, Flush, Retire, Spawn, Stage}

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class VexiiRiscvProbe(cpu : VexiiRiscv, kb : konata.Backend, withRvls : Boolean){
  var enabled = true
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

  onSimEnd(close())

  def pcExtends(pc: Long) = if (xlen == 32) (pc << 32) >> 32 else pc
  def xlenExtends(value: Long) = if (xlen == 32) (value << 32) >> 32 else value

  class HartCtx(val hartId : Int) {
    val fetch = Array.fill(1 << fetchIdWidth)(new FetchCtx())
    val decode = Array.fill(1 << decodeIdWidth)(new DecodeCtx())
    val microOp = Array.fill(1 << microOpIdWidth)(new MicroOpCtx())

    var microOpRetirePtr, microOpAllocPtr = 0
    var lastCommitAt = 0l

    val konataThread = kb.newThread()

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
      konataThread.cycleLock = Long.MaxValue
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

    def done = retireAt != -1 || flushAt != -1
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


    def traceT2s(cycle : Long) = cycle match {
      case 0l => ""
      case v => v.toString
    }
    def toKonata(hart : HartCtx): Unit = {
      val fetch = hart.fetch(fetchId)
      val decode = hart.decode(decodeId)
      val instruction = if(withRvls) rvls.jni.Frontend.disassemble(disass, this.instruction) else "? rvls disabled ?"

      val i = new konata.Instruction()
      i += new Spawn(fetch.spawnAt, hart.hartId)
      i += new Comment(fetch.spawnAt, f"${decode.pc}%x : $instruction")
      if (fetch.spawnAt != -1) i += new Stage(fetch.spawnAt, "F")
      if (decode.spawnAt != -1) i += new Stage(decode.spawnAt, "D")
      if (issueAt != -1) i += new Stage(issueAt, "I")
      if (executeAt != -1) i += new Stage(executeAt, "E")
      if (completionAt != -1) i += new Stage(completionAt, "C")
      if (didCommit) {
        i += new Retire(retireAt)
      } else {
        i += new Flush(flushAt)
      }
      kb.insert(i)

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

      integerWriteValid = false;
      floatWriteValid = false;
      csrValid = false;
      csrWriteDone = false;
      csrReadDone = false;
      floatFlags = 0;
      loadValid = false
      storeValid = false
      isSc = false
    }

    clear()
  };




  val wbp = cpu.host[WhiteboxerPlugin].logic.get

  import wbp._

  def checkPipelines(): Unit = {
    if (fetch.fire.toBoolean) {
      val hart = harts(fetch.hartId.toInt)
      val fetchId = fetch.fetchId.toInt
      hart.fetch((fetchId + 1) & ((1 << fetchIdWidth) - 1)).spawnAt = cycle
    }

    for(decode <- decodes) if (decode.fire.toBoolean) {
      val hart = harts(decode.hartId.toInt)
      val fetchId = decode.fetchId.toInt
      val decodeId = decode.decodeId.toInt
      val ctx = hart.decode(decodeId)
      ctx.pc = pcExtends(decode.pc.toLong)
      ctx.fetchId = fetchId
      ctx.spawnAt = cycle
    }

    for(serialized <- serializeds) if (serialized.fire.toBoolean) {
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
      hart.microOpAllocPtr = microOpId
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


    if (csr.port.fire.toBoolean) {
      val hartId = csr.port.hartId.toInt
      val uopId = csr.port.uopId.toInt
      val hart = harts(hartId)
      val uop = hart.microOp(uopId)
      uop.csrValid = true
      uop.csrAddress = csr.port.address.toInt
      uop.csrWriteDone = csr.port.writeDone.toBoolean
      uop.csrReadDone = csr.port.readDone.toBoolean
      uop.csrWriteData = csr.port.write.toLong
      uop.csrReadData = csr.port.read.toLong
    }

    for (port <- rfWrites.ports) if (port.valid.toBoolean) {
      val hart = harts(port.hartId.toInt)
      val ctx = hart.microOp(port.uopId.toInt)
      port.rfSpec match {
        case IntRegFile => {
          ctx.integerWriteValid = true
          ctx.integerWriteData = xlenExtends(port.data.toLong)
        }
      }
    }

    for(port <- completions.ports) if(port.valid.toBoolean){
      val hart = harts(port.hartId.toInt)
      val microOpId = port.uopId.toInt
      val microOp = hart.microOp(microOpId)
      microOp.completionAt = cycle
      microOp.retireAt = cycle+1
    }

    for(port <- reschedules.flushes) if(port.valid.toBoolean){
      val hart = harts(port.hartId.toInt)
      if(port.withUopId){
        val uopId = port.uopId.toInt
        var ptr = uopId
        val until = (hart.microOpAllocPtr + 1) & microOpIdMask
        while(ptr != until){
          val opCtx = hart.microOp(ptr)
          if(opCtx.flushAt == -1) opCtx.flushAt = cycle
          ptr = (ptr + 1) & microOpIdMask
        }
      }
    }
  }


  def checkCommits(): Unit = {
    for(hart <- harts){
      if(hart.lastCommitAt + 100l < cycle){
        simFailure("Vexii didn't commited anything since too long")
      }

      while(hart.microOp(hart.microOpRetirePtr).done){
        import hart._
        val uop = hart.microOp(hart.microOpRetirePtr)
        val fetch = hart.fetch(uop.fetchId)
        val decode = hart.decode(uop.decodeId)

        hart.konataThread.cycleLock = fetch.spawnAt
        lastCommitAt = cycle

        uop.toKonata(hart)
        if(uop.didCommit) {
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
          if(uop.storeValid){
            backends.foreach(_.storeBroadcast(hartId, hart.microOpRetirePtr & 0xF))
          }
          commitsCallbacks.foreach(_(hartId, decode.pc))
        }
        hart.microOpRetirePtr += 1
      }
    }
  }

  var cycle = 1l
  cpu.clockDomain.onSamplings {
    if(enabled) {
      checkPipelines()
      checkCommits()
      cycle += 1l
      if ((cycle & 0x3FFFl) == 0) flush()
    }
  }
}
