package vexiiriscv.misc

import org.apache.commons.io.FileUtils
import rvls.spinal.TraceBackend
import vexiiriscv._
import vexiiriscv.riscv.Riscv
import spinal.core.sim._
import spinal.core._
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.Fetch

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class VexiiRiscvProbe(cpu : VexiiRiscv, gem5File : Option[File], withRvls : Boolean){
  var backends = ArrayBuffer[TraceBackend]()
  val commitsCallbacks = ArrayBuffer[(Int, Long) => Unit]()
  val gem5 = gem5File.map{f =>
    FileUtils.forceMkdir(f.getParentFile)
    new BufferedWriter(new FileWriter(f))
  }
  val hartsIds = List(0)

  var gem5Enabled = gem5File.nonEmpty

  val xlen = cpu.database(Riscv.XLEN)
  val hartsCount = cpu.database(Global.HART_COUNT)
  val fetchIdWidth = cpu.database(Fetch.ID_WIDTH)
  val decodeIdWidth = cpu.database(Decode.DOP_ID_WIDTH)
  val microOpIdWidth = cpu.database(Decode.UOP_ID_WIDTH)

  val disass = withRvls generate rvls.jni.Frontend.newDisassemble(xlen)
  val harts = hartsIds.map(new HartCtx(_)).toArray

  def add(tracer: TraceBackend): this.type = {
    backends += tracer
    harts.foreach(_.add(tracer))
    this
  }

  def flush(): Unit = {
    gem5.foreach(_.flush())
  }

  def close(): Unit = {
    gem5.foreach(_.close())
    if(withRvls) rvls.jni.Frontend.deleteDisassemble(disass)
  }

  onSimEnd(close())

  def pcExtends(pc: Long) = if (xlen == 32) (pc << 32) >> 32 else pc
  def xlenExtends(value: Long) = if (xlen == 32) (value << 32) >> 32 else value

  class HartCtx(val hartId : Int) {
    val fetch = Array.fill(1 << fetchIdWidth)(new FetchCtx())
    val decode = Array.fill(1 << decodeIdWidth)(new DecodeCtx())
    val microOp = Array.fill(1 << microOpIdWidth)(new MicroOpCtx())

    var microOpPtr = 0
    var lastCommitAt = 0l

    //    val microOpIdQueue = mutable.Queue[Int]()


    def add(tracer: TraceBackend): Unit = {
      for (hartId <- hartsIds) {
        tracer.newCpuMemoryView(hartId, 1, 1) //TODO readIds writeIds
        tracer.newCpu(hartId, s"RV${xlen}IMA", "MSU", 32, hartId)
        val pc = pcExtends(0x80000000l)
        tracer.setPc(hartId, pc)
        this
      }
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

  var opCounter = 0

  class MicroOpCtx() {
    var fetchId = -1
    var decodeId = -1
    var spawnAt = 0l
    var dispatchAt = 0l
    var executeAt = 0l
    var completionAt = 0l
    var instruction = 0l

    def done = completionAt != 0

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
    def writeGem5(hart : HartCtx): Unit = {
      if(gem5Enabled){
        gem5.foreach{ f =>
          val fetch = hart.fetch(fetchId)
          val decode = hart.decode(decodeId)
          val instruction = if(withRvls) rvls.jni.Frontend.disassemble(disass, this.instruction) else "? rvls disabled ?"
          f.write(f"O3PipeView:fetch:${fetch.spawnAt}:0x${decode.pc}%08x:0:${opCounter}:$instruction\n")
          f.write(f"O3PipeView:decode:${traceT2s(decode.spawnAt)}\n")
          f.write(f"O3PipeView:dispatch:${traceT2s(spawnAt)}\n")
          f.write(f"O3PipeView:issue:${traceT2s(executeAt)}\n")
          f.write(f"O3PipeView:complete:${traceT2s(completionAt)}\n")
          f.write(f"O3PipeView:retire:${traceT2s(completionAt+1)}:store:\n")
          //          gem5 << "O3PipeView:fetch:" << traceT2s(fetch.fetchAt) << ":0x" << hex << setw(8) << std :: setfill('0') << op.pc << dec << ":0:" << op.counter << ":" << assembly << endl;
          //          gem5 << "O3PipeView:decode:" << traceT2s(fetch.decodeAt) << endl;
          //          gem5 << "O3PipeView:rename:" << traceT2s(op.renameAt) << endl;
          //          gem5 << "O3PipeView:dispatch:" << traceT2s(op.dispatchAt) << endl;
          //          gem5 << "O3PipeView:issue:" << traceT2s(op.issueAt) << endl;
          //          gem5 << "O3PipeView:complete:" << traceT2s(op.completeAt) << endl;
          //          gem5 << "O3PipeView:retire:" << traceT2s(op.commitAt) << ":store:" << traceT2s(op.sqAllocated ? op.storeAt: 0) << endl;
        }
      }
      opCounter += 1
    }

    def clear() {
      fetchId = -1
      decodeId = -1
      spawnAt = 0l
      dispatchAt = 0l
      executeAt = 0l
      completionAt = 0l

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
      val ctx = hart.microOp(decodeId)
      val decodeCtx = hart.decode(decodeId)
      ctx.clear()
      ctx.fetchId = decodeCtx.fetchId
      ctx.decodeId = decodeId
      ctx.instruction = serialized.microOp.toLong
      ctx.spawnAt = cycle
    }

    for (dispatch <- dispatches) if (dispatch.fire.toBoolean) {
      val hart = harts(dispatch.hartId.toInt)
      val ctx = hart.microOp(dispatch.microOpId.toInt)
      ctx.dispatchAt = cycle
    }

    for (execute <- executes) if (execute.fire.toBoolean) {
      val hart = harts(execute.hartId.toInt)
      val ctx = hart.microOp(execute.microOpId.toInt)
      ctx.executeAt = cycle
    }

    for (intWrite <- rfWrites.ints) if (intWrite.valid.toBoolean) {
      val hart = harts(intWrite.hartId.toInt)
      val ctx = hart.microOp(intWrite.uopId.toInt)
      assert(!ctx.integerWriteValid)
      ctx.integerWriteValid = true
      ctx.integerWriteData = xlenExtends(intWrite.data.toLong)
    }

    for(port <- completions.ports) if(port.valid.toBoolean){
      val hart = harts(port.hartId.toInt)
      val microOpId = port.microOpId.toInt
      val microOp = hart.microOp(microOpId)
      microOp.completionAt = cycle
      microOp.writeGem5(hart)
    }
  }


  def checkCommits(): Unit = {
    for(hart <- harts){
      if(hart.lastCommitAt + 100l < cycle){
        simFailure("Vexii didn't commited anything since too long")
      }

      while(hart.microOp(hart.microOpPtr).done){
        import hart._
        val uop = hart.microOp(hart.microOpPtr)
        val decode = hart.decode(uop.decodeId)
        lastCommitAt = cycle
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
        commitsCallbacks.foreach(_(hartId, decode.pc))
        hart.microOpPtr += 1
      }
    }
  }

  var cycle = 1l
  cpu.clockDomain.onSamplings {
    checkPipelines()
    checkCommits()
    cycle += 1l
    if((cycle & 0x3FFFl) == 0) flush()
  }
}
