package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline._
import spinal.lib.{Flow, OHMux}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.Global._
import vexiiriscv.decode.Decode._
import vexiiriscv.regfile.{RegFileWriter, RegFileWriterService, RegfileService}
import vexiiriscv.riscv.{MicroOp, RD, RegfileSpec}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class WriteBackPlugin(val laneName : String,
                      val rf : RegfileSpec,
                      var writeAt : Int,
                      var allowBypassFrom : Int) extends FiberPlugin with RegFileWriterService{
  withPrefix(laneName + "_" + rf.getName())

  val elaborationLock = Retainer()

  case class Spec(port : Flow[Bits], ctrlAt : Int){
    val impls = ArrayBuffer[UopLayerSpec]()
  }
  val portToSpec = mutable.LinkedHashMap[Flow[Bits],Spec]()
  def createPort(at : Int): Flow[Bits] = {
    val port = Flow(Bits(rf.width bits))
    portToSpec(port) = Spec(port, at)
    assert(at <= writeAt, "WriteBackPlugin.createPort target is behond writeback range")
    port
  }
  def addMicroOp(port: Flow[Bits], layer : LaneLayer, uop: Seq[MicroOp]): Unit = addMicroOp(port, uop.map(layer.apply))
  def addMicroOp(port: Flow[Bits], head: UopLayerSpec, tail: UopLayerSpec*): Unit = addMicroOp(port, head +: tail)
  def addMicroOp(port: Flow[Bits], impls: Seq[UopLayerSpec]): Unit = {
    val spec = portToSpec(port)
    spec.impls ++= impls
  }


  val SEL = Payload(Bool())

  val logic = during setup new Area {
    val eu = host.find[ExecuteLanePlugin](_.laneName == laneName)
    val rfp = host.find[RegfileService](_.rfSpec == rf)
    val uopRetainer = retains(eu.uopLock)
    val buildBefore = retains(eu.pipelineLock, rfp.elaborationLock)
    awaitBuild()

    elaborationLock.await()
    val specs = portToSpec.values
    val grouped = specs.groupByLinked(_.ctrlAt).values
    val sorted = grouped.toList.sortBy(_.head.ctrlAt)
    val DATA = Payload(Bits(rf.width bits))
    val broadcastMin = Math.min(writeAt + rfp.writeLatency, allowBypassFrom)
    eu.setDecodingDefault(SEL, False)
    for (group <- sorted) {
      val ctrlId = group.head.ctrlAt
      for (spec <- group) {
        for (impl <- spec.impls) {
          impl.setRdSpec(DATA, Math.max(ctrlId, broadcastMin), writeAt + rfp.writeLatency)
          impl.addDecoding(SEL -> True)
//          impl.dontFlushFrom(writeAt+1)
        }
      }
    }

    uopRetainer.release()

    val rfa = rfaKeys.get(RD)
    val stages = for (group <- sorted; ctrlId = group.head.ctrlAt) yield new eu.Execute(ctrlId) {
      val hits = B(group.map(_.port.valid))
      val muxed = OHMux.or(hits, group.map(_.port.payload), group == sorted.head && group.size == 1)
      val merged = if(group == sorted.head) muxed else up(DATA) | muxed
      bypass(DATA) := merged

      val write = Flow(RegFileWriter(rf))
      write.valid := down.isFiring && hits.orR && rfa.ENABLE && Global.COMMIT
      write.hartId := HART_ID
      write.uopId := UOP_ID
      write.data := muxed
    }


    val writeCtrl = eu.execute(writeAt)
    val write = new writeCtrl.Area{
      val port = rfp.newWrite(false, sharingKey = laneName)
      port.valid := isValid && isReady && !isCancel && rfa.ENABLE && SEL && Global.COMMIT
      port.address := HART_ID @@ rfa.PHYS
      port.data := DATA
      port.hartId := HART_ID
      port.uopId := UOP_ID
    }
    buildBefore.release()
  }

  override def getRegFileWriters(): Seq[Flow[RegFileWriter]] = logic.stages.map(_.write)
}

