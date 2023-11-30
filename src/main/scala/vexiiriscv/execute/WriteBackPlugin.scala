package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib.misc.pipeline._
import spinal.lib.{Flow, OHMux}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.decode.Decode._
import vexiiriscv.regfile.{RegFileWriter, RegFileWriterService, RegfileService}
import vexiiriscv.riscv.{MicroOp, RD, RegfileSpec}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class WriteBackPlugin(val laneName : String,
                      val rf : RegfileSpec,
                      var writeAt : Int,
                      var writeBackKey : Any = null,
                      var bypassOn: (Int) => Boolean = (ctrlId: Int) => true) extends FiberPlugin with RegFileWriterService{
  withPrefix(laneName + "_" + rf.getName())

  val elaborationLock = Lock()

  lazy val eu = host.find[ExecuteLanePlugin](_.laneName == laneName)
  lazy val rfp = host.find[RegfileService](_.rfSpec == rf)

  setupRetain(eu.uopLock)
  buildBefore(eu.pipelineLock)
  buildBefore(rfp.elaborationLock)

  case class Spec(port : Flow[Bits], ctrlAt : Int){
    val microOps = ArrayBuffer[MicroOp]()
  }
  val portToSpec = mutable.LinkedHashMap[Flow[Bits],Spec]()
  def createPort(at : Int): Flow[Bits] = {
    val port = Flow(Bits(rf.width bits))
    portToSpec(port) = Spec(port, at)
    port
  }

  def addMicroOp(port: Flow[Bits], microOp: Seq[MicroOp]): Unit = {
    val spec = portToSpec(port)
    spec.microOps ++= microOp
  }

  def addMicroOp(port: Flow[Bits], head: MicroOp, tail : MicroOp*): Unit = {
    addMicroOp(port, head +: tail)
  }

  val SEL = Payload(Bool())

  val logic = during build new Area {
    elaborationLock.await()

    val specs = portToSpec.values
    val grouped = specs.groupByLinked(_.ctrlAt).values
    val sorted = grouped.toList.sortBy(_.head.ctrlAt)
    val DATA = Payload(Bits(rf.width bits))
    eu.setDecodingDefault(SEL, False)
    for (group <- sorted) {
      val ctrlId = group.head.ctrlAt
      for (spec <- group) {
        for (op <- spec.microOps) {
          eu.setRdSpec(op, DATA, writeAt + rfp.writeLatency, (ctrlId to writeAt + rfp.writeLatency - 1 + rfp.readLatency).filter(bypassOn))
          eu.addDecoding(op, SEL -> True)
        }
      }
    }
    eu.uopLock.release()

    val rfa = rfaKeys.get(RD)
    val stages = for (group <- sorted; ctrlId = group.head.ctrlAt) yield new eu.Execute(ctrlId) {
      val hits = B(group.map(_.port.valid))
      val muxed = OHMux.or(hits, group.map(_.port.payload), group == sorted.head && group.size == 1)
      val merged = if(group == sorted.head) muxed else up(DATA) | muxed
      bypass(DATA) := merged

      val write = Flow(RegFileWriter(rf))
      write.valid := down.isFiring && hits.orR && rfa.ENABLE
      write.hartId := HART_ID
      write.uopId := UOP_ID
      write.data := muxed
    }


    val writeCtrl = eu.execute(writeAt)
    val write = new writeCtrl.Area{
      val port = rfp.newWrite(false, sharingKey = writeBackKey)
      port.valid := isValid && rfa.ENABLE && SEL
      port.address := HART_ID @@ rfa.PHYS
      port.data := DATA
      port.hartId := HART_ID
      port.uopId := UOP_ID
    }
  }

  override def getRegFileWriters(): Seq[Flow[RegFileWriter]] = logic.stages.map(_.write)
}

