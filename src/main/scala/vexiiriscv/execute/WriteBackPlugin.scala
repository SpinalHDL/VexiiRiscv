package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib.misc.pipeline._
import spinal.lib.{Flow, OHMux}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{MicroOp, RD, RegfileSpec}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class WriteBackPlugin(val euId : String,
                      val rf : RegfileSpec,
                      var writeAt : Int,
                      var bypassOn: (Int) => Boolean = (ctrlId: Int) => true) extends FiberPlugin with CompletionService{
  withPrefix(euId + "_" + rf.getName())

  val elaborationLock = Lock()

  lazy val eu = host.find[ExecuteUnitPlugin](_.euId == euId)
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


  override def getCompletions(): Seq[Flow[CompletionPayload]] = List(logic.write.completion)

  val logic = during build new Area {
    elaborationLock.await()

    val specs = portToSpec.values
    val grouped = specs.groupByLinked(_.ctrlAt).values
    val sorted = grouped.toList.sortBy(_.head.ctrlAt)

    val writeCtrl = eu.execute(writeAt)

    val DATA = Payload(Bits(rf.width bits))
    val stages = for (group <- sorted) yield new Area {
      val ctrlId = group.head.ctrlAt
      val ctrl = eu.execute(ctrlId)
      val hits = B(group.map(_.port.valid))
      val muxed = OHMux.or(hits, group.map(_.port.payload), group == sorted.head && group.size == 1)
      val merged = if(group == sorted.head) muxed else ctrl.up(DATA) | muxed
      ctrl.bypass(DATA) := merged
      for (spec <- group) {
        for(op <- spec.microOps) {
          eu.setRdSpec(op, DATA, writeAt + rfp.writeLatency, (ctrlId to writeAt + rfp.writeLatency-1 + rfp.readLatency).filter(bypassOn))
        }
      }
    }

    eu.uopLock.release()

    val write = new writeCtrl.Area{
      val rfa = Decode.rfaKeys.get(RD)
      val port = rfp.newWrite(false)
      port.valid := isValid && rfa.ENABLE
      port.address := Global.HART_ID @@ rfa.PHYS
      port.data := DATA
      port.hartId := Global.HART_ID
      port.uopId := Decode.UOP_ID

      val completion = Flow(CompletionPayload())
      completion.valid := port.fire
      completion.hartId := Global.HART_ID
      completion.microOpId := Decode.UOP_ID
    }
  }
}

