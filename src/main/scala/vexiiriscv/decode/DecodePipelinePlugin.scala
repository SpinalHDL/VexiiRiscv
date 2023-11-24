package vexiiriscv.decode

import vexiiriscv.misc.{CtrlPipelinePlugin, PipelineService}
import vexiiriscv.schedule.{Ages, ReschedulePlugin}
import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib.misc.pipeline
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global

import scala.collection.mutable

object DecodePipelinePlugin{
  val LANE_SEL = Payload(Bool())
}

class DecodePipelinePlugin extends FiberPlugin with PipelineService{
  setName("decode")
  val elaborationLock = Lock()
  def getAge(at: Int, prediction: Boolean): Int = Ages.DECODE + at * Ages.STAGE + prediction.toInt * Ages.PREDICTION

  override def getLinks(): Seq[Link] = logic.connectors
  val idToRawCtrl = mutable.LinkedHashMap[Int, Ctrl]()


  class Ctrl() extends Area{
    val laneIdToCtrlLane = mutable.LinkedHashMap[Int, LaneImpl]()
    val ctrl = CtrlLink().setCompositeName(this)

    def lane(laneId : Int) = laneIdToCtrlLane.getOrElseUpdate(laneId, new LaneImpl(laneId).setCompositeName(this, "lane" + laneId))

    class LaneArea(laneId : Int) extends CtrlLaneMirror(lane(laneId))
    class LaneImpl(laneId: Int) extends Area with CtrlLaneApi {
      val cancel = Bool()
      override def ctrlLink: CtrlLink = ctrl
      override def laneName: String = laneId.toString
      override def LANE_SEL: Payload[Bool] = DecodePipelinePlugin.LANE_SEL
      override def hasCancelRequest = cancel
    }
  }


  def rawCtrl(id: Int): Ctrl = {
    idToRawCtrl.getOrElseUpdate(id, new Ctrl().setCompositeName(this, "ctrls_" + id.toString))
  }


  val logic = during build new Area{
    elaborationLock.await()
    val idMax = idToRawCtrl.keys.max
    for(i <- 0 to idMax) rawCtrl(i).unsetName() //To ensure the creation to all intermediate nodes
    val ctrls = idToRawCtrl.toList.sortBy(_._1).map(_._2)
    val sc = for((from, to) <- (ctrls, ctrls.tail).zipped) yield new pipeline.StageLink(from.ctrl.down, to.ctrl.up) //.withoutCollapse()
    val connectors = (sc ++ ctrls.map(_.ctrl)).toSeq

    val rp = host[ReschedulePlugin]
    val flushRange = 0 until ctrls.size
    val flushes = for(id <- flushRange) yield new Area {
      val age = getAge(id, true)
      val c = idToRawCtrl(id)
      val doIt = rp.isFlushedAt(age, c.ctrl(Global.HART_ID))
      doIt.foreach(v => c.ctrl.throwWhen(v, usingReady = false))
    }
  }
}