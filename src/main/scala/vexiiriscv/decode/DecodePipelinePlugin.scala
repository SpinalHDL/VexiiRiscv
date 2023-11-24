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
  val idToCtrl = mutable.LinkedHashMap[Int, Ctrl]()


  class Ctrl() extends Area{
    val idToLane = mutable.LinkedHashMap[Int, LaneImpl]()
    val link = CtrlLink().setCompositeName(this)

    def lane(laneId : Int) = idToLane.getOrElseUpdate(laneId, new LaneImpl(laneId).setCompositeName(this, "lane" + laneId))

    class LaneArea(laneId : Int) extends CtrlLaneMirror(lane(laneId))
    class LaneImpl(laneId: Int) extends Area with CtrlLaneApi {
      val cancel = Bool()
      override def ctrlLink: CtrlLink = link
      override def laneName: String = laneId.toString
      override def LANE_SEL: Payload[Bool] = DecodePipelinePlugin.LANE_SEL
      override def hasCancelRequest = cancel
    }
  }


  def ctrl(id: Int): Ctrl = {
    idToCtrl.getOrElseUpdate(id, new Ctrl().setCompositeName(this, "ctrls_" + id.toString))
  }


  val logic = during build new Area{
    elaborationLock.await()
    val idMax = idToCtrl.keys.max
    for(i <- 0 to idMax) ctrl(i).unsetName() //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    val sc = for((from, to) <- (ctrls, ctrls.tail).zipped) yield new pipeline.StageLink(from.link.down, to.link.up) //.withoutCollapse()
    val connectors = (sc ++ ctrls.map(_.link)).toSeq

    val rp = host[ReschedulePlugin]
    val flushRange = 0 until ctrls.size
    val flushes = for(id <- flushRange) yield new Area {
      val age = getAge(id, true)
      val c = idToCtrl(id)
      val doIt = rp.isFlushedAt(age, c.link(Global.HART_ID))
      doIt.foreach(v => c.link.throwWhen(v, usingReady = false))
    }
  }
}