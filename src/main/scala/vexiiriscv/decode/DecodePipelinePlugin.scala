package vexiiriscv.decode

import vexiiriscv.misc.{CtrlPipelinePlugin, PipelineService, TrapService}
import vexiiriscv.schedule.{Ages, ReschedulePlugin}
import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.pipeline
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global

import scala.collection.mutable


class DecodePipelinePlugin extends FiberPlugin with PipelineService{
  setName("decode")


  val elaborationLock = Retainer()
  def getAge(at: Int, prediction: Boolean = false): Int = Ages.DECODE + at * Ages.STAGE + (!prediction).toInt * Ages.NOT_PREDICTION

  override def getLinks(): Seq[Link] = logic.connectors
  val idToCtrl = mutable.LinkedHashMap[Int, Ctrl]()


  class Ctrl() extends Area{
    val idToLane = mutable.LinkedHashMap[Int, LaneImpl]()
    val link = CtrlLink().setCompositeName(this)

    def lane(laneId : Int) = idToLane.getOrElseUpdate(laneId, ContextSwapper.outsideCondScope(new LaneImpl(laneId).setCompositeName(this, "lane" + laneId)))

    class LaneArea(laneId : Int) extends CtrlLaneMirror(lane(laneId))
    class LaneImpl(laneId: Int) extends Area with CtrlLaneApi {
      override def ctrlLink: CtrlLink = link
      override def laneName: String = laneId.toString
      override val upIsCancel = Bool()
      override val downIsCancel = Bool()
    }
  }


  def ctrl(id: Int): Ctrl = {
    idToCtrl.getOrElseUpdate(id, new Ctrl().setCompositeName(this, "ctrls_" + id.toString))
  }

  class LaneArea(ctrlId : Int, laneId : Int) extends CtrlLaneMirror(ctrl(ctrlId).lane(laneId))

  val logic = during setup new Area{
    lazy val ts = host[TrapService]
    val retainer = retains(ts.trapLock)
    awaitBuild()

    val trapPending = ts.newTrapPending()
    retainer.release()

    elaborationLock.await()
    val idMax = idToCtrl.keys.max
    for(i <- 0 to idMax) ctrl(i) //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    val sc = for(ctrlId <- ctrls.indices.dropRight(1);
                 from = ctrls(ctrlId); to = ctrls(ctrlId+1)) yield new pipeline.StageLink(from.link.down, to.link.up){
      override def build() = {
        super.build()
        // Implement LANE_SEL register clearing
        for (laneId <- 0 until Decode.LANES){
          val l = to.lane(laneId)
          when(!l.up.isReady && l.up.isCancel){
            l.up(CtrlLaneApi.LANE_SEL) := False
          }
        }
      }
    }
    val connectors = (sc ++ ctrls.map(_.link)).toSeq
    val rp = host[ReschedulePlugin]

    val flushRange = 0 until ctrls.size
    val flushes = for(ctrlId <- flushRange) yield new Area {
      val age = getAge(ctrlId)
      val c = idToCtrl(ctrlId)

      //TODO throw whole pipeline when all lanes are dead ? probably not that usefull
      val onLanes = for (laneId <- 0 until Decode.LANES) yield new Area {
        val l = c.lane(laneId)
        if (ctrlId != 0) l.up(l.LANE_SEL).setAsReg().init(False)
        val doIt = rp.isFlushedAt(age, c.link(Global.HART_ID), laneId)
        l.downIsCancel := False
        doIt match {
          case Some(cond) =>
            l.upIsCancel := cond
            when(cond) {
              l.bypass(l.LANE_SEL) := False
            }
          case None => l.upIsCancel := False
        }
      }
    }

    for (hartId <- 0 until Global.HART_COUNT) {
      trapPending(hartId) := (for (ctrlId <- 0 to idMax; laneId <- 0 until Decode.LANES; e = ctrl(ctrlId).lane(laneId)) yield e.isValid && e(Global.HART_ID) === hartId && e(Global.TRAP)).orR
    }
  }
}