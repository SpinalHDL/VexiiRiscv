package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline
import spinal.lib.misc.pipeline.Link
import vexiiriscv.Global
import vexiiriscv.misc.{CtrlPipelinePlugin, PipelineService}
import vexiiriscv.schedule.{Ages, ReschedulePlugin}

import scala.collection.mutable

class FetchPipelinePlugin extends FiberPlugin with PipelineService{
  val elaborationLock = Lock()
  def getAge(at: Int, prediction: Boolean): Int = Ages.FETCH + at * Ages.STAGE + prediction.toInt * Ages.PREDICTION

  override def getLinks(): Seq[Link] = logic.connectors
  val idToCtrl = mutable.LinkedHashMap[Int, pipeline.CtrlLink]()
  def ctrl(id : Int) = idToCtrl.getOrElseUpdate(id, pipeline.CtrlLink())

  def up = ctrl(0).up
  val logic = during build new Area{
    elaborationLock.await()
    val idMax = idToCtrl.keys.max
    for(i <- 0 to idMax) ctrl(i).unsetName() //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    val sc = for((from, to) <- (ctrls, ctrls.tail).zipped) yield new pipeline.StageLink(from.down, to.up) //.withoutCollapse()
    val connectors = (sc ++ ctrls).toSeq

    val rp = host[ReschedulePlugin]
    val flushRange = 1 until ctrls.size - 1
    val flushes = for(id <- flushRange) yield new Area {
      val age = getAge(id, true)
      val c = ctrl(id)
      val doIt = rp.isFlushedAt(age, c(Global.HART_ID))
      doIt.foreach(v => c.throwWhen(v, usingReady = false))
    }
  }
}