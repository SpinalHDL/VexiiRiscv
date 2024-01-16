package vexiiriscv.misc

import spinal.core._

import spinal.core.Area
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline
import spinal.lib.misc.pipeline.Link
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.schedule.ReschedulePlugin

import scala.collection.mutable


trait PipelineService{
  def getLinks() : Seq[Link]
}

abstract class CtrlPipelinePlugin extends FiberPlugin with PipelineService{
  val elaborationLock = Retainer()
  override def getLinks(): Seq[Link] = logic.connectors
  val idToCtrl = mutable.LinkedHashMap[Int, pipeline.CtrlLink]()
  def ctrl(id : Int) = idToCtrl.getOrElseUpdate(id, pipeline.CtrlLink())

  def getAge(at: Int, prediction: Boolean): Int
  def up = ctrl(0).up
  val logic = during build new Area{
    elaborationLock.await()
    val idMax = idToCtrl.keys.max
    for(i <- 0 to idMax) ctrl(i).unsetName() //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    val sc = for((from, to) <- (ctrls, ctrls.tail).zipped) yield new pipeline.StageLink(from.down, to.up) //.withoutCollapse()
    val connectors = (sc ++ ctrls).toSeq

//    val rp = host[ReschedulePlugin]
//    val flushRange = 1 until ctrls.size
//    val flushes = for(id <- flushRange) yield new Area {
//      val age = getAge(id, true)
//      val c = ctrl(id)
//      val doIt = rp.isFlushedAt(age, c(Global.HART_ID))
//      doIt.foreach(v => c.throwWhen(v, usingReady = false))
//    }
  }
}