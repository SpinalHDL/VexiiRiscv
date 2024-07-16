package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline
import spinal.lib.misc.pipeline.{CtrlLink, CtrlLinkMirror, Link}
import vexiiriscv.Global
import vexiiriscv.misc.{CtrlPipelinePlugin, PipelineService}
import vexiiriscv.schedule.{Ages, ReschedulePlugin}

import scala.collection.mutable

class FetchPipelinePlugin extends FiberPlugin with PipelineService{
  setName("fetch")
  val elaborationLock = Retainer()
  def getAge(at: Int, prediction: Boolean = false): Int = Ages.FETCH + at * Ages.STAGE + (!prediction).toInt * Ages.NOT_PREDICTION

  override def getLinks(): Seq[Link] = logic.connectors
  val idToFetch = mutable.LinkedHashMap[Int, pipeline.CtrlLink]()
  def fetch(id : Int) = idToFetch.getOrElseUpdate(id, pipeline.CtrlLink())

  val persistenceSpec = mutable.LinkedHashSet[Int]()
  def setPersistence(id : Int) = persistenceSpec += id

  def up = fetch(0).up
  val logic = during build new Area{
    elaborationLock.await()
    val idMax = idToFetch.keys.max
    for(i <- 0 to idMax) fetch(i).unsetName() //To ensure the creation to all intermediate nodes
    val ctrls = idToFetch.toList.sortBy(_._1).map(_._2)
    val sc = (for((from, to) <- (ctrls, ctrls.tail).zipped) yield new pipeline.StageLink(from.down, to.up).withoutCollapse()).toSeq
    val connectors = (sc ++ ctrls).toSeq
    for(e <- persistenceSpec) sc(e-1).withoutCollapse().withPayloadHold()

    val rp = host[ReschedulePlugin]
    val flushRange = 1 until ctrls.size
    val flushes = for(id <- flushRange) yield new Area {
      val age = getAge(id)
      val c = fetch(id)
      val doIt = rp.isFlushedAt(age, c(Global.HART_ID), U(0))
      doIt.foreach(v =>
        if(flushRange.last != id) {
          c.throwWhen(v, usingReady = false)
        } else  {
          c.forgetOneWhen(v)
          c.requests.cancels += v
        }
      )
    }
  }

  class Fetch(id : Int) extends CtrlLinkMirror(fetch(id))
}