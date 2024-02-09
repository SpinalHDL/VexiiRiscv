package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.idslplugin.Location
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.misc.PipelineService
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv._
import vexiiriscv.schedule.Ages

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



class ExecutePipelinePlugin() extends FiberPlugin with PipelineService{
  setName("execute")
  val pipelineLock = Retainer()

  def freezeWhen(cond: Bool)(implicit loc: Location) = freeze.requests += cond
  def freezeIt()(implicit loc: Location) = freezeWhen(ConditionalContext.isTrue())
  def isFreezed(): Bool = freeze.valid

  override def getLinks(): Seq[Link] = logic.connectors
  val idToCtrl = mutable.LinkedHashMap[Int, CtrlLink]()
  def ctrl(id : Int)  : CtrlLink = {
    idToCtrl.getOrElseUpdate(id, CtrlLink().setCompositeName(this, "ctrl" + id))
  }

  val freeze = during build new Area{
    val requests = ArrayBuffer[Bool]()
    val valid = Bool()
  }

  val logic = during build new Area {
    pipelineLock.await()

    // Interconnect the pipeline ctrls
    val idMax = (0 +: idToCtrl.keys.toList).max
    for (i <- 0 to idMax) ctrl(i)  //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    freeze.valid := freeze.requests.orR
    ctrls.last.down.ready := !freeze.valid
    val sc = for ((from, to) <- (ctrls, ctrls.tail).zipped) yield new StageLink(from.down, to.up).withoutCollapse()
    val connectors = (sc ++ ctrls).toSeq
  }
}
