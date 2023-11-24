package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Lock
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

class ExecuteUnitPipelinePlugin() extends FiberPlugin with PipelineService{
  val pipelineLock = Lock()

  override def getLinks(): Seq[Link] = logic.connectors
  val idToCtrl = mutable.LinkedHashMap[Int, CtrlLink]()
  def ctrl(id : Int)  : CtrlLink = {
    idToCtrl.getOrElseUpdate(id, CtrlLink().setCompositeName(this, "ctrl" + id))
  }

  def getAge(at: Int, prediction: Boolean): Int = Ages.EU + at * Ages.STAGE + prediction.toInt * Ages.PREDICTION

  val logic = during build new Area {
    pipelineLock.await()

    // Interconnect the pipeline ctrls
    val idMax = (0 +: idToCtrl.keys.toList).max
    for (i <- 0 to idMax) ctrl(i)  //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    ctrls.last.down.setAlwaysReady()
    val sc = for ((from, to) <- (ctrls, ctrls.tail).zipped) yield new StageLink(from.down, to.up).withoutCollapse()
    val connectors = (sc ++ ctrls).toSeq
  }
}
