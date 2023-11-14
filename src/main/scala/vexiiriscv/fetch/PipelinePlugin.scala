package vexiiriscv.fetch

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline

import scala.collection.mutable

class PipelinePlugin extends FiberPlugin{
  val idToCtrl = mutable.LinkedHashMap[Int, pipeline.CtrlConnector]()
  def ctrl(id : Int) = idToCtrl.getOrElseUpdate(id, pipeline.CtrlConnector())
  val logic = during build new Area{
    val idMax = idToCtrl.keys.max
    for(i <- 0 to idMax) ctrl(i).unsetName() //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    val sc = for((from, to) <- (ctrls, ctrls.tail).zipped) yield new pipeline.StageConnector(from.down, to.up) //.withoutCollapse()
    pipeline.Builder((sc ++ ctrls).toSeq)
    ctrls.last.down.ready := True //TODO remove
  }
}
