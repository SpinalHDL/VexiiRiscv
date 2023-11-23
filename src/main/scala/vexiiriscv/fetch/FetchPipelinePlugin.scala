package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline
import vexiiriscv.misc.CtrlPipelinePlugin
import vexiiriscv.schedule.Ages

import scala.collection.mutable

class FetchPipelinePlugin extends CtrlPipelinePlugin{
  override def getAge(at: Int, prediction: Boolean): Int = Ages.FETCH + at * Ages.STAGE + prediction.toInt*Ages.PREDICTION
}