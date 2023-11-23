package vexiiriscv.decode

import vexiiriscv.misc.CtrlPipelinePlugin
import vexiiriscv.schedule.Ages
import spinal.core._

class DecodePipelinePlugin extends CtrlPipelinePlugin{
  override def getAge(at: Int, prediction: Boolean): Int = Ages.DECODE + at * Ages.STAGE + prediction.toInt * Ages.PREDICTION
}