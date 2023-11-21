package vexiiriscv.fetch

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.database.Database._
import vexiiriscv.Global
import vexiiriscv.riscv

object Fetch extends AreaRoot{
  val WORD_WIDTH  = blocking[Int]
  def WORD_BYTES = WORD_WIDTH/8
  val SLICE_WIDTH = blocking[Int]
  val SLICE_BYTES = blocking[Int]
  val SLICE_COUNT = blocking[Int]
  val SLICE_RANGE_LOW = blocking[Int]
  val SLICE_RANGE = blocking[Range]

  val WORD = Payload(Bits(WORD_WIDTH bits))
  val WORD_PC = Payload(Global.PC)
}
