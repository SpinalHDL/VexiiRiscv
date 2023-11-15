package vexiiriscv.decode

import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline.SignalKey
import vexiiriscv.Global

object Decode extends AreaRoot{
  val LANES  = blocking[Int]
  val INSTRUCTION_WIDTH = blocking[Int]

  val INSTRUCTION = SignalKey(Bits(INSTRUCTION_WIDTH bits))
  val MICRO_OP = SignalKey(Bits(INSTRUCTION_WIDTH bits))
  val ALIGNED_MASK = SignalKey(Bool())
}
