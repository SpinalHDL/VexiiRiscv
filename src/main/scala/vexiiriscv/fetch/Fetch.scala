package vexiiriscv.fetch

import spinal.core._
import spinal.lib.misc.pipeline.SignalKey
import spinal.lib.misc.database.Database._
import vexiiriscv.Global
import vexiiriscv.riscv

object Fetch extends AreaRoot{
  val WORD_WIDTH  = blocking[Int]
  def SLICE_WIDTH = blocking[Int]
  def SLICE_BYTES = blocking[Int]
  def SLICE_COUNT = blocking[Int]

  val WORD = SignalKey(Bits(WORD_WIDTH bits))
  val WORD_PC = SignalKey(Global.PC)


//  val INSTRUCTION_SLICE_COUNT = Stageable(UInt(if (Global.RVC) 1 bits else 0 bits)) // minus one => RVC => 0, normal => 1
//  def SLICE_RANGE_LOW = if (Global.RVC) 1 else 2
//  def SLICE_RANGE = (SLICE_RANGE_LOW + log2Up(SLICE_COUNT) - 1) downto SLICE_RANGE_LOW
}
