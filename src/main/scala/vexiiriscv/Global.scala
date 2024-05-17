package vexiiriscv

import spinal.core._
import spinal.lib.misc.database.Database.blocking
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.fetch.Fetch

/**
 * Define global variables for plugin to share in very non-verbose ways
 */
object Global extends AreaRoot{
  val PHYSICAL_WIDTH   = blocking[Int]
  val MIXED_WIDTH      = blocking[Int]
  val VIRTUAL_WIDTH    = blocking[Int]
  val PC_WIDTH         = blocking[Int]
  val TVAL_WIDTH       = blocking[Int]
  val HART_COUNT       = blocking[Int]
  val CODE_WIDTH       = blocking[Int]
  val TRAP_ARG_WIDTH   = blocking[Int]

  def pcWithSignMsb = PHYSICAL_WIDTH < MIXED_WIDTH
  def expendPc(pc : UInt, width : Int) = Global.pcWithSignMsb.mux(
    pc.asSInt.resize(width bits).asUInt,
    pc.resize(width bits)
  )


  val VIRTUAL_ADDRESS = Payload(UInt(VIRTUAL_WIDTH bits))
  val MIXED_ADDRESS = Payload(UInt(MIXED_WIDTH bits))
  val PHYSICAL_ADDRESS = Payload(UInt(PHYSICAL_WIDTH bits))
  val PC = Payload(UInt(PC_WIDTH bits))
  val PC_TARGET = Payload(UInt(PC_WIDTH-Fetch.SLICE_RANGE_LOW bits))
  val CODE = Payload(Bits(CODE_WIDTH bits))
  val TRAP_ARG = Payload(Bits(TRAP_ARG_WIDTH bits))
  val TVAL = Payload(Bits(TVAL_WIDTH bits))
  val TRAP = Payload(Bool())
  val COMMIT = Payload(Bool())
  val COMPLETED = Payload(Bool())

  def HART_ID_WIDTH = log2Up(HART_COUNT)
  val HART_ID = Payload(UInt(HART_ID_WIDTH bits))
  def hartsIds = 0 until HART_COUNT

}
