package vexiiriscv

import spinal.core._
import spinal.lib.misc.database.Database.blocking
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.fetch.Fetch

/**
 * Define global variables for plugin to share in non-verbose ways
 *
 * A few notes about physical/virtual/mixed addresses, as they are a tricky thing if you try to scrap away any unecessary bit :
 * In RISC-V, the addresses manipulated by the CPU before the MMU can be physical or virtual.
 * The thing is that Physical addresses are unsigned, while virtual addresses are signed (so sign extended to XLEN).
 * So let's say you have :
 * - 32 bits physical address width
 * - 39 bits virtual address width
 * Then, addresses in the CPU before the MMU (mixed addresses) need to have 40 bits (39 bits + 1 bit of sign extension), that allow for instance :
 * - physical address trap at 0x40_0000_0000 to be stored in let's say MEPC to store that address while avoiding the sign extension
 */
object Global extends AreaRoot {
  // Elaboration time constants
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

  // Pipelining keys
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
