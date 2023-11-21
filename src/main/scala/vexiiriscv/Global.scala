package vexiiriscv

import spinal.core._
import spinal.lib.misc.database.Database.blocking
import spinal.lib.misc.pipeline.Payload

/**
 * Define global variables for plugin to share in very non-verbose ways
 */
object Global extends AreaRoot{
  val PHYSICAL_WIDTH   = blocking[Int]
  val MIXED_WIDTH      = blocking[Int]
  val VIRTUAL_WIDTH    = blocking[Int]
  val PC_WIDTH         = blocking[Int]
  val HART_COUNT       = blocking[Int]

  val VIRTUAL_ADDRESS = Payload(UInt(VIRTUAL_WIDTH bits))
  val MIXED_ADDRESS = Payload(UInt(MIXED_WIDTH bits))
  val PHYSICAL_ADDRESS = Payload(UInt(PHYSICAL_WIDTH bits))
  val PC = Payload(UInt(PC_WIDTH bits))

  def HART_ID_WIDTH = log2Up(HART_COUNT)
  val HART_ID = Payload(UInt(HART_ID_WIDTH bits))

}
