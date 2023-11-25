package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._

object Execute extends AreaRoot{
  val BYPASSED = Payload(Bool())
  val LANE_AGE_WIDTH = blocking[Int]
  val LANE_AGE = Payload(UInt(LANE_AGE_WIDTH bits))
}
