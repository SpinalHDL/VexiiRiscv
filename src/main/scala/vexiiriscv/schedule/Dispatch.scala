package vexiiriscv.schedule

import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline.Payload

object Dispatch extends AreaRoot{
  val MASK = Payload(Bool())
}
