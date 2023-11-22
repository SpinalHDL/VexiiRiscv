package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.pipeline._

object Execute extends AreaRoot{
  val BYPASSED = Payload(Bool())
}
