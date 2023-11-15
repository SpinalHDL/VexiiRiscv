package vexiiriscv.schedule

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.decode.DecodePipelinePlugin

/*
How to check if a instruction can schedule :
- If one of the pipeline which implement its micro op is free
- There is no inflight non-bypassed RF write to one of the source operand
- There is no scheduling fence
- For credit based execution, check there is enough credit
*/

class DispatchPlugin(dispatchAt : Int = 3) extends FiberPlugin{
  lazy val dpp = host[DecodePipelinePlugin]
  addLockable(dpp)

  val logic = during build new Area{
    val dispatchCtrl = dpp.ctrl(dispatchAt)

    val slotsCount = 1
    val slots = for(i <- 0 until slotsCount) yield new Area{
      val valid = Bool()
//      val
    }
    dispatchCtrl.down.ready := True //TODO remove
  }
}