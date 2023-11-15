package vexiiriscv.schedule

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.decode.DecodePipelinePlugin

class DispatchPlugin(dispatchAt : Int = 3) extends FiberPlugin{
  lazy val dpp = host[DecodePipelinePlugin]
  during setup{
    dpp.retain()
  }
  val logic = during build new Area{
    val dispatchCtrl = dpp.ctrl(dispatchAt)
    dispatchCtrl.down.ready := True

    dpp.release()
  }
}