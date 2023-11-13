package vexiiriscv.fetch

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv._

class CachelessPlugin extends FiberPlugin{
  lazy val pp = host[PipelinePlugin]
  during setup{
    pp.retain()
  }
  val logic = during build new Area{
    val x = CombInit(pp.ctrl(2)(Global.PC))
    pp.release()
  }
}
