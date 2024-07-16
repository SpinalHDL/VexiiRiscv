package vexiiriscv.fetch

import spinal.core.{Bool, Bundle}
import spinal.lib.Stream
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global

case class PrefetchCmd() extends Bundle {
  val pc = Global.PC()
}

case class FetchProbe() extends Bundle {
  val pc = Global.PC()
  val trap = Bool()
}


abstract class PrefetcherPlugin extends FiberPlugin {
  val io = during build Stream(PrefetchCmd())
}
