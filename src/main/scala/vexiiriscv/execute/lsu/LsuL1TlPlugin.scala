package vexiiriscv.execute.lsu

import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin

class LsuL1TlPlugin extends FiberPlugin{
  val bus = during build {
    val l1 = host[LsuL1Plugin]
    l1.logic.bus.setAsDirectionLess()
    master(l1.logic.bus.toTilelink())
  }
}
