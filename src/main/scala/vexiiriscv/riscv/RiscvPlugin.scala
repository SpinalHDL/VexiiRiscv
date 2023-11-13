package vexiiriscv.riscv

import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global

class RiscvPlugin(var xlen : Int,
                  var hartCount : Int) extends FiberPlugin{

  during setup{
    Riscv.XLEN.set(xlen)
    Global.HART_COUNT.set(hartCount)
  }
}
