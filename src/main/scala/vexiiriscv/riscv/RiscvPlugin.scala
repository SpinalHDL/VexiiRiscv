package vexiiriscv.riscv

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.fetch.Fetch

class RiscvPlugin(var xlen : Int,
                  var rvc : Boolean,
                  var hartCount : Int) extends FiberPlugin{

  val logic = during build new Area{
    Riscv.XLEN.set(xlen)
    Riscv.RVC.set(rvc)
    Riscv.RVF.set(false)
    Riscv.RVD.set(false)
    Global.HART_COUNT.set(hartCount)
    Fetch.SLICE_WIDTH.set(if(Riscv.RVC) 16 else 32)
    Fetch.SLICE_BYTES.set(if(Riscv.RVC) 2 else 4)
    Fetch.SLICE_COUNT.set(Fetch.WORD_WIDTH/Fetch.SLICE_WIDTH)
    Fetch.SLICE_RANGE_LOW.set(if (Riscv.RVC) 1 else 2)
    Fetch.SLICE_RANGE.set((Fetch.SLICE_RANGE_LOW.get + log2Up(Fetch.SLICE_COUNT.get) - 1) downto Fetch.SLICE_RANGE_LOW.get)
  }
}
