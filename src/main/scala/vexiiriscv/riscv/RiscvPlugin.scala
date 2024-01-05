package vexiiriscv.riscv

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.Fetch

class RiscvPlugin(var xlen : Int,
                  var hartCount : Int) extends FiberPlugin{

  val logic = during build new Area{
    if(Riscv.RVC.isEmpty) Riscv.RVC.set(false)
    if(Riscv.RVM.isEmpty) Riscv.RVM.set(false)
    if(Riscv.RVF.isEmpty) Riscv.RVF.set(false)
    if(Riscv.RVD.isEmpty) Riscv.RVD.set(false)
    if(Riscv.RVA.isEmpty) Riscv.RVA.set(false)
    Riscv.XLEN.set(xlen)
    Riscv.FLEN.set(List(Riscv.RVF.get.toInt*32, Riscv.RVD.get.toInt*64).max)
    Riscv.LSLEN.set(List(Riscv.XLEN.get, Riscv.FLEN.get).max)
    Global.HART_COUNT.set(hartCount)
    Fetch.SLICE_WIDTH.set(if(Riscv.RVC) 16 else 32)
    Fetch.SLICE_BYTES.set(if(Riscv.RVC) 2 else 4)
    Fetch.SLICE_COUNT.set(Fetch.WORD_WIDTH/Fetch.SLICE_WIDTH)
    Fetch.SLICE_RANGE_LOW.set(if (Riscv.RVC) 1 else 2)
    Fetch.SLICE_RANGE.set((Fetch.SLICE_RANGE_LOW.get + log2Up(Fetch.SLICE_COUNT.get) - 1) downto Fetch.SLICE_RANGE_LOW.get)
    Fetch.ID_WIDTH.set(10)
    Decode.DOP_ID_WIDTH.set(10)
    Decode.UOP_ID_WIDTH.set(10)
  }
}
