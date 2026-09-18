package vexiiriscv.riscv

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.Fetch
import vexiiriscv.execute.fpu.FpuUtils

/**
 * The aim of this plugin is mostly to serve as a RISC-V CPU parameter trampoline to globally shared constants
 */
class RiscvPlugin(var xlen : Int,
                  var hartCount : Int,
                  var isa: Set[String]) extends FiberPlugin {

  def has(exts: String*) = exts.forall(ext => isa.contains(ext.toLowerCase))

  val logic = during build new Area {
    if(Riscv.RVC.isEmpty) Riscv.RVC.set(has("c"))
    if(Riscv.RVM.isEmpty) Riscv.RVM.set(false)
    if(Riscv.RVF.isEmpty) Riscv.RVF.set(has("f"))
    if(Riscv.RVD.isEmpty) Riscv.RVD.set(has("d"))
    if(Riscv.RVE.isEmpty) Riscv.RVE.set(has("e"))
    if(Riscv.RVQ.isEmpty) Riscv.RVQ.set(has("q"))
    if(Riscv.RVH.isEmpty) Riscv.RVH.set(has("h"))
    if(Riscv.RVZaamo.isEmpty) Riscv.RVZaamo.set(has("zaamo"))
    if(Riscv.RVZalrsc.isEmpty) Riscv.RVZalrsc.set(has("zalrsc"))
    if(Riscv.RVA.isEmpty) Riscv.RVA.set(has("a"))
    if(Riscv.RVB.isEmpty) Riscv.RVB.set(has("zba", "zbb", "zbs"))
    if(Riscv.RVZba.isEmpty) Riscv.RVZba.set(has("zba"))
    if(Riscv.RVZbb.isEmpty) Riscv.RVZbb.set(has("zbb"))
    if(Riscv.RVZbc.isEmpty) Riscv.RVZbc.set(has("zbc"))
    if(Riscv.RVZbs.isEmpty) Riscv.RVZbs.set(has("zbs"))
    if(Riscv.RVZfa.isEmpty) Riscv.RVZfa.set(has("zfa"))
    if(Riscv.RVZfh.isEmpty) Riscv.RVZfh.set(has("zfh"))
    Riscv.XLEN.set(xlen)
    Riscv.FLEN.set(FpuUtils.rsFloatWidth)
    Riscv.LSLEN.set(List(Riscv.XLEN.get, Riscv.FLEN.get).max)
    Global.HART_COUNT.set(hartCount)
    Fetch.SLICE_WIDTH.set(if(Riscv.RVC) 16 else 32)
    Fetch.SLICE_BYTES.set(if(Riscv.RVC) 2 else 4)
    Fetch.SLICE_COUNT.set(Fetch.WORD_WIDTH/Fetch.SLICE_WIDTH)
    Fetch.SLICE_RANGE_LOW.set(if (Riscv.RVC) 1 else 2)
    Fetch.SLICE_RANGE.set((Fetch.SLICE_RANGE_LOW.get + log2Up(Fetch.SLICE_COUNT.get) - 1) downto Fetch.SLICE_RANGE_LOW.get)
    Fetch.ID_WIDTH.set(10)
    Decode.DOP_ID_WIDTH.set(10)
    Decode.UOP_ID_WIDTH.set(16)
  }
}
