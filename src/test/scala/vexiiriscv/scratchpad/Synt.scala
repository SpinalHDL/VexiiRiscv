package vexiiriscv.scratchpad

import spinal.core.{LutInputs, SpinalVerilog}
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import vexiiriscv.{ParamSimple, VexiiRiscv}

import scala.collection.mutable.ArrayBuffer

object IntegrationSynthBench extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(VexiiRiscv(new ParamSimple().plugins()))))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}