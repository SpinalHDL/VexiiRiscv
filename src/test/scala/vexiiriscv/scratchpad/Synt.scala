package vexiiriscv.scratchpad

import spinal.core.{LutInputs, SpinalConfig, SpinalVerilog}
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.{ParamSimple, VexiiRiscv}

import scala.collection.mutable.ArrayBuffer

object IntegrationSynthBench extends App{
  LutInputs.set(6)

  val sc = SpinalConfig()
  sc.addTransformationPhase(new MultiPortWritesSymplifier)
  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(sc.generateVerilog {
    val param = new ParamSimple
    param.decoders = 1
    param.lanes = 1
    Rtl.ffIo(VexiiRiscv(param.plugins()).setDefinitionName("vexii_1i"))
  })
  rtls += Rtl(sc.generateVerilog {
    val param = new ParamSimple
    param.decoders = 2
    param.lanes = 2
    Rtl.ffIo(VexiiRiscv(param.plugins()).setDefinitionName("vexii_2i"))
  })
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}