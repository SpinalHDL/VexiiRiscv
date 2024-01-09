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
    import param._
    decoders = 1
    lanes = 1
    regFileSync = false
    withGShare = true
    withBtb = true
    withRas = true
    //    withMul = false
    //    withDiv = false
    withLateAlu = false
    allowBypassFrom = 0
    relaxedBranch = false
    relaxedShift = false
    relaxedSrc = true
    performanceCounters = 0
    Rtl.ffIo(VexiiRiscv(param.plugins()).setDefinitionName("vexii_1i"))
  })
//  rtls += Rtl(sc.generateVerilog {
//    val param = new ParamSimple
//    param.decoders = 2
//    param.lanes = 2
//    Rtl.ffIo(VexiiRiscv(param.plugins()).setDefinitionName("vexii_2i"))
//  })
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}

/*
2 issue + lates alues
Artix 7 -> 70 Mhz 5318 LUT 2404 FF
Artix 7 -> 120 Mhz 5515 LUT 2410 FF

2 issue
Artix 7 -> 71 Mhz 3755 LUT 1749 FF
Artix 7 -> 129 Mhz 3920 LUT 1753 FF

1 issue ->
Artix 7 -> 90 Mhz 2014 LUT 1231 FF
Artix 7 -> 146 Mhz 2107 LUT 1231 FF

vexii_1i ->
Artix 7 -> 90 Mhz 2087 LUT 1267 FF
Artix 7 -> 145 Mhz 2167 LUT 1267 FF

 */