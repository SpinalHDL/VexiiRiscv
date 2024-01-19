package vexiiriscv.scratchpad

import spinal.core._
import spinal.lib.StreamFifo
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.{ParamSimple, VexiiRiscv}

import scala.collection.mutable.ArrayBuffer

object IntegrationSynthBench extends App{
  LutInputs.set(6)

  val sc = SpinalConfig()
  sc.addTransformationPhase(new MultiPortWritesSymplifier)
  val rtls = ArrayBuffer[Rtl]()

  def add(param : ParamSimple, name : String) = {
    rtls += Rtl(sc.generateVerilog {
      Rtl.ffIo(VexiiRiscv(param.plugins()).setDefinitionName(if(name.isEmpty) param.getName() else name))
    })
  }

  def add(postfix: String)(body : ParamSimple => Unit) : Unit = {
    val p = new ParamSimple()
    body(p)
    add(p, postfix)
  }

  add(""){ p =>
    p.regFileSync = false
    p.withMul = false
    p.withDiv = false
  }
  add("") { p =>
    p.regFileSync = false
    p.withMul = false
    p.withDiv = false
    p.allowBypassFrom = 0
  }
  add("") { p =>
    p.regFileSync = false
    p.withMul = false
    p.withDiv = false
    p.withGShare = true
    p.withBtb = true
    p.withRas = true
    p.allowBypassFrom = 0
  }
  add("") { p =>
    p.regFileSync = false
    p.withMul = true
    p.withDiv = true
    p.withGShare = true
    p.withBtb = true
    p.withRas = true
    p.allowBypassFrom = 0
  }
//  add("") { p =>
//    p.decoders = 1
//    p.lanes = 1
//    p.regFileSync = false
//    p.withGShare = true
//    p.withBtb = true
//    p.withRas = true
//    p.withLateAlu = false
//    p.allowBypassFrom = 0
//    p.relaxedBranch = false
//    p.relaxedShift = false
//    p.relaxedSrc = true
//    p.performanceCounters = 0
//    p.withRvc = false
//  }



  //  rtls += Rtl(sc.generateVerilog {
//    val param = new ParamSimple
//    import param._
//    decoders = 1
//    lanes = 1
//    regFileSync = false
//    withGShare = true
//    withBtb = true
//    withRas = true
//    //    withMul = false
//    //    withDiv = false
//    withLateAlu = false
//    allowBypassFrom = 0
//    relaxedBranch = false
//    relaxedShift = false
//    relaxedSrc = true
//    performanceCounters = 0
//    withRvc = false
//    Rtl.ffIo(VexiiRiscv(param.plugins()).setDefinitionName("vexii_1i"))
//  })
//
//  rtls += Rtl(sc.generateVerilog {
//    val param = new ParamSimple
//    import param._
//    decoders = 1
//    lanes = 1
//    regFileSync = false
//    withGShare = true
//    withBtb = true
//    withRas = true
//    //    withMul = false
//    //    withDiv = false
//    withLateAlu = false
//    allowBypassFrom = 0
//    relaxedBranch = false
//    relaxedShift = false
//    relaxedSrc = true
//    performanceCounters = 0
//    withAlignerBuffer = true
//    withRvc = true
//    Rtl.ffIo(VexiiRiscv(param.plugins()).setDefinitionName("vexii_1i_rvc"))
//  })
//  rtls += Rtl(sc.generateVerilog {
//    val param = new ParamSimple
//    param.decoders = 2
//    param.lanes = 2
//    Rtl.ffIo(VexiiRiscv(param.plugins()).setDefinitionName("vexii_2i"))
//  })

//    rtls += Rtl(sc.generateVerilog {
//      new StreamFifo(UInt(8 bits), 16)
//    })
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

vexii_1i ->
Artix 7 -> 90 Mhz 2090 LUT 1292 FF
Artix 7 -> 149 Mhz 2222 LUT 1292 FF


vexii_1i ->
Artix 7 -> 90 Mhz 2057 LUT 1293 FF
Artix 7 -> 139 Mhz 2195 LUT 1293 FF
vexii_1i_rvc ->
Artix 7 -> 83 Mhz 2286 LUT 1462 FF
Artix 7 -> 119 Mhz 2462 LUT 1462 FF

 */