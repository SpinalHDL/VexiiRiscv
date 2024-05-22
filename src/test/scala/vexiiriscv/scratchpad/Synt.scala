package vexiiriscv.scratchpad

import spinal.core._
import spinal.lib.StreamFifo
import spinal.lib.eda.bench.{AlteraStdTargets, Bench, EfinixStdTargets, Rtl, Target, XilinxStdTargets}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.execute.lsu.LsuL1Plugin
import vexiiriscv.{ParamSimple, VexiiRiscv}

import scala.collection.mutable.ArrayBuffer

object IntegrationSynthBench extends App{
  LutInputs.set(6)

  val sc = SpinalConfig()
  val rtls = ArrayBuffer[Rtl]()

  def add(paramGen : => ParamSimple, name : String) = {
    rtls += Rtl(sc.generateVerilog {
      val param = paramGen
      Rtl.ffIo(VexiiRiscv(ParamSimple.setPma(param.plugins())).setDefinitionName(if(name.isEmpty) param.getName() else name.replace(" ", "_")))
    })
  }

  def add(postfix: String)(body : ParamSimple => Unit) : Unit = {

    add({
      val p = new ParamSimple()
      body(p)
      p
    }, postfix)
  }

//  add("nothing") { p =>
//
//  }

//  add("debug") { p =>
//    p.privParam.withDebug = true
//  }
//  add("debug instr") { p =>
//    p.privParam.withDebug = true
//    p.embeddedJtagInstruction = true
//    p.embeddedJtagCd = ClockDomain.current.copy(reset = Bool().setName("debugReset"))
//    p.embeddedJtagNoTapCd = ClockDomain.external("jtag", withReset = false)
//  }
//
//  add("debug jtag") { p =>
//    p.privParam.withDebug = true
//    p.embeddedJtagTap = true
//    p.embeddedJtagCd = ClockDomain.current.copy(reset = Bool().setName("debugReset"))
//  }


//    add("no fpu") { p =>
//      p.fetchL1Enable = true
//      p.lsuL1Enable = true
//      p.relaxedBranch = true
//      p.withMul = true
//      p.withDiv = true
//      p.skipFma = true
//      p.divArea = false
//    }

//    add("no fpu with bypass") { p =>
//      p.fetchL1Enable = true
//      p.lsuL1Enable = true
//      p.relaxedBranch = true
//      p.withMul = true
//      p.withDiv = true
//      p.skipFma = true
//      p.divArea = false
//      p.allowBypassFrom = 0
//    }

  //
//  add("with fpu no fma") { p =>
//    p.fetchL1Enable = true
//    p.lsuL1Enable = true
//    p.relaxedBranch = true
//    p.withRvf = true
//    p.withMul = true
//    p.withDiv = true
//    p.skipFma = true
//    p.divArea = false
//  }
//
//  add("with fpu inaccurate fma") { p =>
//    p.fetchL1Enable = true
//    p.lsuL1Enable = true
//    p.relaxedBranch = true
//    p.withRvf = true
//    p.withMul = true
//    p.withDiv = true
//    p.fpuFmaFullAccuracy = false
//    p.divArea = false
//  }
//  add("with fpu") { p =>
//    p.fetchL1Enable = true
//    p.lsuL1Enable = true
//    p.relaxedBranch = true
//    p.withRvf = true
//    p.withMul = true
//    p.withDiv = true
//    p.divArea = false
//  }
//
//  add("with fpu bypass") { p =>
//    p.fetchL1Enable = true
//    p.lsuL1Enable = true
//    p.relaxedBranch = true
//    p.withRvf = true
//    p.withMul = true
//    p.withDiv = true
//    p.allowBypassFrom = 0
//    p.divArea = false
//  }


  //  add ("fmax") { p =>
//    import p._
//    fetchCachelessForkAt = 1
//    lsuPmaAt = 1
//    lsuForkAt = 1
//    relaxedBranch = true
//    xlen = 32
//  }

//  add("fmax with counters") { p =>
//    import p._
//    fetchCachelessForkAt = 1
//    lsuPmaAt = 1
//    lsuForkAt = 1
//    relaxedBranch = true
//    xlen = 32
//    withPerformanceCounters = true
//  }
//  add("fetch lsu l1 4k") { p =>
//    p.fetchL1Enable = true
//    p.lsuL1Enable = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 1
////    p.allowBypassFrom = 0
//    p.relaxedBranch = true
//  }
//  add("fetch lsu l1 4k sb") { p =>
//    p.fetchL1Enable = true
//    p.lsuL1Enable = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 1
////    p.allowBypassFrom = 0
//    p.relaxedBranch = true
//    p.lsuStoreBufferSlots = 2
//    p.lsuStoreBufferOps = 32
//  }



//  add ("microsoc32") { p =>
//    import p._
//    fetchCachelessForkAt = 1
//    lsuPmaAt = 1
//    lsuForkAt = 1
//    relaxedBranch = true
//    allowBypassFrom = 0
//    xlen = 32
//  }
//  add("microsoc64") { p =>
//    import p._
//    fetchCachelessForkAt = 1
//    lsuPmaAt = 1
//    lsuForkAt = 1
//    relaxedBranch = true
//    allowBypassFrom = 0
//    xlen = 64
//  }

//  add("lsu16k") { p =>
//    import p._
//    lsuL1Enable = true
//    lsuL1Sets = 64
//    lsuL1Ways = 4
//  }

//  add("lsu16kwb") { p =>
//    import p._
//    lsuL1Enable = true
//    lsuL1Sets = 64
//    lsuL1Ways = 4
//    LsuL1RefillCount = 2
//    lsuL1WritebackCount = 2
//    lsuStoreBufferSlots = 2
//    lsuStoreBufferOps = 32
//  }


//
//  add("bypass all") { p =>
//    p.allowBypassFrom = 0
//  }
//
//  add("mul") { p =>
//    p.withMul = true
//  }
//  add("div") { p =>
//    p.withDiv = true
//  }
//  add("div fast") { p =>
//    p.withDiv = true
//    p.divArea = false
//  }
//  add("rvc") { p =>
//    p.withRvc = true
//    p.withAlignerBuffer = true
//  }
//
//  add("fetch l1") { p =>
//    p.withFetchL1 = true
//  }
//
//  add("fetch l1 16k") { p =>
//    p.withFetchL1 = true
//    p.fetchL1Sets = 64
//    p.fetchL1Ways = 4
//  }
//


//  add("lsu l1 4k") { p =>
//    p.lsuL1Enable = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 1
//  }
//  add("lsu l1 16k") { p =>
//    p.lsuL1Enable = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 4
//  }
//  add("lsu l1 64k") { p =>
//    p.lsuL1Enable = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 16
//  }
//  add("lsu l1 4k bypass all") { p =>
//    p.withLsuL1 = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 1
//    p.allowBypassFrom = 0
//  }
//  add("lsu l1 4k su") { p =>
//    p.regFileSync = false
//    p.withLsuL1 = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 1
//    p.privParam.withSupervisor = true;
//    p.privParam.withUser = true;
//  }
//  add("lsu l1 4k su mmu") { p =>
//    p.regFileSync = false
//    p.withLsuL1 = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 1
//    p.privParam.withSupervisor = true;
//    p.privParam.withUser = true;
//    p.withMmu = true
//  }


//  add("btb") { p =>
//    p.withBtb = true
//  }


//  add("btb gshare") { p =>
//    p.regFileSync = false
//    p.withGShare = true
//    p.withBtb = true
//  }

//  add("btb ras") { p =>
//    p.regFileSync = false
//    p.withBtb = true
//    p.withRas = true
//  }

//  add("btb gshare ras") { p =>
//    p.withBtb = true
//    p.withGShare = true
//    p.withRas = true
//  }
//
//  add("late alu") { p =>
//    p.allowBypassFrom = 0
//    p.withLateAlu = true
//  }
//
//  add("issuex2") { p =>
//    p.allowBypassFrom = 0
//    p.decoders = 2
//    p.lanes = 2
//  }
//
//  add("issuex2_late") { p =>
//    p.allowBypassFrom = 0
//    p.decoders = 2
//    p.lanes = 2
//    p.withLateAlu = true
//  }

//  add("fullPerf") { p =>
//    p.regFileSync = false
//    p.allowBypassFrom = 0
//    p.decoders = 2
//    p.lanes = 2
////    p.withDispatcherBuffer = true
//
//    p.withMul = true
//    p.withDiv = true
//
//    p.withBtb = true
//    p.withGShare = true
//    p.withRas = true
//    p.relaxedBranch = true
//    p.relaxedBtb = true
//
////    p.privParam.withSupervisor = true;
////    p.privParam.withUser = true;
////    p.withMmu = true
//
//    p.lsuL1Enable = true
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 4
//
//    p.fetchL1Enable = true
//    p.fetchL1Sets = 64
//    p.fetchL1Ways = 4
//  }

//    add("fullPerf2") { p =>
//      p.allowBypassFrom = 0
//      p.decoders = 2
//      p.lanes = 2
//      p.withDispatcherBuffer = true
//
//      p.withMul = true
//      p.withDiv = true
//
//      p.withBtb = true
//      p.withGShare = true
//      p.withRas = true
//      p.relaxedBranch = true
//      p.relaxedBtb = true
//
////      p.privParam.withSupervisor = true;
////      p.privParam.withUser = true;
////      p.withMmu = true
//
//      p.lsuL1Enable = true
//      p.lsuL1Sets = 64
//      p.lsuL1Ways = 4
//
//      p.fetchL1Enable = true
//      p.fetchL1Sets = 64
//      p.fetchL1Ways = 4
//    }


  //  def cachedPerf(p : ParamSimple): ParamSimple = {
//    p.regFileSync = false
//    p.allowBypassFrom = 0
//    p.withGShare = true
//    p.withBtb = true
//    p.withRas = true
//    p.relaxedBranch = true
//    p.relaxedBtb = true
//    p.withFetchL1 = true
//    p.withLsuL1 = true
//    p.fetchL1Sets = 64
//    p.fetchL1Ways = 4
//    p.lsuL1Sets = 64
//    p.lsuL1Ways = 4
//    p.withLsuBypass = true
//    p
//  }
//
//  add("") { p =>
//    cachedPerf(p)
//  }
//  add("") { p =>
//    cachedPerf(p)
//    p.withLateAlu = true
//  }
//
//  add("") { p =>
//    cachedPerf(p)
//    p.decoders = 2
//    p.lanes = 2
//  }
//
//  add("") { p =>
//    cachedPerf(p)
//    p.decoders = 2
//    p.lanes = 2
//    p.withLateAlu = true
//  }




  //  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = true
//    p.divRadix = 2
//  }
//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = true
//    p.divArea = false
//    p.divRadix = 2
//  }
//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = true
//    p.divRadix = 4
//  }

//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = false
//    p.withLsuL1 = true
//  }
//  add("") { p =>
//    import p._
//    decoders = 1
//    lanes = 1
//    regFileSync = false
//    withGShare = false
//    withBtb = false
//    withRas = false
//    withMul = false
//    withDiv = false
//    withLateAlu = false
//    allowBypassFrom = 0
//    relaxedBranch = true
//    relaxedShift = false
//    relaxedSrc = true
//    performanceCounters = 0
//    privParam.withSupervisor = true
//    privParam.withUser = true
//    withMmu = false
//    withRva = true
//    withRvc = false
//    withAlignerBuffer = withRvc
//    withFetchL1 = true
//    withLsuL1 = true
//    xlen = 32
//    lsuL1Sets = 64
//    lsuL1Ways = 1
//    withLsuBypass = false
//  }
//  add("") { p =>
//    import p._
//    decoders = 1
//    lanes = 1
//    regFileSync = false
//    withGShare = true
//    withBtb = true
//    withRas = true
//    withMul = false
//    withDiv = false
//    withLateAlu = false
//    allowBypassFrom = 0
//    relaxedBranch = true
//    relaxedShift = false
//    relaxedSrc = true
//    performanceCounters = 0
//    privParam.withSupervisor = true
//    privParam.withUser = true
//    withMmu = false
//    withRva = true
//    withRvc = false
//    withAlignerBuffer = withRvc
//    withFetchL1 = true
//    withLsuL1 = true
//    xlen = 32
//    lsuL1Sets = 64
//    lsuL1Ways = 1
//    withLsuBypass = false
//  }
//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = false
//    p.allowBypassFrom = 0
//  }
//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = false
//    p.allowBypassFrom = 0
//    p.withLateAlu = true
//  }

//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = false
//    p.withGShare = true
//    p.withBtb = true
//    p.withRas = true
//  }
//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = false
//    p.withGShare = true
//    p.withBtb = true
//    p.withRas = true
//    p.relaxedBranch = true
//  }

//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = false
//    p.withDiv = false
//    p.withGShare = true
//    p.withBtb = true
//    p.withRas = true
//    p.allowBypassFrom = 0
//  }
//  add("") { p =>
//    p.regFileSync = false
//    p.withMul = true
//    p.withDiv = true
//    p.withGShare = true
//    p.withBtb = true
//    p.withRas = true
//    p.allowBypassFrom = 0
//  }
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



  rtls += Rtl(sc.generateVerilog {
    val param = new ParamSimple
    import param._
    decoders = 1
    lanes = 1
    regFileSync = false
//    withGShare = true
//    withBtb = true
//    withRas = true
    fetchL1Enable = true
    lsuL1Enable = true
    withMul = true
    withDiv = true
    allowBypassFrom = 0
    relaxedBranch = true
    Rtl.ffIo(VexiiRiscv(ParamSimple.setPma(param.plugins())).setDefinitionName("vexii_1i_nobtb"))
  })

  rtls += Rtl(sc.generateVerilog {
    val param = new ParamSimple
    import param._
    decoders = 1
    lanes = 1
    regFileSync = false
    withGShare = true
    withBtb = true
    withRas = true
    withMul = true
    withDiv = true
    fetchL1Enable = true
    lsuL1Enable = true
    allowBypassFrom = 0
    relaxedBranch = true
    relaxedBtb = true
    Rtl.ffIo(VexiiRiscv(ParamSimple.setPma(param.plugins())).setDefinitionName("vexii_1i"))
  })

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

//  rtls += Rtl(sc.generateVerilog {
//    val param = new ParamSimple
//    import param._
//    decoders = 1
//    lanes = 1
//    regFileSync = false
//    withGShare = true
//    withBtb = true
//    withRas = true
//    withMul = true
//    withDiv = true
//    divArea = false
//
//    xlen = 64
//    withRva = true
//    withRvc = true
//    withAlignerBuffer = true
//
//    allowBypassFrom = 0
//    relaxedBranch = true
//    withPerformanceCounters = true
//    additionalPerformanceCounters = 0
//
//    fetchL1Enable = true
//    fetchL1Sets = 64
//    fetchL1Ways = 4
//
//    lsuL1Enable = true
//    lsuL1Sets = 64
//    lsuL1Ways = 4
//    lsuL1RefillCount = 2
//    lsuL1WritebackCount = 2
//    lsuStoreBufferSlots = 2
//    lsuStoreBufferOps = 32
//    lsuL1Coherency = true
//
//
//    val plugins = param.plugins()
//    ParamSimple.setPma(plugins)
//    plugins.foreach {
//      case p: LsuL1Plugin =>
//        p.ackIdWidth = 8
//        p.probeIdWidth = log2Up(p.writebackCount)
//      case _ =>
//    }
//    Rtl.ffIo(VexiiRiscv(plugins).setDefinitionName("vexii_debian_no_fpu"))
//  })


//  rtls += Rtl(sc.generateVerilog {
//    val param = new ParamSimple
//    import param._
//    decoders = 1
//    lanes = 1
//    regFileSync = false
//    withGShare = true
//    withBtb = true
//    withRas = true
//    withMul = true
//    withDiv = true
//    divArea = false
//    relaxedBranch = true
//    relaxedBtb = true
//
//    xlen = 64
//    privParam.withSupervisor = true
//    privParam.withUser = true
//    withMmu = true
//    withRva = true
//    withRvf = true
//    withRvd = true
//    fpuFmaFullAccuracy = false
//    withRvc = true
//    withAlignerBuffer = true
//    privParam.withDebug = true
//
//    allowBypassFrom = 0
//    withPerformanceCounters = true
//    additionalPerformanceCounters = 0
//
//    fetchL1Enable = true
//    fetchL1Sets = 64
//    fetchL1Ways = 4
//
//    lsuL1Enable = true
//    lsuL1Sets = 64
//    lsuL1Ways = 4
//    lsuL1RefillCount = 2
//    lsuL1WritebackCount = 2
//    lsuStoreBufferSlots = 2
//    lsuStoreBufferOps = 32
//    lsuL1Coherency = true
//
//
//    val plugins = param.plugins()
//    ParamSimple.setPma(plugins)
//    plugins.foreach{
//      case p : LsuL1Plugin =>
//        p.ackIdWidth = 8
//        p.probeIdWidth = log2Up(p.writebackCount)
//      case _ =>
//    }
//    Rtl.ffIo(VexiiRiscv(plugins).setDefinitionName("vexii_debian"))
//  })

  val targets = ArrayBuffer[Target]()
  targets ++=  XilinxStdTargets(withFMax = true, withArea = true)
//  targets ++= AlteraStdTargets()
//  targets ++= EfinixStdTargets(withFMax = true, withArea = true)

  Bench(rtls, targets)
}

/*
vexii_debian_no_fpu ->
Artix 7 -> 71 Mhz 6262 LUT 4710 FF
Artix 7 -> 153 Mhz 6886 LUT 4763 FF
vexii_debian fpuFmaFullAccuracy no fpu bypass
Artix 7 -> 71 Mhz 10197 LUT 7288 FF
Artix 7 -> 136 Mhz 11068 LUT 7314 FF
vexii_debian fpuFmaFullAccuracy bypass from 0
Artix 7 -> 67 Mhz 11344 LUT 7539 FF
Artix 7 -> 137 Mhz 12394 LUT 7611 FF
vexii_debian fpuFmaFullAccuracy bypass from 2->
Artix 7 -> 66 Mhz 11155 LUT 7607 FF
Artix 7 -> 136 Mhz 12109 LUT 7646 FF



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


rv32i_d1_l1_rfa_rsrc ->
Artix 7 -> 90 Mhz 1172 LUT 870 FF
Artix 7 -> 212 Mhz 1255 LUT 870 FF
rv32i_d1_l1_rfa_btb_ras_gshare_rsrc ->
Artix 7 -> 83 Mhz 1437 LUT 1049 FF
Artix 7 -> 128 Mhz 2190 LUT 1102 FF
rv32i_d1_l1_rfa_btb_ras_gshare_rbra_rsrc ->
Artix 7 -> 90 Mhz 1456 LUT 1123 FF
Artix 7 -> 163 Mhz 2226 LUT 1172 FF

rv32i_d1_l1_rfa_btb_ras_gshare_rsrc ->
Artix 7 -> 90 Mhz 1436 LUT 1049 FF
Artix 7 -> 139 Mhz 1560 LUT 1058 FF
rv32i_d1_l1_rfa_btb_ras_gshare_rbra_rsrc ->
Artix 7 -> 90 Mhz 1456 LUT 1123 FF
Artix 7 -> 164 Mhz 1573 LUT 1123 FF

rv32i_d1_l1_rfa_rsrc ->
Artix 7 -> 90 Mhz 1172 LUT 870 FF
Artix 7 -> 212 Mhz 1255 LUT 870 FF
rv32i_d1_l1_rfa_lsul1_rsrc ->
Artix 7 -> 90 Mhz 1327 LUT 1198 FF
Artix 7 -> 200 Mhz 1439 LUT 1200 FF

rv32iasu_d1_l1_rfa_fl1_lsul1xW1xS64_bp0_rsrc ->
Artix 7 -> 90 Mhz 1959 LUT 1574 FF
Artix 7 -> 177 Mhz 2091 LUT 1590 FF
rv32iasu_d1_l1_rfa_fl1_lsul1xW1xS64_bp0_btb_ras_gshare_rsrc ->
Artix 7 -> 87 Mhz 2169 LUT 1810 FF
Artix 7 -> 119 Mhz 2333 LUT 1824 FF

rv32i_d1_l1_rfa_rsrc ->
Artix 7 -> 90 Mhz 1153 LUT 933 FF
Artix 7 -> 193 Mhz 1236 LUT 935 FF
rv32i_d1_l1_rfa_rsrc_isft ->
Artix 7 -> 90 Mhz 1108 LUT 972 FF
Artix 7 -> 187 Mhz 1218 LUT 974 FF





nothing ->
Artix 7 -> 90 Mhz 1109 LUT 883 FF
bypass_all ->
Artix 7 -> 90 Mhz 1288 LUT 998 FF
mul ->
Artix 7 -> 90 Mhz 1158 LUT 955 FF
div ->
Artix 7 -> 90 Mhz 1263 LUT 989 FF
div_fast ->
Artix 7 -> 90 Mhz 1336 LUT 1021 FF
rvc ->
Artix 7 -> 90 Mhz 1293 LUT 1090 FF
fetch_l1 ->
Artix 7 -> 90 Mhz 1068 LUT 1001 FF
fetch_l1_16k ->
Artix 7 -> 90 Mhz 1170 LUT 1116 FF
lsu_l1 ->
Artix 7 -> 90 Mhz 1359 LUT 1162 FF
lsu_l1_bypass_all ->
Artix 7 -> 90 Mhz 1536 LUT 1164 FF
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1754 LUT 1349 FF
btb ->
Artix 7 -> 90 Mhz 1171 LUT 947 FF
btb_gshare ->
Artix 7 -> 86 Mhz 1246 LUT 1040 FF
btb_ras ->
Artix 7 -> 88 Mhz 1249 LUT 981 FF
btb_gshare_ras ->
Artix 7 -> 90 Mhz 1316 LUT 1074 FF
late_alu ->
Artix 7 -> 90 Mhz 1821 LUT 1087 FF
issuex2 ->
Artix 7 -> 90 Mhz 2113 LUT 1206 FF





nothing ->
Artix 7 -> 90 Mhz 1093 LUT 997 FF
Trion -> 51 Mhz LUT 1840   FF 1259
Titanium -> 179 Mhz LUT 1977   FF 1259
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1367 LUT 1264 FF
Trion -> 53 Mhz LUT 2300   FF 1591
Titanium -> 178 Mhz LUT 2203   FF 1549
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1590 LUT 1445 FF
Trion -> 43 Mhz LUT 2689   FF 1767
Titanium -> 152 Mhz LUT 2690   FF 1770

nothing ->
Artix 7 -> 90 Mhz 1093 LUT 997 FF
Trion -> 51 Mhz LUT 1840   FF 1259
Titanium -> 179 Mhz LUT 1977   FF 1259
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1362 LUT 1266 FF
Trion -> 51 Mhz LUT 2213   FF 1591
Titanium -> 143 Mhz LUT 2215   FF 1551
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1791 LUT 1453 FF
Trion -> 52 Mhz LUT 2847   FF 1777
Titanium -> 159 Mhz LUT 2726   FF 1732







nothing ->
Artix 7 -> 90 Mhz 1093 LUT 997 FF
Artix 7 -> 190 Mhz 1190 LUT 997 FF
Trion -> 53 Mhz LUT 1906   FF 1261
Trion -> 102 Mhz LUT 1906   FF 1261
Titanium -> 216 Mhz LUT 1890   FF 1261
Titanium -> 297 Mhz LUT 1890   FF 1261
Cyclone V -> 138 Mhz 857 ALMs
Cyclone IV -> 118 Mhz 2,030 LUT 1,352 FF
bypass_all ->
Artix 7 -> 0 Mhz ???
Artix 7 -> 186 Mhz 1406 LUT 998 FF
Trion -> 49 Mhz LUT 2150   FF 1261
Trion -> 100 Mhz LUT 2150   FF 1261
Titanium -> 150 Mhz LUT 2273   FF 1261
Titanium -> 293 Mhz LUT 2273   FF 1261
Cyclone V -> 133 Mhz 1,031 ALMs
Cyclone IV -> 115 Mhz 2,220 LUT 1,353 FF
mul ->
Artix 7 -> 90 Mhz 1189 LUT 1069 FF
Artix 7 -> 195 Mhz 1331 LUT 1075 FF
Trion -> 46 Mhz LUT 1931   FF 1379
Trion -> 107 Mhz LUT 1931   FF 1379
Titanium -> 173 Mhz LUT 1882   FF 1333
Titanium -> 218 Mhz LUT 1882   FF 1333
Cyclone V -> 143 Mhz 972 ALMs
Cyclone IV -> 111 Mhz 2,245 LUT 1,426 FF
div ->
Artix 7 -> 90 Mhz 1292 LUT 1103 FF
Artix 7 -> 192 Mhz 1401 LUT 1113 FF
Trion -> 46 Mhz LUT 2098   FF 1369
Trion -> 107 Mhz LUT 2098   FF 1369
Titanium -> 184 Mhz LUT 2076   FF 1368
Titanium -> 280 Mhz LUT 2076   FF 1368
Cyclone V -> 140 Mhz 1,002 ALMs
Cyclone IV -> 115 Mhz 2,333 LUT 1,458 FF
div_fast ->
Artix 7 -> 90 Mhz 1365 LUT 1135 FF
Artix 7 -> 186 Mhz 1457 LUT 1136 FF
Trion -> 41 Mhz LUT 2218   FF 1432
Trion -> 107 Mhz LUT 2218   FF 1432
Titanium -> 200 Mhz LUT 2148   FF 1405
Titanium -> 207 Mhz LUT 2148   FF 1405
Cyclone V -> 138 Mhz 1,060 ALMs
Cyclone IV -> 116 Mhz 2,423 LUT 1,490 FF
rvc ->
Artix 7 -> 90 Mhz 1293 LUT 1090 FF
Artix 7 -> 145 Mhz 1444 LUT 1092 FF
Trion -> 47 Mhz LUT 2103   FF 1354
Trion -> 84 Mhz LUT 2103   FF 1354
Titanium -> 176 Mhz LUT 2051   FF 1354
Titanium -> 204 Mhz LUT 2051   FF 1354
Cyclone V -> 104 Mhz 1,007 ALMs
Cyclone IV -> 86 Mhz 2,348 LUT 1,445 FF
fetch_l1 ->
Artix 7 -> 90 Mhz 1109 LUT 1115 FF
Artix 7 -> 192 Mhz 1228 LUT 1115 FF
Trion -> 56 Mhz LUT 1955   FF 1324
Trion -> 109 Mhz LUT 1955   FF 1324
Titanium -> 206 Mhz LUT 1965   FF 1281
Titanium -> 246 Mhz LUT 1965   FF 1281
Cyclone V -> 135 Mhz 875 ALMs
Cyclone IV -> 112 Mhz 2,143 LUT 1,405 FF
fetch_l1_16k ->
Artix 7 -> 90 Mhz 1246 LUT 1230 FF
Artix 7 -> 177 Mhz 1352 LUT 1239 FF
Trion -> 52 Mhz LUT 2070   FF 1390
Trion -> 108 Mhz LUT 2070   FF 1390
Titanium -> 181 Mhz LUT 2066   FF 1390
Titanium -> 260 Mhz LUT 2066   FF 1390
Cyclone V -> 128 Mhz 1,037 ALMs
Cyclone IV -> 110 Mhz 2,458 LUT 1,517 FF
lsu_l1 ->
Artix 7 -> 90 Mhz 1375 LUT 1266 FF
Artix 7 -> 204 Mhz 1516 LUT 1268 FF
Trion -> 48 Mhz LUT 2212   FF 1591
Trion -> 105 Mhz LUT 2212   FF 1591
Titanium -> 176 Mhz LUT 2324   FF 1551
Titanium -> 211 Mhz LUT 2324   FF 1551
Cyclone V -> FAILED
Cyclone IV -> 122 Mhz 2,417 LUT 1,542 FF
lsu_l1_bypass_all ->
Artix 7 -> 90 Mhz 1514 LUT 1267 FF
Artix 7 -> 195 Mhz 1662 LUT 1299 FF
Trion -> 47 Mhz LUT 2723   FF 1593
Trion -> 99 Mhz LUT 2723   FF 1593
Titanium -> 131 Mhz LUT 2795   FF 1553
Titanium -> 198 Mhz LUT 2795   FF 1553
Cyclone V -> FAILED
Cyclone IV -> 118 Mhz 2,612 LUT 1,543 FF
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1375 LUT 1266 FF
Artix 7 -> 204 Mhz 1516 LUT 1268 FF
Trion -> 48 Mhz LUT 2212   FF 1591
Trion -> 105 Mhz LUT 2212   FF 1591
Titanium -> 176 Mhz LUT 2324   FF 1551
Titanium -> 211 Mhz LUT 2324   FF 1551
Cyclone V -> FAILED
Cyclone IV -> 122 Mhz 2,417 LUT 1,542 FF
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1375 LUT 1266 FF
Artix 7 -> 204 Mhz 1516 LUT 1268 FF
Trion -> 48 Mhz LUT 2212   FF 1591
Trion -> 105 Mhz LUT 2212   FF 1591
Titanium -> 176 Mhz LUT 2324   FF 1551
Titanium -> 211 Mhz LUT 2324   FF 1551
Cyclone V -> FAILED
Cyclone IV -> 122 Mhz 2,417 LUT 1,542 FF
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1716 LUT 1453 FF
Artix 7 -> 190 Mhz 1923 LUT 1459 FF
Trion -> 49 Mhz LUT 2815   FF 1821
Trion -> 103 Mhz LUT 2815   FF 1821
Titanium -> 151 Mhz LUT 2797   FF 1732
Titanium -> 258 Mhz LUT 2797   FF 1732
Cyclone V -> FAILED
Cyclone IV -> 119 Mhz 3,085 LUT 1,726 FF
btb ->
Artix 7 -> 90 Mhz 1251 LUT 1092 FF
Artix 7 -> 131 Mhz 1364 LUT 1103 FF
Trion -> 50 Mhz LUT 2032   FF 1386
Trion -> 83 Mhz LUT 2032   FF 1386
Titanium -> 178 Mhz LUT 2017   FF 1401
Titanium -> 275 Mhz LUT 2017   FF 1401
Cyclone V -> 99 Mhz 959 ALMs
Cyclone IV -> 93 Mhz 2,255 LUT 1,447 FF
btb_gshare_ras ->
Artix 7 -> 90 Mhz 1355 LUT 1221 FF
Artix 7 -> 128 Mhz 1457 LUT 1232 FF
Trion -> 48 Mhz LUT 2252   FF 1602
Trion -> 83 Mhz LUT 2252   FF 1602
Titanium -> 162 Mhz LUT 2282   FF 1552
Titanium -> 260 Mhz LUT 2282   FF 1552
Cyclone V -> 97 Mhz 1,075 ALMs
Cyclone IV -> 89 Mhz 2,495 LUT 1,698 FF
late_alu ->
Artix 7 -> 90 Mhz 1725 LUT 1304 FF
Artix 7 -> 180 Mhz 1823 LUT 1315 FF
Trion -> 46 Mhz LUT 3371   FF 1536
Trion -> 87 Mhz LUT 3371   FF 1536
Titanium -> 148 Mhz LUT 3322   FF 1567
Titanium -> 185 Mhz LUT 3322   FF 1567
Cyclone V -> 118 Mhz 1,287 ALMs
Cyclone IV -> 109 Mhz 2,865 LUT 1,665 FF
issuex2 ->
Artix 7 -> 90 Mhz 2587 LUT 1477 FF
Artix 7 -> 156 Mhz 2800 LUT 1519 FF
Trion -> 45 Mhz LUT 4372   FF 1872
Trion -> 76 Mhz LUT 4372   FF 1872
Titanium -> 158 Mhz LUT 4251   FF 1872
Titanium -> 231 Mhz LUT 4251   FF 1872
Cyclone V -> FAILED
Cyclone IV -> 97 Mhz 4,273 LUT 1,750 FF
issuex2_late ->
Artix 7 -> 90 Mhz 3733 LUT 2152 FF
Artix 7 -> 149 Mhz 4009 LUT 2187 FF
Trion -> 35 Mhz LUT 6585   FF 2491
Trion -> 75 Mhz LUT 6585   FF 2491
Titanium -> 138 Mhz LUT 6686   FF 2617
Titanium -> 152 Mhz LUT 6686   FF 2617
Cyclone V -> FAILED
Cyclone IV -> 81 Mhz 5,991 LUT 2,416 FF



nothing ->
Artix 7 -> 90 Mhz 1098 LUT 997 FF
Artix 7 -> 193 Mhz 1199 LUT 997 FF
btb ->
Artix 7 -> 90 Mhz 1211 LUT 1092 FF
Artix 7 -> 134 Mhz 1333 LUT 1096 FF
btb_gshare_ras ->
Artix 7 -> 90 Mhz 1369 LUT 1221 FF
Artix 7 -> 128 Mhz 1473 LUT 1239 FF


btb ->
Artix 7 -> 90 Mhz 1198 LUT 1091 FF
Artix 7 -> 148 Mhz 1325 LUT 1091 FF
btb_gshare_ras ->
Artix 7 -> 90 Mhz 1344 LUT 1220 FF
Artix 7 -> 146 Mhz 1471 LUT 1229 FF


upstream
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1376 LUT 1266 FF
Artix 7 -> 204 Mhz 1509 LUT 1270 FF
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1778 LUT 1453 FF
Artix 7 -> 192 Mhz 2046 LUT 1502 FF
lsu_l1_64k ->
Artix 7 -> 90 Mhz 2940 LUT 2182 FF
Artix 7 -> 163 Mhz 3352 LUT 2237 FF

patched
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1372 LUT 1266 FF
Artix 7 -> 194 Mhz 1519 LUT 1266 FF
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1717 LUT 1453 FF
Artix 7 -> 191 Mhz 1960 LUT 1524 FF
lsu_l1_64k ->
Artix 7 -> 90 Mhz 2363 LUT 2179 FF
Artix 7 -> 165 Mhz 2583 LUT 2210 FF

patched without dirty
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1358 LUT 1264 FF
Artix 7 -> 198 Mhz 1539 LUT 1270 FF
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1693 LUT 1445 FF
Artix 7 -> 184 Mhz 1989 LUT 1537 FF
lsu_l1_64k ->
Artix 7 -> 90 Mhz 2344 LUT 2147 FF
Artix 7 -> 161 Mhz 2557 LUT 2155 FF

patched without plru
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1372 LUT 1266 FF
Artix 7 -> 194 Mhz 1519 LUT 1266 FF
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1688 LUT 1449 FF
Artix 7 -> 192 Mhz 1927 LUT 1521 FF
lsu_l1_64k ->
Artix 7 -> 90 Mhz 2342 LUT 2153 FF
Artix 7 -> 165 Mhz 2528 LUT 2158 FF

patched without plru / dirty bypass
lsu_l1_4k ->
Artix 7 -> 90 Mhz 1358 LUT 1264 FF
Artix 7 -> 198 Mhz 1539 LUT 1270 FF
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1589 LUT 1441 FF
Artix 7 -> 196 Mhz 1753 LUT 1441 FF
lsu_l1_64k ->
Artix 7 -> 90 Mhz 2298 LUT 2121 FF
Artix 7 -> 165 Mhz 2485 LUT 2136 FF


fullPerf no bpre->
Artix 7 -> 90 Mhz 3784 LUT 2621 FF
Artix 7 -> 142 Mhz 4098 LUT 2707 FF
fullPerf2 no bpre ->
Artix 7 -> 88 Mhz 4714 LUT 3056 FF
Artix 7 -> 140 Mhz 5079 LUT 3087 FF

fullPerf ->
Artix 7 -> 77 Mhz 4144 LUT 3257 FF
Artix 7 -> 114 Mhz 4534 LUT 3303 FF
fullPerf2 ->
Artix 7 -> 65 Mhz 5030 LUT 3694 FF
Artix 7 -> 117 Mhz 5404 LUT 3749 FF


lsu_l1_4k ->
Artix 7 -> 90 Mhz 1379 LUT 1273 FF
Artix 7 -> 196 Mhz 1546 LUT 1274 FF
lsu_l1_16k ->
Artix 7 -> 90 Mhz 1660 LUT 1472 FF
Artix 7 -> 206 Mhz 1958 LUT 1577 FF


fmax ->
Artix 7 -> 234 Mhz 1256 LUT 1128 FF
fmax_with_counters ->
Artix 7 -> 219 Mhz 1351 LUT 1198 FF
fetch_lsu_l1_4k ->
Artix 7 -> 223 Mhz 1759 LUT 1510 FF
fetch_lsu_l1_4k_sb ->
Artix 7 -> 219 Mhz 1762 LUT 1596 FF

nothing ->
Artix 7 -> 204 Mhz 1233 LUT 1032 FF
fmax ->
Artix 7 -> 247 Mhz 1267 LUT 1123 FF
fetch_lsu_l1_4k ->
Artix 7 -> 236 Mhz 1501 LUT 1431 FF
fetch_lsu_l1_4k_sb ->
Artix 7 -> 225 Mhz 1611 LUT 1571 FF

nothing ->
Artix 7 -> 90 Mhz 1102 LUT 1031 FF
Artix 7 -> 203 Mhz 1223 LUT 1031 FF
debug ->
Artix 7 -> 90 Mhz 1206 LUT 1129 FF
Artix 7 -> 200 Mhz 1351 LUT 1129 FF
debug_instr ->
Artix 7 -> 90 Mhz 1443 LUT 1575 FF
Artix 7 -> 195 Mhz 1573 LUT 1575 FF
debug_jtag ->
Artix 7 -> 90 Mhz 1427 LUT 1608 FF
Artix 7 -> 193 Mhz 1579 LUT 1608 FF


fullPerf ->
Artix 7 -> 87 Mhz 4354 LUT 3050 FF
Artix 7 -> 127 Mhz 4663 LUT 3061 FF
fullPerf2 ->
Artix 7 -> 90 Mhz 4308 LUT 3419 FF
Artix 7 -> 156 Mhz 4813 LUT 3593 FF

no_fpu ->
Artix 7 -> 90 Mhz 1431 LUT 1577 FF
Artix 7 -> 203 Mhz 1609 LUT 1588 FF
with_fpu_no_fma ->
Artix 7 -> 90 Mhz 2626 LUT 2633 FF
Artix 7 -> 175 Mhz 2933 LUT 2653 FF
with_fpu_inaccurate_fma ->
Artix 7 -> 90 Mhz 2876 LUT 2753 FF
Artix 7 -> 192 Mhz 3160 LUT 2771 FF
with_fpu ->
Artix 7 -> 90 Mhz 3086 LUT 2865 FF
Artix 7 -> 186 Mhz 3430 LUT 2899 FF
with_fpu_bypass ->
Artix 7 -> 90 Mhz 3773 LUT 2996 FF
Artix 7 -> 183 Mhz 4158 LUT 3029 FF

no_fpu ->
Artix 7 -> 90 Mhz 1725 LUT 1715 FF
Artix 7 -> 206 Mhz 1950 LUT 1745 FF
no_fpu_with_bypass ->
Artix 7 -> 90 Mhz 1882 LUT 1716 FF
Artix 7 -> 202 Mhz 2129 LUT 1728 FF 
with_fpu_no_fma ->
Artix 7 -> 90 Mhz 3048 LUT 2855 FF
Artix 7 -> 181 Mhz 3478 LUT 2871 FF
with_fpu_inaccurate_fma ->
Artix 7 -> 90 Mhz 3317 LUT 2985 FF
Artix 7 -> 180 Mhz 3689 LUT 3022 FF
with_fpu ->
Artix 7 -> 90 Mhz 3504 LUT 3097 FF
Artix 7 -> 184 Mhz 3898 LUT 3149 FF
with_fpu_bypass ->
Artix 7 -> 90 Mhz 4217 LUT 3227 FF
Artix 7 -> 185 Mhz 4634 LUT 3264 FF

with_fpu_inaccurate_fma ->
Artix 7 -> 90 Mhz 3468 LUT 3219 FF
Artix 7 -> 181 Mhz 3874 LUT 3238 FF

with_fpu_inaccurate_fma ->
Artix 7 -> 90 Mhz 3502 LUT 3109 FF
Artix 7 -> 185 Mhz 3885 LUT 3143 FF

with_fpu_inaccurate_fma i2f ->
Artix 7 -> 90 Mhz 3605 LUT 3173 FF
Artix 7 -> 181 Mhz 3986 LUT 3195 FF

with_fpu_inaccurate_fma f2i ->
Artix 7 -> 90 Mhz 3808 LUT 3227 FF
Artix 7 -> 165 Mhz 4199 LUT 3274 FF

with_fpu_inaccurate_fma ->
Artix 7 -> 90 Mhz 3877 LUT 3229 FF
Artix 7 -> 177 Mhz 4310 LUT 3309 FF
 */