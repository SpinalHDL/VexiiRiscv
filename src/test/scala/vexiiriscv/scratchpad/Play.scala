package vexiiriscv.scratchpad

import spinal.core._
import spinal.lib.LatencyAnalysis
import spinal.lib.misc.PathTracer
import vexiiriscv._
import vexiiriscv.VexiiRiscv
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.execute.{SrcPlugin}

object Play1 extends App {
  val sc = SpinalConfig()
  val report = sc.generateVerilog {
    val param = new ParamSimple()
    VexiiRiscv(param.plugins())
  }

  val host = report.toplevel.host
  val src = host.find[SrcPlugin](_.layer.name == "early0")
//  println(LatencyAnalysis(
//    host[LsuCachelessPlugin].logic.redoPort,
//    src.logic.src(src.SRC1)
//  ))
//  println(PathTracer.impl(
//    host[LsuCachelessPlugin].logic.redoPort,
//    src.logic.src(src.SRC1)
//  ).report())
}

