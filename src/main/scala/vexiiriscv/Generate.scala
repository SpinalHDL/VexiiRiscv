package vexiiriscv

import spinal.core._
import spinal.lib.LatencyAnalysis
import spinal.lib.misc.PathTracer
import vexiiriscv._
import vexiiriscv.VexiiRiscv
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.execute.{LsuCachelessPlugin, SrcPlugin}

object Generate extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    param.addOptions(this)
  }.parse(args, Unit).nonEmpty)

  sc.addTransformationPhase(new MultiPortWritesSymplifier)
  val report = sc.generateVerilog {
    VexiiRiscv(param.plugins())
  }
}

