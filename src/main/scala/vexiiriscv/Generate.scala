package vexiiriscv

import spinal.core._
import spinal.lib.LatencyAnalysis
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.{M2sTransfers, SizeRange}
import spinal.lib.misc.PathTracer
import spinal.lib.system.tag.{PmaRegion, PmaRegionImpl}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.execute.SrcPlugin
import vexiiriscv.execute.lsu._
import vexiiriscv.fetch._

import scala.collection.mutable.ArrayBuffer

object Generate extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    param.addOptions(this)
  }.parse(args, Unit).nonEmpty)

  val report = sc.generateVerilog {
    val plugins = param.plugins()
    ParamSimple.setPma(plugins)
    VexiiRiscv(plugins)
  }
}

