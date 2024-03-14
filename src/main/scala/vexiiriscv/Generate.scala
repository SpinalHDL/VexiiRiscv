package vexiiriscv

import spinal.core._
import spinal.lib.LatencyAnalysis
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.{M2sTransfers, SizeRange}
import spinal.lib.misc.PathTracer
import spinal.lib.system.tag.{PmaRegion, PmaRegionImpl}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.decode.{Decode, DecodePipelinePlugin}
import vexiiriscv.execute.{CsrRamPlugin, ExecuteLanePlugin, SrcPlugin}
import vexiiriscv.execute.lsu._
import vexiiriscv.fetch._
import vexiiriscv.regfile.RegFilePlugin

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




object GeneratTweeked extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    param.addOptions(this)
  }.parse(args, Unit).nonEmpty)

  param.fetchL1Enable = true
  param.lsuL1Enable = true
  param.lsuL1Sets = 64
  param.lsuL1Ways = 1
  param.relaxedBranch = true
  param.lsuStoreBufferSlots = 2
  param.lsuStoreBufferOps = 32

  val report = sc.generateVerilog {
    val plugins = param.plugins()
    ParamSimple.setPma(plugins)
    VexiiRiscv(plugins)
  }

  report.toplevel.database.on{
    //    val from = report.toplevel.host[ExecuteLanePlugin].execute(0).down(Decode.UOP)
    //    val from = report.toplevel.host[LsuPlugin].logic.storeBuffer.slots(0).tag
    val from = report.toplevel.host[DecodePipelinePlugin].ctrl(1).lane(0).up(Decode.INSTRUCTION)
    val to = report.toplevel.host[FetchL1Plugin].logic.banks.head.read.cmd.valid
    //    val to = report.toplevel.host[CsrRamPlugin].logic.writeLogic.port.data
    println(PathTracer.impl(from, to).report())
  }
}

