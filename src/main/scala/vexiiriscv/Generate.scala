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

  sc.addTransformationPhase(new MultiPortWritesSymplifier)
  val report = sc.generateVerilog {
    val plugins = param.plugins()
    val regions = ArrayBuffer[PmaRegion](
      new PmaRegionImpl(
        mapping = SizeMapping(0x80000000l, 0x80000000l),
        isMain = true,
        isExecutable = true,
        transfers = M2sTransfers(
          get = SizeRange.all,
          putFull = SizeRange.all,
        )
      ),
      new PmaRegionImpl(
        mapping = SizeMapping(0x10000000l, 0x10000000l),
        isMain = false,
        isExecutable = true,
        transfers = M2sTransfers(
          get = SizeRange.all,
          putFull = SizeRange.all,
        )
      )
    )
    plugins.foreach {
      case p: FetchCachelessPlugin => p.regions.load(regions)
      case p: LsuCachelessPlugin => p.regions.load(regions)
      case p: FetchL1Plugin => p.regions.load(regions)
      case p: LsuPlugin => p.ioRegions.load(regions)
      case p: LsuL1Plugin => p.regions.load(regions)
      case _ =>
    }
    VexiiRiscv(plugins)
  }
}

