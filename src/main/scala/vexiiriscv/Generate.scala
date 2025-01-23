package vexiiriscv

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib.{AnalysisUtils, LatencyAnalysis}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.{M2sTransfers, SizeRange}
import spinal.lib.misc.{InterruptNode, PathTracer}
import spinal.lib.system.tag.{MemoryEndpoint, PMA, PmaRegion, PmaRegionImpl, VirtualEndpoint}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.decode.{Decode, DecodePipelinePlugin}
import vexiiriscv.execute.{CsrRamPlugin, ExecuteLanePlugin, SrcPlugin}
import vexiiriscv.execute.lsu._
import vexiiriscv.fetch._
import vexiiriscv.misc.PrivilegedPlugin
import vexiiriscv.prediction.BtbPlugin
import vexiiriscv.regfile.RegFilePlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber

import scala.collection.mutable.ArrayBuffer

// Generates VexiiRiscv verilog using command line arguments
object Generate extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()
  val regions = ArrayBuffer[PmaRegion]()
  val analysis = new AnalysisUtils

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    param.addOptions(this)
    analysis.addOption(this)
    ParamSimple.addOptionRegion(this, regions)
  }.parse(args, Unit).nonEmpty)

  if(regions.isEmpty) regions ++= ParamSimple.defaultPma

  val report = sc.generateVerilog {
    val plugins = param.plugins()
    ParamSimple.setPma(plugins, regions)
    VexiiRiscv(plugins)
  }

  analysis.report(report)
}

//Generates a tilelink version of VexiiRiscv verilog using command line arguments
object GenerateTilelink extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()
  val regions = ArrayBuffer[PmaRegion]()
  var tlSinkWidth = 0

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    opt[Int]("tl-sink-width") action { (v, c) => tlSinkWidth = v }
    param.addOptions(this)
    ParamSimple.addOptionRegion(this, regions)
  }.parse(args, Unit).nonEmpty)

  if(regions.isEmpty) regions ++= ParamSimple.defaultPma

  val report = sc.generateVerilog {
    val plugins = param.plugins()
    import spinal.lib.bus.tilelink._
    import spinal.lib.bus.tilelink.fabric._
    new Component {
      setDefinitionName("VexiiRiscvTilelink")
      val cpu = new TilelinkVexiiRiscvFiber(plugins)
      val mem = new SlaveBus(
        M2sSupport(
          transfers = M2sTransfers.all,
          dataWidth = param.memDataWidth,
          addressWidth = param.physicalWidth
        ),
        S2mParameters(
          List(
            S2mAgent(
              name = null,
              sinkId = SizeMapping(0, 1 << tlSinkWidth),
              emits = S2mTransfers(probe = SizeRange(0x40))
            )
          )
        )
      )

      // Custom memory mapping
      val tags = mem.node.spinalTags.filter(!_.isInstanceOf[MemoryEndpoint])
      mem.node.spinalTags.clear()
      mem.node.spinalTags ++= tags
      val virtualRegions = for (region <- regions) yield new VirtualEndpoint(mem.node, region.mapping) {
        if (region.isMain) self.addTag(PMA.MAIN)
        if (region.isExecutable) self.addTag(PMA.EXECUTABLE)
      }

      mem.node << cpu.iBus
      mem.node << cpu.dBus
      if(cpu.lsuL1Bus != null) mem.node << cpu.lsuL1Bus

      // Bind interrupts
      val mti, msi, mei = InterruptNode.master()
      cpu.priv.get.mti << mti; in(mti.flag)
      cpu.priv.get.msi << msi; in(msi.flag)
      cpu.priv.get.mei << mei; in(mei.flag)

      val sei = (cpu.priv.get.sei != null) generate InterruptNode.master()
      if(sei != null) cpu.priv.get.sei << sei; in(sei.flag)

      val patcher = Fiber patch new AreaRoot {
        val hartId = param.withHartIdInput generate plugins.collectFirst{
          case p : PrivilegedPlugin => p.api.harts(0).hartId.toIo
        }

        val memA = mem.node.bus.a
        out(memA.compliantMask()).setName(memA.mask.getName())
        memA.mask.setName(memA.mask.getName()+"_non_compliant")
        memA.mask.setAsDirectionLess()
      }
    }

  }

  for(m <- report.toplevel.mem.node.m2s.parameters.masters){
    println(m.name)
    for(source <- m.mapping){
      println(s"- ${source.id} ${source.emits}")
    }
  }
}