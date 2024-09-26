package vexiiriscv

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib.LatencyAnalysis
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

object Generate extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()
  val regions = ArrayBuffer[PmaRegion]()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    param.addOptions(this)
    ParamSimple.addOptionRegion(this, regions)
  }.parse(args, Unit).nonEmpty)

  if(regions.isEmpty) regions ++= ParamSimple.defaultPma

  val report = sc.generateVerilog {
    val plugins = param.plugins()
    ParamSimple.setPma(plugins, regions)
    VexiiRiscv(plugins)
  }
}


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
  param.withBtb = true
  param.withGShare = true
  param.withRas = true


  val report = sc.generateVerilog {
    val plugins = param.plugins()
    ParamSimple.setPma(plugins)
    VexiiRiscv(plugins)
  }

  report.toplevel.database.on{
    //    val from = report.toplevel.host[ExecuteLanePlugin].execute(0).down(Decode.UOP)
    //    val from = report.toplevel.host[LsuPlugin].logic.storeBuffer.slots(0).tag
//    val from = report.toplevel.host[DecodePipelinePlugin].ctrl(1).lane(0).up(Decode.INSTRUCTION)
//    val to = report.toplevel.host[FetchL1Plugin].logic.banks.head.read.cmd.valid
    val from = report.toplevel.host[BtbPlugin].logic.readPort.rsp.head.hash
    val to = report.toplevel.host[FetchL1Plugin].logic.banks.head.read.cmd.valid
    //    val to = report.toplevel.host[CsrRamPlugin].logic.writeLogic.port.data
    println(PathTracer.impl(from, to).report())
  }
}



/*
- Node((toplevel/FetchL1Plugin_logic_banks_0_read_cmd_valid :  Bool))
  - Node((toplevel/fetch_logic_ctrls_0_up_isReady :  Bool))
    - Node((toplevel/fetch_logic_ctrls_0_up_ready :  Bool))
      - Node((toplevel/when_CtrlLink_l151 :  Bool))
        - Node(| Bits)
          - Node(Bits ## Bits)
            - Node(Bits ## Bits)
              - Node(Bits -> Bits)
                - Node((toplevel/fetch_logic_ctrls_0_haltRequest_BtbPlugin_l165 :  Bool))
                  - Node(Bool && Bool)
                    - Node(UInt === UInt)
                      - Node((toplevel/BtbPlugin_logic_readPort_cmd_payload :  UInt[9 bits]))
                        - Node(resize(UInt,9 bits))
                          - Node((UInt >> Int)[30 bits])
                            - Node((toplevel/fetch_logic_ctrls_0_down_Fetch_WORD_PC :  UInt[32 bits]))
                              - Node((toplevel/fetch_logic_ctrls_0_up_Fetch_WORD_PC :  UInt[32 bits]))
                                - Node((toplevel/PcPlugin_logic_harts_0_output_payload :  UInt[32 bits]))
                                  - Node((toplevel/PcPlugin_logic_harts_0_aggregator_target_1 :  UInt[32 bits]))
                                    - Node((toplevel/PcPlugin_logic_harts_0_aggregator_target :  UInt[32 bits]))
                                      - Node((Bits -> UInt of 32 bits))
                                        - Node((Bits | Bits)[32 bits])
                                          - Node((Bits | Bits)[32 bits])
                                            - Node((Bool ? Bits | Bits)[32 bits])
                                              - Node(Bits(Int))
                                                - Node((toplevel/PcPlugin_logic_harts_0_aggregator_oh :  Bits[4 bits]))
                                                  - Node((toplevel/_zz_PcPlugin_logic_harts_0_aggregator_oh_4 :  Bits[4 bits]))
                                                    - Node(Bool && Bool)
                                                      - Node((toplevel/_zz_PcPlugin_logic_harts_0_aggregator_oh_1 :  Bool))
                                                        - Node(Bits(Int))
                                                          - Node((toplevel/_zz_PcPlugin_logic_harts_0_aggregator_oh :  Bits[4 bits]))
                                                            - Node(Bits ## Bits)
                                                              - Node(Bits ## Bits)
                                                                - Node(Bits -> Bits)
                                                                  - Node((toplevel/PcPlugin_logic_harts_0_aggregator_valids_2 :  Bool))
                                                                    - Node(Bool && Bool)
                                                                      - Node(Bool && Bool)
                                                                        - Node((toplevel/BtbPlugin_logic_pcPort_valid :  Bool))
                                                                          - Node((toplevel/BtbPlugin_logic_applyIt_doIt :  Bool))
                                                                            - Node(Bool && Bool)
                                                                              - Node((toplevel/BtbPlugin_logic_applyIt_needIt :  Bool))
                                                                                - Node(Bool && Bool)
                                                                                  - Node(| Bits)
                                                                                    - Node((toplevel/BtbPlugin_logic_applyIt_chunksTakenOh :  Bits[1 bits]))
                                                                                      - Node((Bits & Bits)[1 bits])
                                                                                        - Node((toplevel/BtbPlugin_logic_applyIt_chunksMask :  Bits[1 bits]))
                                                                                          - Node(Bits -> Bits)
                                                                                            - Node(Bool && Bool)
                                                                                              - Node((toplevel/fetch_logic_ctrls_1_down_BtbPlugin_logic_chunksLogic_0_hitCalc_HIT :  Bool))
                                                                                                - Node(Bool && Bool)
                                                                                                  - Node(UInt === UInt)
                                                                                                    - Node((toplevel/fetch_logic_ctrls_1_down_BtbPlugin_logic_chunksLogic_0_readRsp_ENTRY_hash :  UInt[16 bits]))
                                                                                                      - Node((toplevel/BtbPlugin_logic_readPort_rsp_0_hash :  UInt[16 bits]))
                                                                                                                                                                        - Node((toplevel/BtbPlugin_logic_readPort_rsp_0_hash :  UInt[16 bits]))
 */