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
import vexiiriscv.prediction.BtbPlugin
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