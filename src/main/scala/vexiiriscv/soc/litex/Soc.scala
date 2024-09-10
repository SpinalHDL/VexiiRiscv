package vexiiriscv.soc.litex

import spinal.core.fiber.{Fiber, hardFork}
import spinal.core._
import spinal.core.blackboxByteEnables.generateUnblackboxableError
import spinal.core.internals.MemTopology
import spinal.core.sim.SimDataPimper
import spinal.lib.bus.amba4.axi.sim.{Axi4ReadOnlyMonitor, Axi4ReadOnlySlaveAgent, Axi4WriteOnlyMonitor, Axi4WriteOnlySlaveAgent}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer, Axi4ToTilelinkFiber}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.amba4.axilite.sim.{AxiLite4ReadOnlySlaveAgent, AxiLite4WriteOnlySlaveAgent}
import spinal.lib.bus.misc.args.{PeriphSpecs, PeriphTilelinkFiber}
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber, SelfFLush}
import spinal.lib.bus.tilelink.{coherent, fabric}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.cpu.riscv.debug.DebugModuleFiber
import spinal.lib.eda.bench.{Bench, Rtl}
import spinal.lib.graphic.YcbcrConfig
import spinal.lib.graphic.vga.{TilelinkVgaCtrlFiber, TilelinkVgaCtrlSpec, Vga, VgaRgbToYcbcr, VgaYcbcrPix2}
import spinal.lib.misc.{PathTracer, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.sim.SparseMemory
import spinal.lib.{AnalysisUtils, Delay, Flow, ResetCtrlFiber, StreamPipe, master, memPimped, slave}
import spinal.lib.system.tag.{MemoryConnection, MemoryEndpoint, MemoryEndpointTag, MemoryTransferTag, MemoryTransfers, PMA, VirtualEndpoint}
import vexiiriscv.ParamSimple
import vexiiriscv.compat.{EnforceSyncRamPhase, MultiPortWritesSymplifier}
import vexiiriscv.fetch.FetchL1Plugin
import vexiiriscv.prediction.GSharePlugin
import vexiiriscv.schedule.DispatchPlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.soc.demo.DebugModuleSocFiber

import java.awt.{Dimension, Graphics}
import java.awt.image.BufferedImage
import javax.swing.{JFrame, JPanel, WindowConstants}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class LitexMemoryRegion(mapping : SizeMapping, mode : String, bus : String){
  def isExecutable = mode.contains("x")
  def isCachable = mode.contains("c")
  def onPeripheral = bus match {
    case "m" => false
    case "p" => true
  }
  def onMemory = !onPeripheral
}

class SocConfig(){
  var vexiiParam = new ParamSimple()
  val regions = ArrayBuffer[LitexMemoryRegion]()
  var withJtagTap = false
  var withJtagInstruction = false
  def withDebug = withJtagInstruction || withJtagTap
  var withDma = false
  var mBusWidth = 64
  var l2Bytes = 0
  var l2Ways = 0
  var cpuCount = 1
  var litedramWidth = 32
  var withAxi3 = false
  var withPeripheralCd = false
  var selfFlush : SelfFLush = null
  val periph = new PeriphSpecs
  val video = ArrayBuffer[TilelinkVgaCtrlSpec]()
  var withCpuCd = false

  def addOptions(parser: scopt.OptionParser[Unit]): Unit = {
    import parser._
    vexiiParam.addOptions(parser)
    periph.addOptions(parser)
    TilelinkVgaCtrlSpec.addOption(parser, video)
    opt[Int]("litedram-width") action { (v, c) => litedramWidth = v }
    //    opt[Seq[String]]("l2-self-flush") action { (v, c) => selfFlush = coherent.SelfFLush(BigInt(v(0)), BigInt(v(1)), BigInt(v(2))) }
    opt[Seq[String]]("l2-self-flush") action { (v, c) =>
      selfFlush = coherent.SelfFLush(BigInt(v(0), 16), BigInt(v(1), 16), BigInt(v(2)))
    }
    opt[Int]("cpu-count") action { (v, c) => cpuCount = v }
    opt[Int]("l2-bytes") action { (v, c) => l2Bytes = v }
    opt[Int]("l2-ways") action { (v, c) => l2Ways = v }
    opt[Unit]("with-dma") action { (v, c) => withDma = true }
    opt[Unit]("with-cpu-clk") action { (v, c) => withCpuCd = true }
    opt[Unit]("with-axi3") action { (v, c) => withAxi3 = true }
    opt[Unit]("with-jtag-tap") action { (v, c) => withJtagTap = true; vexiiParam.privParam.withDebug = true }
    opt[Unit]("with-jtag-instruction") action { (v, c) => withJtagInstruction = true; vexiiParam.privParam.withDebug = true }
    opt[Seq[String]]("memory-region") unbounded() action { (v, c) =>
      assert(v.length == 4, "--memory-region need 4 parameters")
      val r = new LitexMemoryRegion(SizeMapping(BigInt(v(0)), BigInt(v(1))), v(2), v(3))
      regions += r
      assert(!(r.onMemory && !r.isCachable), s"Region $r isn't supported by VexiiRiscv, data cache will always cache memory")
    }
  }

  def withL2 = l2Bytes > 0
}

class Soc(c : SocConfig) extends Component {

  import c._

  val litexCd = ClockDomain.external("litex")
  val cpuClk = withCpuCd.mux(ClockDomain.external("cpu", withReset = false), litexCd)
  val cpuResetCtrl = cpuClk(new ResetCtrlFiber())
  cpuResetCtrl.addAsyncReset(litexCd.isResetActive, HIGH)
  val cpuCd = cpuResetCtrl.cd


  val system = cpuCd on new AreaRoot {
    val mainDataWidth = vexiiParam.memDataWidth
    val withCoherency = vexiiParam.lsuL1Coherency
    val vexiis = for (hartId <- 0 until cpuCount) yield new TilelinkVexiiRiscvFiber(vexiiParam.plugins(hartId))
    for (vexii <- vexiis) {
      if (vexiiParam.fetchL1Enable) vexii.iBus.setDownConnection { (down, up) =>
        down.a << up.a.halfPipe().halfPipe()
        up.d << down.d.m2sPipe()
      }
      if (vexiiParam.lsuL1Enable) {
        vexii.lsuL1Bus.setDownConnection(a = withCoherency.mux(StreamPipe.HALF, StreamPipe.FULL), b = StreamPipe.HALF_KEEP, c = StreamPipe.FULL, d = StreamPipe.M2S_KEEP, e = StreamPipe.HALF)
        vexii.dBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S_KEEP)
      }
    }

    val ioBus = fabric.Node()

    val memRegions = regions.filter(e => e.onMemory && e.isCachable)

    val withMem = memRegions.nonEmpty
    val mem = withMem generate new Area {
      val toAxi4 = withMem generate new fabric.Axi4Bridge(withAxi3 = withAxi3)
      toAxi4.up.forceDataWidth(litedramWidth)
      toAxi4.down.addTag(PMA.MAIN)
      toAxi4.down.addTag(PMA.EXECUTABLE)
      for (region <- memRegions) {
        toAxi4.down.addTag(new MemoryEndpointTag(region.mapping))
      }
    }

    val video = for (spec <- c.video) yield new Area {
      setName(spec.name)
      val cd = ClockDomain.external(spec.name, withReset = false)
      val resetCtrl = cd(new ResetCtrlFiber())
      resetCtrl.addAsyncReset(ClockDomain.current.isResetActive, HIGH)
      spec.param.dmaParam.dataWidth = mainDataWidth
      val ctrl = new TilelinkVgaCtrlFiber(spec.param, resetCtrl.cd)

      val phy = Fiber build new ClockingArea(resetCtrl.cd) {
        setName(spec.name)
        val vga = ctrl.logic.ctrl.io.vga
        vga.simPublic()

        val ycbcrConfig = YcbcrConfig(8, 8, 8)
        val toYcbcr = new VgaRgbToYcbcr(spec.param.rgbConfig, ycbcrConfig)
        toYcbcr.io.up <> vga

        val pix2 = new VgaYcbcrPix2(ycbcrConfig)
        pix2.io.up <> toYcbcr.io.down

        val vSync, hSync, colorEn = out(Bool())
        val color = out Bits (16 bits)
        vSync := pix2.io.down.vSync
        hSync := pix2.io.down.hSync
        colorEn := pix2.io.down.colorEn
        color := pix2.io.down.color.asBits
      }
    }

    val withLowLatencyPeriph = !withMem
    val peripheral = new ClockingArea(litexCd) {
      val bus = Node()
      bus << ioBus
      if (vexiiParam.lsuL1Enable && !withLowLatencyPeriph) bus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)
      bus.forceDataWidth(32)

      val clint = new TilelinkClintFiber()
      clint.node at 0xF0010000l of bus

      val plic = new TilelinkPlicFiber()
      plic.node at 0xF0C00000l of bus

      val externalInterrupts = new Area {
        val port = in Bits (32 bits)
        val toPlic = for (i <- 0 to 31) yield (i != 0) generate new Area {
          val node = plic.createInterruptSlave(i)
          node.withUps = false
          node.flag := port(i)
        }
      }

      val fromArgs = new PeriphTilelinkFiber(periph, bus, plic)

      for (vexii <- vexiis) {
        vexii.bind(clint)
        vexii.bind(plic)
      }

      val toAxiLite4 = new fabric.AxiLite4Bridge
      toAxiLite4.up << bus

      val axiLiteRegions = regions.filter(e => e.onPeripheral)
      val virtualRegions = for (region <- axiLiteRegions) yield new VirtualEndpoint(toAxiLite4.down, region.mapping) {
        if (region.isCachable) self.addTag(PMA.MAIN)
        if (region.isExecutable) self.addTag(PMA.EXECUTABLE)
      }
    }

    val dma = c.withDma generate new ClockingArea(litexCd){
      val bus = slave(
        Axi4(
          Axi4Config(
            addressWidth = 32,
            dataWidth = mainDataWidth,
            idWidth = 4
          )
        )
      )

      val bridge = new Axi4ToTilelinkFiber(64, 4)
      bridge.up load bus.pipelined(ar = StreamPipe.HALF, aw = StreamPipe.HALF, w = StreamPipe.FULL, b = StreamPipe.HALF, r = StreamPipe.FULL)

      val filter = new fabric.TransferFilter()
      filter.up << bridge.down

      //As litex reset will release before our one, we need to ensure that we don't eat a transaction
      Fiber build {
        bridge.read.get
        bridge.write.get
        when(ClockDomain.current.isResetActive) {
          bus.ar.ready := False
          bus.aw.ready := False
          bus.w.ready := False
        }
      }
    }

    val splited = vexiiParam.lsuL1Enable generate new Area{
      val mBus = Node()
      mBus.forceDataWidth(mainDataWidth)

      ioBus.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.NONE)
      if(withMem) mem.toAxi4.up << mBus
      peripheral.bus << mBus

      val nc = !withCoherency generate new Area {
        for (vexii <- vexiis) {
          mBus << List(vexii.iBus, vexii.lsuL1Bus)
          ioBus << List(vexii.dBus)
        }
      }

      val wc = withCoherency generate new Area {
        val cBus = fabric.Node()

        for (vexii <- vexiis) {
          cBus << List(vexii.iBus, vexii.lsuL1Bus)
          ioBus << List(vexii.dBus)
        }

        for (video <- video) cBus << video.ctrl.dma

        if(withDma) {
          (cBus << dma.filter.down).setDownConnection(a = StreamPipe.FULL)
        }

        val hub = (withCoherency && !withL2) generate new Area {
          val hub = new HubFiber()
          hub.up << cBus
          hub.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL)
          hub.down.forceDataWidth(mainDataWidth)
          mBus << hub.down
        }

        val l2 = (withCoherency && withL2) generate new Area {
          val cache = new CacheFiber(withCtrl = false)
          cache.parameter.cacheWays = l2Ways
          cache.parameter.cacheBytes = l2Bytes
          cache.parameter.selfFlush = selfFlush
          cache.parameter.readProcessAt = 2+(l2Bytes >= 512*1024).toInt
//          cache.parameter.generalSlotCount = 12
//          cache.parameter.downPendingMax = 8

          cache.up << cBus
          cache.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL, d = StreamPipe.FULL)
          cache.down.setDownConnection(d = StreamPipe.S2M)
          cache.down.forceDataWidth(mainDataWidth)
          mBus << cache.down
        }

        assert(!(!withCoherency && withL2))
      }
    }


    val patcher = Fiber build new AreaRoot {
      val mBusAxi = withMem generate mem.toAxi4.down.expendId(8)
      val mBus = withMem generate Axi4SpecRenamer(master(
        mBusAxi.pipelined(ar = StreamPipe.FULL, aw = StreamPipe.FULL, w = StreamPipe.FULL, b = StreamPipe.FULL, r = StreamPipe.FULL) //(ar = StreamPipe.FULL, aw = StreamPipe.FULL, w = StreamPipe.FULL, b = StreamPipe.FULL, r = StreamPipe.FULL).pipelined(ar = StreamPipe.FULL, aw = StreamPipe.FULL, w = StreamPipe.FULL, b = StreamPipe.FULL, r = StreamPipe.FULL)
      ))

      val pBus = litexCd on AxiLite4SpecRenamer(master(
        (vexiiParam.lsuL1Enable && !withLowLatencyPeriph).mux(
          peripheral.toAxiLite4.down.pipelined(
            ar = StreamPipe.HALF, aw = StreamPipe.HALF, w = StreamPipe.HALF, b = StreamPipe.HALF, r = StreamPipe.HALF
          ),
          peripheral.toAxiLite4.down.pipelined(),
        )
      ))
      if (c.withDma) Axi4SpecRenamer(dma.bus)

      if(withCoherency && withL2){
        for (bank <- splited.wc.l2.cache.logic.cache.cache.data.banks) {
          bank.ram.preventAsBlackBox() // Some synthesis tools have issues inferring efficient layout when byte mask is used.
        }
      }


      println(MemoryConnection.getMemoryTransfers(vexiis(0).dBus))

//      def debug(that: Data) : Unit = that.addAttribute("mark_debug", "true")
//      def debug[T <: Data](that: spinal.lib.Stream[T]) : Unit = {
//        debug(that.valid)
//        debug(that.ready)
//      }
//      def debug(that: tilelink.Bus) : Unit = {
//        debug(that.a)
//        debug(that.d)
//        if(that.p.withBCE){
//          debug(that.b)
//          debug(that.c)
//          debug(that.e)
//        }
//      }
//      debug(splited.wc.l2.cache.up.bus)
//      debug(splited.wc.l2.cache.up.bus.a.address)
//      debug(splited.wc.l2.cache.down.bus)
//      for(v <- vexiis){
//        debug(v.iBus.bus)
//        debug(v.lsuL1Bus.bus)
//        debug(v.dBus.bus)
//        v.logic.core.host.services.foreach{
//          case p : FetchL1Plugin => {
//            debug(p.logic.events.get.access)
//            debug(p.logic.events.get.miss)
//            debug(p.logic.events.get.waiting)
//            debug(p.logic.refill.onRsp.holdHarts)
//            p.logic.refill.slots.foreach(s => debug(s.valid))
//          }
//          case p: DispatchPlugin => {
//            debug(p.logic.events.get.frontendStall)
//            debug(p.logic.events.get.backendStall)
//          }
//          case _ =>
//        }
//      }
    }
  }

  val debugReset = c.withDebug generate in.Bool()
  val debug = c.withDebug generate ClockDomain(cpuCd.clock, debugReset)(new DebugModuleSocFiber(withJtagInstruction) {
    out(dm.ndmreset)
    system.vexiis.foreach(bindHart)
  })
}


object blackboxPolicy extends MemBlackboxingPolicy{
  override def translationInterest(topology: MemTopology): Boolean = {
    if(topology.writes.exists(e => e.mask != null && e.getSymbolWidth == 8) && topology.mem.initialContent == null) return true
    if (topology.readWriteSync.exists(e => e.mask != null && e.getSymbolWidth == 8) && topology.mem.initialContent == null) return true
    if (topology.readsAsync.size != 0 && topology.mem.initialContent == null) return true
    false
  }

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = generateUnblackboxableError(topology, who, message)
}

object SocGen extends App{
  var netlistDirectory = "."
  var netlistName = "VexiiRiscvLitex"
  val socConfig = new SocConfig()
  var reducedIo = false
  import socConfig._

//  vexiiParam.fetchL1Enable = true
//  vexiiParam.lsuL1Enable = true
  vexiiParam.privParam.withRdTime = true

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    socConfig.addOptions(this)
    opt[String]("netlist-directory") action { (v, c) => netlistDirectory = v }
    opt[String]("netlist-name") action { (v, c) => netlistName = v }
    opt[Unit]("reduced-io") action { (v, c) => reducedIo = true }
  }.parse(args, Unit).nonEmpty)

  vexiiParam.lsuL1Coherency = cpuCount > 1 || withDma

  val spinalConfig = SpinalConfig(inlineRom = true, targetDirectory = netlistDirectory)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
  spinalConfig.addStandardMemBlackboxing(blackboxPolicy)
//  spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

  val report = spinalConfig.generateVerilog {
    val soc = new Soc(socConfig).setDefinitionName(netlistName)
    if(reducedIo) Fiber patch{
      Rtl.xorOutputs(soc, soc.litexCd)
      Rtl.compactInputs(soc, soc.litexCd)
    }
    soc
  }

  val cpu0 = report.toplevel.system.vexiis(0).logic.core
//  val cpu2 = report.toplevel.system.vexiis(2).logic.core
//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl3_up_LsuL1_PHYSICAL_ADDRESS_lane0")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0")

//  val from = cpu0.reflectBaseType("DispatchPlugin_logic_slots_0_ctx_uop")
//  val from = cpu0.reflectBaseType("DispatchPlugin_logic_slots_0_ctx_hm_RS3_PHYS")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl0_down_float_RS3_lane0")

//  val from = cpu0.reflectBaseType("LsuL1Plugin_logic_c_pip_ctrl_2_up_LsuL1_PHYSICAL_ADDRESS") // start point was optimized, but aligner timing issue remains
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_Decode_UOP_lane0")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0")

//  val from = cpu0.reflectBaseType("fetch_logic_ctrls_2_down_valid")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl3_up_LsuL1_PHYSICAL_ADDRESS_lane0")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_integer_RS2_lane0")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0")
//  val to = cpu0.reflectBaseType("FpuPackerPlugin_logic_pip_node_1_s0_EXP_DIF_PLUS_ONE")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0")
//  val to = cpu0.host[GSharePlugin].logic.onLearn.cmd.valid

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0")
//  val to = cpu0.reflectBaseType("LsuL1Plugin_logic_writeback_slots_1_timer_counter")

//  val from = report.toplevel.reflectBaseType("vexiis_0_lsuL1Bus_noDecoder_toDown_d_rData_opcode")   <---- TODO fix this path
//  val to = cpu0.reflectBaseType("LsuL1Plugin_logic_c_pip_ctrl_2_up_onPreCtrl_WB_HAZARD")
//
//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0")   <---- TODO fix this path
//  val to = cpu0.reflectBaseType("LsuL1Plugin_logic_banks_1_write_valid")

//  val from = cpu0.reflectBaseType("early0_DivPlugin_logic_processing_divRevertResult")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl2_up_early0_SrcPlugin_SRC1_lane0")

//  val from = cpu0.reflectBaseType("_zz_vexiis_0_logic_core_toplevel_execute_ctrl2_down_FpuUnpack_RS3_badBoxing_HIT_lane0")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl3_up_FpuF2iPlugin_logic_onSetup_SHIFTED_PARTIAL_lane0")

//  val from = cpu0.reflectBaseType("LsuL1Plugin_logic_c_pip_ctrl_2_up_FORCE_HAZARD") //That big
//  val to = cpu0.reflectBaseType("FpuCsrPlugin_api_flags_OF")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl4_up_LsuPlugin_logic_onPma_IO_lane0")
//  val to = cpu0.reflectBaseType("CsrAccessPlugin_bus_write_halt")
//  val to = cpu0.reflectBaseType("FpuCsrPlugin_api_flags_NX")



//  val drivers = mutable.LinkedHashSet[BaseType]()
//  AnalysisUtils.seekNonCombDrivers(to){driver =>
//    driver match {
//      case bt : BaseType => drivers += bt
//    }
//  }
//  drivers.foreach(e => println(e.getName()))
//  println("******")
//  println(PathTracer.impl(from, to).report())
}

object PythonArgsGen extends App{
  val vexiiParam = new ParamSimple()
  import vexiiParam._
  var pythonPath ="miaou.py"
  assert(new scopt.OptionParser[Unit]("Vexii") {
    help("help").text("prints this usage text")
    vexiiParam.addOptions(this)
    opt[String]("python-file") action { (v, c) => pythonPath = v }

  }.parse(args, Unit).nonEmpty)

  import java.io.PrintWriter

  new PrintWriter(pythonPath) {
    write(
      s"""
         |VexiiRiscv.xlen = $xlen
         |VexiiRiscv.with_rvm = ${(withMul && withDiv).toInt}
         |VexiiRiscv.with_rva = ${withRva.toInt}
         |VexiiRiscv.with_rvf = ${withRvf.toInt}
         |VexiiRiscv.with_rvd = ${withRvd.toInt}
         |VexiiRiscv.with_rvc = ${withRvc.toInt}
         |VexiiRiscv.with_lsu_software_prefetch = ${lsuSoftwarePrefetch.toInt}
         |VexiiRiscv.with_lsu_hardware_prefetch = "${lsuHardwarePrefetch}"
         |VexiiRiscv.internal_bus_width = ${memDataWidth}
         |""".stripMargin)
    close()
  }

}

object VgaDisplaySim{
  import spinal.core.sim._
  def apply(vga : Vga, cd : ClockDomain): Unit ={

    var width = 800
    var height = 600
    var scale = 1
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_BGR);

    val frame = new JFrame{
      setPreferredSize(new Dimension(800, 600));

      add(new JPanel{
        this.setPreferredSize(new Dimension(width, height))
        override def paintComponent(g : Graphics) : Unit = {
          g.drawImage(image, 0, 0, width*scale,height*scale, null)
        }
      })

      pack();
      setVisible(true);
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    }

    var overflow = false
    var x,y = 0
    cd.onSamplings{
      val vsync = vga.vSync.toBoolean
      val hsync = vga.hSync.toBoolean
      val colorEn = vga.colorEn.toBoolean
      if(colorEn) {
//        val color = vga.color.r.toInt << (16 + 8 - vga.rgbConfig.rWidth) | vga.color.g.toInt << (8 + 8 - vga.rgbConfig.gWidth) | vga.color.b.toInt << (0 + 8 - vga.rgbConfig.bWidth)
        val color = vga.color.r.toInt << (16 + 8 - vga.rgbConfig.rWidth) | vga.color.g.toInt << (8 + 8 - vga.rgbConfig.gWidth) | vga.color.b.toInt << (0 + 8 - vga.rgbConfig.bWidth)
        if(x < width && y < height) {
          image.setRGB(x, y, color)
        }
        x+=1
      }
      if(!vsync){
        y = 0
//        for(y <- 0 until height; x <- 0 until width) image.setRGB(x,y,0)
      }
      if(!hsync){
        if(x != 0){
          y+=1
          frame.repaint()
        }
        x = 0
      }
    }
  }
}

object SocSim extends App{
  val socConfig = new SocConfig()
  import socConfig._

  //  vexiiParam.fetchL1Enable = true
  //  vexiiParam.lsuL1Enable = true
  vexiiParam.privParam.withRdTime = true

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    socConfig.addOptions(this)
  }.parse(args, Unit).nonEmpty)

  vexiiParam.lsuL1Coherency = vexiiParam.lsuL1Coherency || cpuCount > 1 || withDma

  val spinalConfig = SpinalConfig()
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

  import spinal.core.sim._
  SimConfig.withConfig(spinalConfig).withFstWave.compile(new Soc(socConfig)).doSimUntilVoid(seed=32){dut =>
    dut.litexCd.forkStimulus(10000)
    if(socConfig.withCpuCd) dut.cpuClk.forkStimulus(5000)
    for(video <- dut.system.video){
      video.cd.forkStimulus(20000)
      VgaDisplaySim(video.ctrl.logic.ctrl.io.vga, video.resetCtrl.cd)
    }
    if(dut.debugReset != null)fork{
      dut.debugReset #= true
      dut.cpuCd.waitRisingEdge(2)
      dut.debugReset #= false
    }
    sleep(1)


    val onPbus = new Area {
      val axi = dut.system.patcher.pBus
      new AxiLite4ReadOnlySlaveAgent(axi.ar, axi.r, dut.litexCd){

      }
      val woa = new AxiLite4WriteOnlySlaveAgent(axi.aw, axi.w, axi.b, dut.litexCd) {

      }
    }

    val onAxi = new Area{
      val ddrMemory = SparseMemory()
      val axi = dut.system.patcher.mBus
      val woa = new Axi4WriteOnlySlaveAgent(axi.aw, axi.w, axi.b, dut.cpuCd) {
        awDriver.factor = 0.9f
        wDriver.factor = 0.9f
        bDriver.transactionDelay = () => 1
      }
      var bytesAccess = 0l
      new Axi4WriteOnlyMonitor(axi.aw, axi.w, axi.b, dut.cpuCd) {
        override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = ddrMemory.write(address.toLong, data)
        override def onWriteStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {
          bytesAccess += len * (1 << size)
        }
      }
      val roa = new Axi4ReadOnlySlaveAgent(axi.ar, axi.r, dut.cpuCd, withReadInterleaveInBurst = false) {
        arDriver.factor = 0.8f
        rDriver.transactionDelay = () => simRandom.nextInt(3)
        baseLatency = 60 * 1000

//        periodicaly(300*1000*1000){
//          val value = ((60 + simRandom.nextInt(3000))*1000)
//          println(s"RANDOM $value")
//          baseLatency = value
//
////          val value = simRandom.nextFloat() * 0.8f
////          println(s"RANDOM $value")
////          rDriver.setFactor(value)
//        }

        override def readByte(address: BigInt): Byte = {
          bytesAccess += 1
          ddrMemory.read(address.toLong)
        }
      }
    }
    for(y <- 0 until 600; x <- 0 until 800){
      val color = (x & 0xFF) + ((y & 0xFF) << 8)// + (((x+y) & 0xFF) << 16)
      onAxi.ddrMemory.write(0x40c00000 + x * 4 + y * 4 * 800, color)
//      val color = (x & 0x1F)+((y & 0x3F) << 5)
//      onAxi.ddrMemory.write(0x40c00000 + x*2+y*2*640, color + (color << 16))
    }

    var cnt = 0
    def setPix(value : Int) = {
      onAxi.ddrMemory.write(0x40c00000 + cnt, value)
      onAxi.ddrMemory.write(0x40c00000 + cnt + 4, value)
      cnt += 8
    }
    setPix(0x00000000)
    setPix(0x000000ff)
    setPix(0x0000ff00)
    setPix(0x00ff0000)
    setPix(0x00ffffff)
    onAxi.ddrMemory.write(0x40c00000 + cnt, 0x000000FF); cnt += 4
    onAxi.ddrMemory.write(0x40c00000 + cnt, 0x00800080); cnt += 4

  }
}
/*
report_path -to  VexiiRiscvLitex_44a81283d17b029c539716862d04b1a0/vexiis_3_iBus_bus_a_rValid~FF|CE -nworst 100

make CROSS_COMPILE=riscv-none-embed-      PLATFORM=generic      PLATFORM_RISCV_XLEN=64      PLATFORM_RISCV_ISA=rv64gc      PLATFORM_RISCV_ABI=lp64d      FW_FDT_PATH=../linux.dtb      FW_JUMP_ADDR=0x41000000       FW_JUMP_FDT_ADDR=0x46000000      -j20
scp build/platform/generic/firmware/fw_jump.bin root@nexys.local:/boot/opensbi.bin

# debian 4c
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv --cpu-variant=debian --with-jtag-tap  --bus-standard axi-lite \
--vexii-args="--lsu-software-prefetch --lsu-hardware-prefetch rpt --performance-counters 9 --regfile-async --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 4 --lsu-l1-writeback-count 4 --lsu-l1-store-buffer-slots=4" \
--cpu-count=4 --with-jtag-tap  --with-video-framebuffer --l2-self-flush=40c00000,40dd4c00,1666666  --with-sdcard --with-ethernet --with-coherent-dma --l2-byte=262144  --sys-clk-freq 100000000 \
--update-repo=no --soc-json build/csr.json --build   --load

# debian 4c perf
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv --cpu-variant=debian --with-jtag-tap  --bus-standard axi-lite \
--vexii-args="--fetch-l1-hardware-prefetch=nl --fetch-l1-refill-count=2 --fetch-l1-mem-data-width-min=128 --lsu-l1-mem-data-width-min=128 --lsu-software-prefetch --lsu-hardware-prefetch rpt --performance-counters 9 --regfile-async --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 4 --lsu-l1-writeback-count 4 --lsu-l1-store-buffer-slots=4" \
--cpu-count=4 --with-jtag-tap  --with-video-framebuffer --l2-self-flush=40c00000,40dd4c00,1666666  --with-sdcard --with-ethernet --with-coherent-dma --l2-byte=262144  --sys-clk-freq 100000000 \
--update-repo=no --soc-json build/csr.json --build   --load

--vivado-synth-directive=performanceoptimized --vivado-route-directive=aggressiveexplore
 --fetch-l1-mem-data-width-min=128 --lsu-l1-mem-data-width-min=128
 --fetch-l1-hardware-prefetch=nl --fetch-l1-refill-count=2
 --lsu-software-prefetch --lsu-hardware-prefetch rpt


python3 -m litex.tools.litex_json2dts_linux build/csr.json --root-device=mmcblk0p2 > build/linux.dts
dtc -o dtb -o build/linux.dtb build/linux.dts

vex 1 =>
memspeed at 0x40000000 (sequential, 8.0kib)...
  write speed: 1.6mib/s
   read speed: 867.6kib/s


write speed: 647.4kib/s
 read speed: 689.3kib/s

write speed: 811.9kib/s
 read speed: 833.5kib/s

write speed: 1.3mib/s
 read speed: 833.5kib/s

write speed: 1.3mib/s
 read speed: 1.0mib/s


python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged" --with-jtag-tap --build --load
--with-video-framebuffer --with-spi-sdcard --with-ethernet

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --with-mul --with-div --with-rva --with-btb --with-ras --with-gshare --fetch-l1 --fetch-l1-sets=64 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-sets=64 --lsu-l1-ways=4 --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-supervisor --with-user --performance-counters 0 --debug-privileged --debug-triggers=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64" --with-jtag-tap --trace-fst --with-jtagremote


litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --with-mul --with-div --with-rva --with-btb --with-ras --with-gshare --fetch-l1-sets=64 --fetch-l1-ways=4 --lsu-l1-sets=64 --lsu-l1-ways=4 --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-supervisor --with-user --performance-counters 0" --trace-fst --sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json --trace-start 120000000000000 --trace-end 122000000000000 --trace

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --with-mul --with-div --with-rva --with-btb --with-ras --with-gshare --fetch-l1-sets=64 --fetch-l1-ways=4 --lsu-l1-sets=64 --lsu-l1-ways=4 --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-supervisor --with-user --performance-counters 0" --trace-fst --sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json --trace-start 120000000000000 --trace-end 122000000000000 --trace

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64  \
--vexii-args=" \
--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 \
--regfile-async --xlen=64 --with-rvc --with-rvf --with-rvd \
--fetch-l1 --fetch-l1-ways=4 --fetch-l1-mem-data-width-min=64 \
--lsu-l1 --lsu-l1-ways=4  --lsu-l1-mem-data-width-min=64 --lsu-l1-refill-count 1 --lsu-l1-writeback-count 1 --with-lsu-bypass \
--relaxed-branch"  --cpu-count=2 --with-jtag-tap  --with-jtagremote  --sdram-init boot.json

--sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json

--decoders=2 --lanes=2 --with-dispatcher-buffer"

--trace
--trace-start 600000000000
60000000000000
--sdram-init images/sim.json


/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl -c "load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rootfs.cpio 0x40000000" -c exit
(* mark_debug = "true" *)

// minimal linux
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --with-rva --with-supervisor --performance-counters 0" --with-jtag-tap  --load
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/image 0x40000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rv32.dtb 0x40ef0000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rootfs.cpio 0x41000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/opensbi.bin 0x40f00000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000
resume

//linux++ single issue 1.77
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --with-jtag-tap  --build --load

//linux++ dual issue
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass --decoders=2 --lanes=2" --with-jtag-tap  --build --load

//linux++ 64 bits fpu
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --with-jtag-tap  --bus-standard axi-lite --vexii-args=" \
--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 \
--regfile-async --xlen=64 --with-rvc --with-rvf --with-rvd --fma-reduced-accuracy \
--fetch-l1 --fetch-l1-ways=4 --fetch-l1-mem-data-width-min=64 \
--lsu-l1 --lsu-l1-ways=4  --lsu-l1-mem-data-width-min=64 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --lsu-l1-store-buffer-slots=2  --with-lsu-bypass \
--with-btb --with-ras --with-gshare --relaxed-branch --relaxed-btb"  --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma --l2-byte=131072 --update-repo=no  --sys-clk-freq 100000000 --build   --load

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --with-jtag-tap  --bus-standard axi-lite --vexii-args=" \
--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 9 \
--regfile-async --xlen=64 --with-rvc --with-rvf --with-rvd --fma-reduced-accuracy \
--fetch-l1 --fetch-l1-ways=4 --fetch-l1-mem-data-width-min=64 \
--lsu-l1 --lsu-l1-ways=4  --lsu-l1-mem-data-width-min=64 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --lsu-l1-store-buffer-slots=2  --with-lsu-bypass \
--with-btb --with-ras --with-gshare --relaxed-branch --relaxed-btb"  --cpu-count=4 --with-jtag-tap  --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma --l2-byte=262144 --update-repo=no  --sys-clk-freq 100000000 --build   --load


debian boot :
- 1c => 164s log
- 4c => 77s log

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--debug-privileged" --with-jtag-tap --build --load
export hart_count=2
/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--debug-privileged" --with-jtag-instruction --build --load
openocd -f digilent_nexys_video.tcl -f vexiiriscv_jtag_tunneled.tcl


python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4  --with-btb --with-ras --with-gshare" --with-jtag-tap  --load



opensbi =>
git clone https://github.com/dolu1990/opensbi.git --branch vexii-debian
cd opensbi
make platform_riscv_xlen=64 platform_riscv_abi=lp64d platform_riscv_isa=rv64gc cross_compile=riscv-none-embed- platform=litex/vexriscv

git clone https://github.com/dolu1990/opensbi.git --branch upstream
cd opensbi
make cross_compile=riscv-none-embed- \
     platform=generic \
     fw_fdt_path=../linux.dtb \
     fw_jump_addr=0x41000000  \
     fw_jump_fdt_addr=0x46000000 \
     -j20

arm semihosting enable

//linux ++ dual core
make O=build/full  BR2_EXTERNAL=../config litex_vexriscv_full_defconfig
(cd build/full/ && make -j20)

litex_sim --cpu-type=vexiiriscv  --cpu-variant=linux --with-sdram --sdram-data-width=64 --bus-standard axi-lite --cpu-count=1  --with-jtag-tap --sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/boot.json
python3 -m litex_boards.targets.digilent_nexys_video --soc-json build/digilent_nexys_video/csr.json --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-spi-sdcard --with-ethernet  --build --load
python3 -m litex_boards.targets.digilent_nexys_video --soc-json build/digilent_nexys_video/csr.json --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma --l2-bytes=131072 --load
--lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32

export hart_count=2
/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl -f dev.tcl

load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/image 0x40000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/linux_2c.dtb 0x40ef0000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/rootfs.cpio 0x41000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000
targets riscv.cpu.1; resume
targets riscv.cpu.0; resume

boot 0x40f00000

udhcpc
cat >> /etc/x11/xorg.conf << eof
> section "module"
>   load "fb"
>   load "shadow"
>   load "fbdevhw"
> endsection
> eof

export SDL_VIDEODRIVER=directfb
export SDL_DIRECTFB_X11_CHECK=0

https://github.com/zielmicha/sdl2/blob/master/readme.directfb
https://bbs.archlinux.org/viewtopic.php?id=243100

--dfb:system=fbdev

systemctl disable lightdm.service
weston --backend=drm-backend.so --use-pixman
weston --use-pixman

https://www.brendangregg.com/perf.html
perf stat  md5sum /home/miaou/readonly/mp3/01-long_distance_calling-metulsky_curse_revisited.mp3
perf record md5sum /home/miaou/readonly/mp3/01-long_distance_calling-metulsky_curse_revisited.mp3
perf record -g  -e cpu-clock  md5sum /home/miaou/readonly/mp3/01-long_distance_calling-metulsky_curse_revisited.mp

perf report
perf report --stdio

video capture => qv4l2

openssl speed -provider legacy -provider default aes-128-cbc

pmu hardware doesn't support sampling/overflow-interrupts

branch-instructions or branches                    [hardware event]
branch-misses                                      [hardware event]
bus-cycles                                         [hardware event]
cache-misses                                       [hardware event]
cache-references                                   [hardware event]
cpu-cycles or cycles                               [hardware event]
instructions                                       [hardware event]
ref-cycles                                         [hardware event]
stalled-cycles-backend or idle-cycles-backend      [hardware event]
stalled-cycles-frontend or idle-cycles-frontend    [hardware event]
alignment-faults                                   [software event]
bpf-output                                         [software event]
cgroup-switches                                    [software event]
context-switches or cs                             [software event]
cpu-clock                                          [software event]
cpu-migrations or migrations                       [software event]
dummy                                              [software event]
emulation-faults                                   [software event]
major-faults                                       [software event]
minor-faults                                       [software event]
page-faults or faults                              [software event]
task-clock                                         [software event]
duration_time                                      [tool event]
user_time                                          [tool event]
system_time                                        [tool event]


cpu:
l1-dcache-loads or cpu/l1-dcache-loads/
l1-dcache-load-misses or cpu/l1-dcache-load-misses/
l1-dcache-stores or cpu/l1-dcache-stores/
l1-dcache-store-misses or cpu/l1-dcache-store-misses/
l1-dcache-prefetches or cpu/l1-dcache-prefetches/
l1-dcache-prefetch-misses or cpu/l1-dcache-prefetch-misses/
l1-icache-loads or cpu/l1-icache-loads/
l1-icache-load-misses or cpu/l1-icache-load-misses/
l1-icache-prefetches or cpu/l1-icache-prefetches/
l1-icache-prefetch-misses or cpu/l1-icache-prefetch-misses/
llc-loads or cpu/llc-loads/
llc-load-misses or cpu/llc-load-misses/
llc-stores or cpu/llc-stores/
llc-store-misses or cpu/llc-store-misses/
llc-prefetches or cpu/llc-prefetches/
llc-prefetch-misses or cpu/llc-prefetch-misses/
dtlb-loads or cpu/dtlb-loads/
dtlb-load-misses or cpu/dtlb-load-misses/
dtlb-stores or cpu/dtlb-stores/
dtlb-store-misses or cpu/dtlb-store-misses/
dtlb-prefetches or cpu/dtlb-prefetches/
dtlb-prefetch-misses or cpu/dtlb-prefetch-misses/
itlb-loads or cpu/itlb-loads/
itlb-load-misses or cpu/itlb-load-misses/
branch-loads or cpu/branch-loads/
branch-load-misses or cpu/branch-load-misses/
node-loads or cpu/node-loads/
node-load-misses or cpu/node-load-misses/
node-stores or cpu/node-stores/
node-store-misses or cpu/node-store-misses/
node-prefetches or cpu/node-prefetches/
node-prefetch-misses or cpu/node-prefetch-misses/

bluetooth modem
nmcli connection show
nmcli connection up "abcd"

pulseaudio --dump-resample-methods
nano /etc/pulse/daemon.conf
pacmd list-sink-inputs


bluetooth :
killall bluealsa
bluealsa -p a2dp-source -p a2dp-sink --a2dp-force-audio-cd &
bluetoothctl
connect 88:c9:e8:e6:2a:69
pulseaudio --start
systemctl status bluetooth
speaker-test -t wav -c 6
speaker-test -t wav -c 6 -d btheadset
pacmd list-sinks
aplay -d bluealsa piano2.wav
mpg123 -a bluealsa -b 1024 http://stream.radioparadise.com/mp3-192

https://agl-gsod-2020-demo-mkdocs.readthedocs.io/en/latest/icefish/apis_services/reference/audio/audio/bluez-alsa/

connman-wait-online.service
systemd-networkd-wait-online.service
systemctl restart bluetooth

pulseaudio-module-bluetooth naaaaa

mpg123 -a bluealsa mp3/01-long_distance_calling-metulsky_curse_revisited.mp3
--sbc-quality=low


perf stat -p $! --timeout 1000 -e branch-misses,branches,l1-dcache-loads,l1-dcache-load-misses,l1-icache-loads,l1-icache-load-misses,cycles,instructions
perf stat -p $! --timeout 1000 -e r12,r13,r1a,r1b,stalled-cycles-frontend,stalled-cycles-backend,cycles,instructions,branch-misses,branches


perf stat -p $! --timeout 1000 -e r12,r13,r1a,r1b,cycles,instructions,branch-misses,branches
perf stat -p $! --timeout 1000 -e stalled-cycles-frontend,stalled-cycles-backend,cycles,instructions



perf stat -p $! --timeout 1000 -e cycles,instructions,stalled-cycles-frontend,branch-misses,branches,r12,r13
perf stat -p $! --timeout 1000 -e cycles,instructions,stalled-cycles-backend,r1a,r1b


r8000000000000000,r8000000000000001,r8000000000000004

~/c/libsdl2/libsdl2-2.30.2+dfsg/debian/build-tests# make -j1 check "testsuiteflags=-j1 --verbose" verbose=1 v=1 &> testlog.txt

export deb_build_options="nocheck parallel=4"
fakeroot debian/rules binary

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv --cpu-variant=debian  --with-jtag-tap --cpu-count=1 --with-jtag-tap  --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma --l2-byte=262144 --update-repo=no --build --load

todo debug :
[ 9576.106084] cpu: 0 pid: 4072 comm: gmain not tainted 6.1.0-rc2+ #11
[ 9576.109440] watchdog: bug: soft lockup - cpu#1 stuck for 22s! [gdbus:4073]
[ 9576.111128] epc : find_vma+0x14/0x44
[ 9576.116689] cpu: 1 pid: 4073 comm: gdbus not tainted 6.1.0-rc2+ #11
[ 9576.119598]  ra : do_page_fault+0xf2/0x31a
[ 9576.124672] epc : handle_mm_fault+0x3c/0xd6
[ 9576.128004] epc : ffffffff8010b100 ra : ffffffff800073d8 sp : ffffffc800ecb880
[ 9576.131406]  ra : handle_mm_fault+0x36/0xd6
[ 9576.137241]  gp : ffffffff810c9240 tp : ffffffd802923480 t0 : ffffffff800072e6
[ 9576.140644] epc : ffffffff8010580a ra : ffffffff80105804 sp : ffffffc800ed3870
[ 9576.146478]  t1 : ffffffff80c00208 t2 : ffffffff80c00288 s0 : ffffffc800ecb8a0
[ 9576.152313]  gp : ffffffff810c9240 tp : ffffffd80435b480 t0 : 0000000000000001
[ 9576.158148]  s1 : ffffffc800ecb920 a0 : ffffffd8008f0f00 a1 : 0000002ac1f46b36
[ 9576.163982]  t1 : 000000000000000e t2 : 0000000000000002 s0 : ffffffc800ed38a0
[ 9576.169818]  a2 : ffffffff810c8a68 a3 : 0000000000040000 a4 : 0000000000000000
[ 9576.175651]  s1 : 0000000000000215 a0 : 0000000000000400 a1 : 0000000000000002
[ 9576.181486]  a5 : 0000002ac1f46b36 a6 : ffffffc800ecba88 a7 : 0000000000000002
[ 9576.187321]  a2 : 0000000000000008 a3 : 0000000000000007 a4 : 0000000000000000
[ 9576.193156]  s2 : ffffffd8008f0f00 s3 : ffffffd802923480 s4 : 0000002ac1f46b36
[ 9576.198990]  a5 : 0000000000001000 a6 : 0000000000001000 a7 : 00000000000000fd
[ 9576.204825]  s5 : ffffffd802923480 s6 : 0000000000000215 s7 : 000000000000000c
[ 9576.210659]  s2 : 0000000000000400 s3 : 0000003f94000b96 s4 : ffffffc800ed3920
[ 9576.216495]  s8 : 000000000000000f s9 : 000000000000000d s10: ffffffd8008f0f60
[ 9576.222329]  s5 : ffffffd8008442a8 s6 : 0000000000000215 s7 : 000000000000000c
[ 9576.228164]  s11: 000000000000000f t3 : 0000000000000001 t4 : 0000000000000340
[ 9576.233998]  s8 : 000000000000000f s9 : 000000000000000d s10: ffffffd8008f0f60
[ 9576.239833]  t5 : 0000000000000000 t6 : 0000000000000000
[ 9576.245667]  s11: 000000000000000f t3 : 0000003f94000000 t4 : ffffffd803d5e10c
[ 9576.249974] status: 0000000200000120 badaddr: 0000000000000000 cause: 8000000000000005
[ 9576.255808]  t5 : 0000000000000000 t6 : 0000003fa3b98fff
[ 9576.262205] [<ffffffff800073d8>] do_page_fault+0xf2/0x31a
[ 9576.266506] status: 0000000200000120 badaddr: 0000000000000000 cause: 8000000000000005
[ 9576.270884] [<ffffffff80002f76>] ret_from_exception+0x0/0xc
[ 9576.277276] [<ffffffff80007402>] do_page_fault+0x11c/0x31a
[ 9576.281789] [<ffffffff80144a98>] do_sys_poll+0x144/0x42c
[ 9576.286235] [<ffffffff80002f76>] ret_from_exception+0x0/0xc
[ 9576.295073] [<ffffffff80144a98>] do_sys_poll+0x144/0x42c

perf stat  --timeout 1000 -e r12,r13,r1a,r1b,stalled-cycles-frontend,stalled-cycles-backend,cycles,instructions,branch-misses,branches -p $!


perf stat  --timeout 1000 -p $! -e stalled-cycles-frontend,stalled-cycles-backend,cycles,instructions,branch-misses,branches
perf stat  --timeout 1000 -p $! -e r12,r13,r1a,r1b,cycles,instructions,stalled-cycles-frontend,stalled-cycles-backend

https://www.elecard.com/videos
https://download.blender.org/peach/bigbuckbunny_movies/
ffmpeg -re -i BigBuckBunny_320x180.mp4  -c:v rawvideo -pix_fmt rgba -f fbdev /dev/fb0
ffplay -vf "scale=640:360" -sws_flags neighbor -an BigBuckBunny_320x180.mp4 
nohup mplayer video/BigBuckBunny_320x180.mp4 &
perf stat  --timeout 1000 -p $! -e r12,r13,r1a,r1b,cycles,instructions,branch-misses,branches
export LD_DEBUG=statistics
https://www.ducea.com/2008/03/06/howto-recompile-debian-packages/
apt-get source  mplayer
apt-get build-dep mplayer
cd <package-ver>
debuild -us -uc


systemctl --user stop pulseaudio.service pulseaudio.socket
systemctl --user disable pulseaudio.service pulseaudio.socket
systemctl --user mask pulseaudio.service
/etc/asound.conf

relaxed btb =>
startup finished in 9.108s (kernel) + 1min 17.848s (userspace) = 1min 26.956s
graphical.target reached after 1min 13.470s in userspace.
timed 5026 gametics in 9474 realtics (18.567659 fps)

stressed btb + late store
startup finished in 8.510s (kernel) + 1min 11.652s (userspace) = 1min 20.162s
graphical.target reached after 1min 10.586s in userspace.
timed 5026 gametics in 8885 realtics (19.798536 fps)
12084250      stalled-cycles-frontend          #   14.50% frontend cycles idle
16944058      stalled-cycles-backend           #   20.33% backend cycles idle

stressed btb + late alu + late store
startup finished in 9.098s (kernel) + 1min 9.677s (userspace) = 1min 18.776s
graphical.target reached after 1min 8.574s in userspace.

16 kb i$d$ relaxed btb
startup finished in 8.219s (kernel) + 1min 5.727s (userspace) = 1min 13.946s
graphical.target reached after 1min 4.705s in userspace.
timed 5026 gametics in 8999 realtics (19.547728 fps

16 kb i$d$ relaxed btb 64 bits bus
startup finished in 8.237s (kernel) + 1min 753ms (userspace) = 1min 8.991s
graphical.target reached after 59.891s in userspace.
timed 5026 gametics in 8943 realtics (19.670134 fps)

16 kb i$d$ stressed btb 64 bits bus
startup finished in 8.806s (kernel) + 1min 213ms (userspace) = 1min 9.019s
graphical.target reached after 59.298s in userspace.
timed 5026 gametics in 8742 realtics (20.122398 fps)



slice_x122y49        fdre (prop_fdre_c_q)         0.456    11.240 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/vexiis_1_logic_core_toplevel_execute_ctrl1_up_float_rs2_lane0_reg[52]/q
                     net (fo=7, routed)           0.824    12.064    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuunpack_rs2_f64_exponent[0]
slice_x133y48        lut3 (prop_lut3_i0_o)        0.124    12.188 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_inserter_rs2_exponent[4]_i_3__0/o
                     net (fo=1, routed)           0.615    12.803    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuunpack_rs2_expraw[0]
slice_x134y46        carry4 (prop_carry4_cyinit_o[1])
                                                  0.598    13.401 f  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_inserter_rs2_exponent_reg[4]_i_2__0/o[1]
                     net (fo=7, routed)           1.162    14.563    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/_zz_vexiis_1_logic_core_toplevel_execute_ctrl1_down_fpuunpack_rs2_rs_pre_norm_lane0_exponent[2]
slice_x149y49        lut6 (prop_lut6_i4_o)        0.303    14.866 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat[3]_i_13__0/o
                     net (fo=4, routed)           0.622    15.489    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat[3]_i_13__0_n_0
slice_x149y50        lut5 (prop_lut5_i0_o)        0.124    15.613 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat[3]_i_9__0/o
                     net (fo=1, routed)           0.000    15.613    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat[3]_i_9__0_n_0
slice_x149y50        carry4 (prop_carry4_s[2]_co[3])
                                                  0.398    16.011 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat_reg[3]_i_2__0/co[3]
                     net (fo=1, routed)           0.000    16.011    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat_reg[3]_i_2__0_n_0
slice_x149y51        carry4 (prop_carry4_ci_o[3])
                                                  0.313    16.324 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat_reg[5]_i_4__0/o[3]
                     net (fo=1, routed)           1.281    17.605    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/_zz_fpuaddsharedplugin_logic_pip_node_0_adder_preshift_exp21_1[7]
slice_x148y55        lut6 (prop_lut6_i2_o)        0.306    17.911 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat[5]_i_8/o
                     net (fo=1, routed)           0.295    18.206    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat[5]_i_8_n_0
slice_x149y56        lut6 (prop_lut6_i3_o)        0.124    18.330 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat[5]_i_3__0_comp_1/o
                     net (fo=1, routed)           1.084    19.414    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpusqrtplugin_logic_sqrt/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat_reg[0]
slice_x130y60        lut6 (prop_lut6_i1_o)        0.124    19.538 r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpusqrtplugin_logic_sqrt/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat[5]_i_1/o
                     net (fo=6, routed)           0.756    20.294    vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat
slice_x137y55        fdse                                         r  vexiiriscvlitex_2f3ff2b95842595a3b7d75e26dfd301e/vexiis_1_logic_core/fpuaddsharedplugin_logic_pip_node_1_adder_preshift_expdifabssat_reg[3]/s


reg [32:0] miaou;

always@(posedge sys_clk, negedge crg_locked) begin
  if(!crg_locked) miaou <= 0;
  else miaou <= miaou + 1;
end

assign xilinxasyncresetsynchronizerimpl0 = (~crg_locked) || !miaou[32];
assign rx_rx = regs1;
assign xilinxasyncresetsynchronizerimpl1 = (~zynq_ps7_rst_n);

 */
