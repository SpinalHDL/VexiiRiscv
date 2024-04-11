package vexiiriscv.soc.litex

import spinal.core.fiber.Fiber
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer, Axi4ToTilelinkFiber}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.cpu.riscv.debug.DebugModuleFiber
import spinal.lib.misc.TilelinkClintFiber
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.{Delay, Flow, ResetCtrlFiber, StreamPipe, master, slave}
import spinal.lib.system.tag.{MemoryConnection, MemoryEndpoint, MemoryTransferTag, PMA}
import vexiiriscv.ParamSimple
import vexiiriscv.compat.{EnforceSyncRamPhase, MultiPortWritesSymplifier}
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.soc.demo.DebugModuleSocFiber

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
//  var sharedBusWidth = 32
  def withL2 = l2Bytes > 0
}

class Soc(c : SocConfig, systemCd : ClockDomain) extends Component{
  import c._

  val system = systemCd on new AreaRoot {
    val mainDataWidth = vexiiParam.memDataWidth
    val withCoherency = vexiiParam.lsuL1Coherency
    val vexiis = for (hartId <- 0 until cpuCount) yield new TilelinkVexiiRiscvFiber(vexiiParam.plugins(hartId))
    for (vexii <- vexiis) {
      vexii.lsuL1Bus.setDownConnection(a = StreamPipe.HALF, b = StreamPipe.HALF, c = StreamPipe.FULL, d = StreamPipe.M2S, e = StreamPipe.HALF)
      vexii.dBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S)
      vexii.iBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S)
    }

    val cBus, ioBus = fabric.Node()
    ioBus.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.NONE)
    for (vexii <- vexiis) {
      cBus << List(vexii.iBus, vexiiParam.fetchL1Enable.mux(vexii.lsuL1Bus, vexii.dBus))
      if(vexiiParam.fetchL1Enable) ioBus << List(vexii.dBus)
    }

    val dma = c.withDma generate new Area {
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
      cBus << filter.down
      filter.down.setDownConnection(a = StreamPipe.FULL)



      //As litex reset will release before our one, we need to ensure that we don't eat a transaction
      Fiber build {
        bridge.read.get
        bridge.write.get
        when(ClockDomain.current.isResetActive){
          bus.ar.ready := False
          bus.aw.ready := False
          bus.w.ready := False
        }
      }
    }

    assert(!(!withCoherency && withL2))

    var perfBus: Node = null
    val direct = (!withCoherency) generate new Area{
      perfBus = cBus
    }

    val hub = (withCoherency && !withL2) generate new Area {
      val hub = new HubFiber()
      hub.up << cBus
      hub.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL)
      hub.down.forceDataWidth(mainDataWidth)
      perfBus = hub.down
    }

    val l2 = (withCoherency && withL2) generate new Area {
      val cache = new CacheFiber()
      cache.parameter.cacheWays = l2Ways
      cache.parameter.cacheBytes = l2Bytes
      cache.up << cBus
      cache.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL, d = StreamPipe.FULL)
      cache.down.setDownConnection(d = StreamPipe.S2M)
      cache.down.forceDataWidth(mainDataWidth)
      perfBus = cache.down
    }

    val memRegions = regions.filter(e => e.onMemory && e.isCachable)
    val axiLiteRegions = regions.filter(e => e.onPeripheral)

    val withMem = memRegions.nonEmpty
    val toAxi4 = withMem generate new fabric.Axi4Bridge
    if (withMem) {
      toAxi4.up.forceDataWidth(litedramWidth)
      regions.filter(_.onMemory).foreach(r =>
        toAxi4.up at r.mapping of perfBus
      )
      toAxi4.down.addTag(PMA.MAIN)
      toAxi4.down.addTag(PMA.EXECUTABLE)
      for(region <- memRegions) {
        toAxi4.down.addTag(new MemoryEndpoint {
          override def mapping = SizeMapping(0, region.mapping.size)
        })
      }
    }


    val peripheral = new Area {
      val bus = Node()
      bus << (perfBus, ioBus)
      bus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)
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


      for (vexii <- vexiis) {
        vexii.bind(clint)
        vexii.bind(plic)
      }

      val toAxiLite4 = new fabric.AxiLite4Bridge
      toAxiLite4.up << bus




      val virtualRegions = for (region <- axiLiteRegions) yield new Area with SpinalTagReady {
        def self = this

        new MemoryConnection {
          override def up = toAxiLite4.down
          override def down = self
          override def transformers = Nil
//          override def mapping = region.mapping //TODO
          populate()
        }
        self.addTag(new MemoryEndpoint {
          override def mapping = region.mapping
        })

        addTag(new MemoryTransferTag {
          override def get = toAxiLite4.up.m2s.parameters.emits
        })
        if (region.isCachable) addTag(PMA.MAIN)
        if (region.isExecutable) addTag(PMA.EXECUTABLE)
      }
    }

    val mBus = withMem generate (Fiber build master(toAxi4.down.pipelined()))
    val pBus = Fiber build master(peripheral.toAxiLite4.down.pipelined(ar = StreamPipe.HALF, aw = StreamPipe.HALF, w = StreamPipe.HALF, b = StreamPipe.HALF, r = StreamPipe.HALF))

//    val debug = c.withDebug generate new Area {
//      val cd = ClockDomain.current.copy(reset = in Bool())
//      val cdi = c.withJtagInstruction generate ClockDomain.external("jtag_instruction", withReset = false)
//
//      val dm = cd(new DebugModuleFiber())
//      vexiis.foreach(dm.bindHart)
//      val tap = c.withJtagTap generate cd(dm.withJtagTap())
//      val instruction = c.withJtagInstruction generate cdi(dm.withJtagInstruction())
//    }

    val patcher = Fiber build new Area {
      if (c.withDma) {
        Axi4SpecRenamer(dma.bus)
      }
      if (withMem) Axi4SpecRenamer(mBus.get)
      AxiLite4SpecRenamer(pBus.get)

//      vexii(0).dBus.bus


      val i = MemoryConnection.getMemoryTransfers(vexiis(0).iBus)
      val d = MemoryConnection.getMemoryTransfers(vexiis(0).dBus)
//      val p = MemoryConnection.getMemoryTransfers(vexiis(0).pBus)

      println(i)

//      if (withJtagTap) debug.tap.jtag.setName("jtag")
//      if (withJtagInstruction) debug.instruction.setName("jtag_instruction")
//      if (c.withDebug) {
//        debug.dm.ndmreset.toIo().setName("debug_ndmreset")
//        debug.cd.reset.setName("debug_reset")
//      }

//      val tracer = master(Reg(Flow(Bits(8 bits))))
//      val trigger = False
//      tracer.valid init (False)
//      tracer.valid := Delay(trigger, 2)
//      tracer.payload init (0)
//      for (nax <- vexii) {
//        nax.plugins.collectFirst { case p: CsrTracer => p } match {
//          case Some(p) => when(p.logic.flowOut.valid) {
//            trigger := True
//            tracer.payload := p.logic.flowOut.payload
//          }
//          case None =>
//        }
//      }

//      vexiis.foreach(_.logic.core.addAttribute("keep_hierarchy", "yes"))
//      component.definition.addAttribute("keep_hierarchy", "yes")
    }
  }

  val debugReset = c.withDebug generate in.Bool()
  val debug = c.withDebug generate ClockDomain(systemCd.clock, debugReset)(new DebugModuleSocFiber(withJtagInstruction) {
    out(dm.ndmreset)
    system.vexiis.foreach(bindHart)
  })
}




object SocGen extends App{
  var netlistDirectory = "."
  var netlistName = "VexiiRiscvLitex"
  val socConfig = new SocConfig()
  import socConfig._

  vexiiParam.fetchL1Enable = true
  vexiiParam.lsuL1Enable = true
  vexiiParam.privParam.withRdTime = true

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    vexiiParam.addOptions(this)
    opt[String]("netlist-directory") action { (v, c) => netlistDirectory = v }
    opt[String]("netlist-name") action { (v, c) => netlistName = v }
    opt[Int]("litedram-width") action { (v, c) => litedramWidth = v }
    opt[Int]("cpu-count") action { (v, c) => cpuCount = v }
    opt[Int]("l2-bytes") action { (v, c) => l2Bytes = v }
    opt[Int]("l2-ways") action { (v, c) => l2Ways = v }
    opt[Unit]("with-dma") action { (v, c) => withDma = true }
    opt[Unit]("with-jtag-tap") action { (v, c) => withJtagTap = true }
    opt[Unit]("with-jtag-instruction") action { (v, c) => withJtagInstruction = true }
    opt[Seq[String]]("memory-region") unbounded() action  { (v, c) =>
      assert(v.length == 4, "--memory-region need 4 parameters")
      val r = new LitexMemoryRegion(SizeMapping(BigInt(v(0)), BigInt(v(1))), v(2), v(3))
      regions += r
      assert(!(r.onMemory && !r.isCachable), s"Region $r isn't supported by VexiiRiscv, data cache will always cache memory")
    }
  }.parse(args, Unit).nonEmpty)

  vexiiParam.lsuL1Coherency = cpuCount > 1 || withDma

  val spinalConfig = SpinalConfig(inlineRom = true, targetDirectory = netlistDirectory)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
  spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
  spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

  spinalConfig.generateVerilog {

    new Soc(socConfig, ClockDomain.external("system")).setDefinitionName(netlistName)
  }
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
         |VexiiRiscv.with_rvc = ${withRvc.toInt}
         |VexiiRiscv.with_rvm = ${(withMul && withDiv).toInt}
         |VexiiRiscv.internal_bus_width = ${memDataWidth}
         |""".stripMargin)
    close()
  }

}

/*
vex 1 =>
Memspeed at 0x40000000 (Sequential, 8.0KiB)...
  Write speed: 1.6MiB/s
   Read speed: 867.6KiB/s


Write speed: 647.4KiB/s
 Read speed: 689.3KiB/s

Write speed: 811.9KiB/s
 Read speed: 833.5KiB/s

Write speed: 1.3MiB/s
 Read speed: 833.5KiB/s

Write speed: 1.3MiB/s
 Read speed: 1.0MiB/s


python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged" --with-jtag-tap --build --load
--with-video-framebuffer --with-spi-sdcard --with-ethernet

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --with-mul --with-div --with-rva --with-btb --with-ras --with-gshare --fetch-l1 --fetch-l1-sets=64 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-sets=64 --lsu-l1-ways=4 --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-supervisor --with-user --performance-counters 0 --debug-privileged --debug-triggers=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64" --with-jtag-tap --trace-fst --with-jtagremote


litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --with-mul --with-div --with-rva --with-btb --with-ras --with-gshare --fetch-l1-sets=64 --fetch-l1-ways=4 --lsu-l1-sets=64 --lsu-l1-ways=4 --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-supervisor --with-user --performance-counters 0" --trace-fst --sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json --trace-start 120000000000000 --trace-end 122000000000000 --trace

--sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json

--decoders=2 --lanes=2 --with-dispatcher-buffer"

--trace
--trace-start 600000000000
60000000000000
--sdram-init images/sim.json


/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl -c "load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rootfs.cpio 0x40000000" -c exit
(* MARK_DEBUG = "TRUE" *)

// Minimal linux
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --with-rva --with-supervisor --performance-counters 0" --with-jtag-tap  --load
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/Image 0x40000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rv32.dtb 0x40ef0000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rootfs.cpio 0x41000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/opensbi.bin 0x40f00000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000
resume

//Linux++ single issue 1.77
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --with-jtag-tap  --build --load

//Linux++ dual issue
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass --decoders=2 --lanes=2" --with-jtag-tap  --build --load



python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--debug-privileged" --with-jtag-tap --build --load
export HART_COUNT=2
/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--debug-privileged" --with-jtag-instruction --build --load
openocd -f digilent_nexys_video.tcl -f vexiiriscv_jtag_tunneled.tcl


python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4  --with-btb --with-ras --with-gshare" --with-jtag-tap  --load




//linux ++ dual core
make O=build/full  BR2_EXTERNAL=../config litex_vexriscv_full_defconfig
(cd build/full/ && make -j20)

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --cpu-count=2  --with-jtag-tap --sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json
python3 -m litex_boards.targets.digilent_nexys_video --soc-json build/digilent_nexys_video/csr.json --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-spi-sdcard --with-ethernet  --build --load
python3 -m litex_boards.targets.digilent_nexys_video --soc-json build/digilent_nexys_video/csr.json --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma --l2-bytes=131072 --load
--lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32

export HART_COUNT=2
/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl -f dev.tcl

load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/Image 0x40000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/linux_2c.dtb 0x40ef0000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/rootfs.cpio 0x41000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000
targets riscv.cpu.1; resume
targets riscv.cpu.0; resume

boot 0x40f00000

udhcpc
cat >> /etc/X11/xorg.conf << EOF
> Section "Module"
>   Load "fb"
>   Load "shadow"
>   Load "fbdevhw"
> EndSection
> EOF

 */