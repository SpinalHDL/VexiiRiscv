package vexiiriscv.soc.litex

import rvls.spinal.{FileBackend, RvlsBackend}
import spinal.core.fiber.{Fiber, hardFork}
import spinal.lib._
import spinal.core._
import spinal.core.blackboxByteEnables.generateUnblackboxableError
import spinal.core.internals.{MemTopology, PhaseContext, PhaseNetlist}
import spinal.core.sim.{SimDataPimper, killRandom}
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
import spinal.lib.com.eth.sg.{MacSgFiber, MacSgFiberSpec, MacSgParam}
import spinal.lib.com.jtag.sim.JtagRemote
import spinal.lib.cpu.riscv.debug.{DebugModuleFiber, DebugModuleSocFiber}
import spinal.lib.eda.bench.{Bench, Rtl}
import spinal.lib.graphic.YcbcrConfig
import spinal.lib.graphic.vga.{TilelinkVgaCtrlFiber, TilelinkVgaCtrlSpec, Vga, VgaRgbToYcbcr, VgaYcbcrPix2}
import spinal.lib.misc.{Elf, PathTracer, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.misc.test.DualSimTracer
import spinal.lib.sim.SparseMemory
import spinal.lib.{AnalysisUtils, Delay, Flow, ResetCtrlFiber, StreamPipe, master, memPimped, slave, traversableOncePimped}
import spinal.lib.system.tag.{MemoryConnection, MemoryEndpoint, MemoryEndpointTag, MemoryTransferTag, MemoryTransfers, PMA, VirtualEndpoint}
import vexiiriscv.{Global, ParamSimple}
import vexiiriscv.compat.{EnforceSyncRamPhase, MultiPortWritesSymplifier}
import vexiiriscv.execute.ExecuteLanePlugin
import vexiiriscv.execute.lsu.LsuL1Plugin
import vexiiriscv.fetch.{Fetch, FetchL1Plugin, FetchPipelinePlugin, PcPlugin}
import vexiiriscv.misc.{PrivilegedPlugin, TrapPlugin}
import vexiiriscv.prediction.GSharePlugin
import vexiiriscv.riscv.Riscv
import vexiiriscv.schedule.DispatchPlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.soc.micro.MicroSocSim.{elfFile, traceKonata, withRvlsCheck}
import vexiiriscv.test.{PeripheralEmulator, VexiiRiscvProbe, WhiteboxerPlugin}
import vexiiriscv.tester.{FsmHal, FsmHalGen, FsmOption, FsmTask}

import java.awt.{Dimension, Graphics}
import java.awt.image.BufferedImage
import java.io.File
import javax.swing.{JFrame, JPanel, WindowConstants}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/*
SocSim is a simple developpment testbench of the SoC. This is not meant to be used as a regression tool.
Here is an example of arguments :
  --sim-peripheral --regfile-async --with-rvm --with-rva --with-rvc --with-rvd --allow-bypass-from=0 --performance-counters=0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --with-lsu-bypass --relaxed-branch --with-supervisor --fetch-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-ways=4 --lsu-l1-mem-data-width-min=64 --xlen=64 --fma-reduced-accuracy --fpu-ignore-subnormal --with-btb --with-ras --with-gshare --fetch-l1-hardware-prefetch=nl --fetch-l1-refill-count=2 --fetch-l1-mem-data-width-min=128 --lsu-l1-mem-data-width-min=128 --lsu-software-prefetch --lsu-hardware-prefetch rpt --performance-counters 9 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 4 --lsu-l1-writeback-count 4 --lsu-l1-store-buffer-slots=4 --relaxed-div --reset-vector 2147483648 --cpu-count=1 --l2-bytes=524288 --l2-ways=4 --litedram-width=128 --memory-region=268435456,131072,rwx,p --memory-region=2147483648,1073741824,rwxc,m --with-jtag-tap --lsu-l1-coherency --max-ipc --decoders=2 --lanes=2 --load-elf ext/NaxSoftware/baremetal/dhrystone_vexii/build/rv64imafdc/dhrystone_vexii.elf --trace-rvls --trace-konata --trace-spike --trace-wave --reset-vector=0x80000000
 */
object SocSim extends App{
  val socConfig = new SocConfig()
  import socConfig._

  val elfs = ArrayBuffer[File]();
  val bins = ArrayBuffer[(Long, File)]()
  var bootstrapOpensbi = Option.empty[(Long, Long)] // Allows to inject a small bootloader which will setup things and then jump to opensbi
  val fsmTasksGen = mutable.Queue[() => FsmTask]() // Allow to script stimulus from the command line (putc / getc)
  var withRvlsCheck = false
  var traceWave = false
  var traceKonata = false
  var traceRvlsLog = false
  var traceSpikeLog = false
  var dualSim = false
  var seed = 32
  var litexPeripheral = true
  var simPeripheral = false

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    opt[String]("load-elf").unbounded() action { (v, c) => elfs += new File(v) }
    opt[Seq[String]]("load-bin").unbounded() action { (v, c) => bins += java.lang.Long.parseLong(v(0).replace("0x", ""), 16) -> new File(v(1)) }
    opt[Seq[String]]("opensbi-bootstrap").unbounded() action { (v, c) => bootstrapOpensbi = Some(java.lang.Long.parseLong(v(0).replace("0x", ""), 16) -> java.lang.Long.parseLong(v(1).replace("0x", ""), 16)) }
    opt[Unit]("dual-sim") action { (v, c) => dualSim = true }
    opt[Unit]("sim-peripheral") action { (v, c) => simPeripheral = true; litexPeripheral = false }
    opt[Unit]("check-rvls") action { (v, c) => withRvlsCheck = true}
    opt[Unit]("trace-konata") action { (v, c) => traceKonata = true }
    opt[Unit]("trace-spike") action { (v, c) => traceSpikeLog = true }
    opt[Unit]("trace-rvls") action { (v, c) => traceRvlsLog = true }
    opt[Unit]("trace-wave") action { (v, c) => traceWave = true }
    socConfig.addOptions(this)
    FsmOption(this, fsmTasksGen)
  }.parse(args, ()).nonEmpty)

  vexiiParam.lsuL1Coherency = vexiiParam.lsuL1Coherency || cpuCount > 1 || withDma
  vexiiParam.privParam.withRdTime = false // To keep in sync with RVLS

  val spinalConfig = SpinalConfig()
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

  import spinal.core.sim._

  val compiled = SimConfig.allOptimisation.withConfig(spinalConfig).withFstWave.compile(new Soc(socConfig))
  dualSim match {
    case true => DualSimTracer.withCb(compiled, window = 5000000 * 10000l, seed=seed)(test)
    case false => compiled.doSimUntilVoid(seed=seed) { dut => disableSimWave(); test(dut, f => f) }
  }

  def test(dut : Soc, onTrace : (=> Unit) => Unit = cb => {}) : Unit = {
    killRandom()
    dut.litexCd.withSyncReset().forkStimulus(10000)

    // Will load a little bootloader which will initialise a0 a1 a2 to and then jump to opensbi
    val bootstrapBytes = bootstrapOpensbi.map { case (dts, opensbi) => Riscv.bootToOpensbi(dts, opensbi) }

    // Create the VexiiRiscv probe's backends
    val rvls = new RvlsBackend(new File(currentTestPath)).spinalSimFlusher(hzToLong(1000 Hz))
    val tracerFile = traceRvlsLog.option(new FileBackend(new File(currentTestPath(), "tracer.log")))
    val traceBackends = tracerFile ++ List(rvls)

    // Provide the memory preinitialization traces
    for(backend <- traceBackends) {
      bootstrapBytes.foreach(array => backend.loadBytes(vexiiParam.resetVector.toLong, array))
      for (elf <- elfs) backend.loadElf(0, elf)
      for ((at, bin) <- bins) backend.loadBin(at, bin)
    }

    val konata = traceKonata.option(
      new vexiiriscv.test.konata.Backend(new File(currentTestPath, "konata.log")).spinalSimFlusher(hzToLong(1000 Hz))
    )

    // Will inspect the VexiiRiscv cores behaviour
    val probes = for((vexii, hartId) <- dut.system.vexiis.zipWithIndex) yield new VexiiRiscvProbe(
      cpu = vexii.logic.core,
      kb = konata
    ){
      for(backend <- traceBackends) add(backend)
      autoRegions()
      for(backend <- traceBackends) backend.setPc(hartId, vexiiParam.resetVector)
      trace = false
      livenessThreshold = 21000l // Because reset controllers hold the system quite a bit
    }

    if(tracerFile.nonEmpty) probes.foreach(p => p.backends.remove(p.backends.indexOf(tracerFile.get)))
    onTrace {
      println("TRACE ENABLED")
      if (traceWave) enableSimWave()
      if (withRvlsCheck && traceSpikeLog) rvls.debug()
      if (traceKonata) probes.foreach(_.trace = true)

      tracerFile.foreach{f =>
        f.spinalSimFlusher(10 * 10000)
        f.spinalSimTime(10*10000)
        probes.foreach(_.addHead(f)) //addHead to be sure we log things before rvls backends
      }
    }

    val fsmHal = new FsmHalGen(fsmTasksGen)

    if(withJtagTap && !dualSim)  spinal.lib.com.jtag.sim.JtagRemote(dut.debug.tap.jtag, 10000*4)
    if(socConfig.withCpuCd) dut.cpuClk.forkStimulus(5000)
    for(video <- dut.system.video){
      video.cd.forkStimulus(20000)
      VgaDisplaySim(video.ctrl.logic.ctrl.io.vga, video.resetCtrl.cd)
    }
    for(mac <- dut.system.macSg){
      mac.txCd.forkStimulus(7000)
      mac.rxCd.forkStimulus(7000)
      val phy = mac.fiber.logic.phy
      phy.rx.ctl #= 0
      mac.txCd.onSamplings {
        phy.rx.d #= phy.tx.d.toInt
        phy.rx.ctl #= phy.tx.ctl.toInt
      }
    }
    if(dut.debugReset != null)fork{
      dut.debugReset #= true
      dut.cpuCd.waitRisingEdge(2)
      dut.debugReset #= false
    }
    if(socConfig.withDma){
      dut.system.dma.bus.ar.valid #= false
      dut.system.dma.bus.aw.valid #= false
      dut.system.dma.bus.w.valid #= false
      dut.system.dma.bus.r.ready #= false
      dut.system.dma.bus.b.ready #= false
    }

    sleep(1)

    // Implements a very minimal model of the Litex peripherals, enough to get linux to run (serial port without interrupts)
    val onPbusLitex = litexPeripheral generate new Area {
      val axi = dut.system.patcher.pBus
      val UART_REG            = 0xF0001000l
      val UART_REG_RXTX		    = UART_REG + 0*4
      val UART_REG_TXFULL		  = UART_REG + 1*4
      val UART_REG_RXEMPTY  	= UART_REG + 2*4
      val UART_REG_EV_STATUS	= UART_REG + 3*4
      val UART_REG_EV_PENDING	= UART_REG + 4*4
      val UART_REG_EV_ENABLE	= UART_REG + 5*4
      new AxiLite4ReadOnlySlaveAgent(axi.ar, axi.r, dut.litexCd){
        override def doRead(addr: BigInt) = {
          super.doRead(addr)
          axi.r.payload.data.randomize()
          axi.r.payload.data #= (addr.toLong match {
            case x if x < 0x40 => 0
            case UART_REG_TXFULL => 0
            case UART_REG_RXTX => {
              if(fsmHal.putcQueue.nonEmpty) fsmHal.putcQueue.dequeue().toInt & 0xFF
              else if(System.in.available() != 0) System.in.read().toInt & 0xFF
              else ???
            }
            case UART_REG_RXEMPTY => (fsmHal.putcQueue.isEmpty || System.in.available() == 0).toInt
          })
        }
      }
      val woa = new AxiLite4WriteOnlySlaveAgent(axi.aw, axi.w, axi.b, dut.litexCd) {
        override def onWrite(addr: BigInt, data: BigInt, strb: BigInt) = {
          super.onWrite(addr, data, strb)
          addr.toLong match {
            case UART_REG_RXTX => print(data.toChar); if (fsmHal.tasks.nonEmpty) fsmHal.tasks.head.getc(fsmHal, data.toChar)
            case UART_REG_EV_PENDING =>
            case UART_REG_EV_ENABLE =>
          }
        }
      }
    }

    val onPbusSim = simPeripheral generate new Area {
      val axi = dut.system.patcher.pBus
      val emu = new PeripheralEmulator(0x10000000, null, null) {
        override def getClintTime() = simTime()/10000
      }
      new AxiLite4ReadOnlySlaveAgent(axi.ar, axi.r, dut.litexCd){
        override def doRead(addr: BigInt) = {
          super.doRead(addr)
          axi.r.payload.data.randomize()
          val data = Array.fill[Byte](axi.config.bytePerWord)(0)
          emu.access(false, addr.toLong, data)
          axi.r.payload.data #= data
        }
      }
      val woa = new AxiLite4WriteOnlySlaveAgent(axi.aw, axi.w, axi.b, dut.litexCd) {
        override def onWrite(addr: BigInt, data: BigInt, strb: BigInt) = {
          super.onWrite(addr, data, strb)
          axi.r.payload.data.randomize()
          val buffer = Array.fill[Byte](axi.config.bytePerWord)(0)
          buffer(0) = data.toByte
          emu.access(true, addr.toLong, buffer)
        }
      }
    }

    val onAxi = new Area{
      val ddrMemory = SparseMemory(seed = 0) //seed 0 to match RVLS behavior
      bootstrapBytes.foreach( array => ddrMemory.write(vexiiParam.resetVector.toLong, array))

      for (file <- elfs) {
        val elf = new Elf(file, socConfig.vexiiParam.xlen)
        elf.load(ddrMemory, 0)
      }
      for ((offset, file) <- bins) {
        ddrMemory.loadBin(offset, file)
      }
      val axi = dut.system.patcher.mBus
      val woa = new Axi4WriteOnlySlaveAgent(axi.aw, axi.w, axi.b, dut.cpuCd) {
        awDriver.factor = 0.9f
        wDriver.factor = 0.9f
        bDriver.transactionDelay = () => 1
      }
      var bytesAccess = 0l
      new Axi4WriteOnlyMonitor(axi.aw, axi.w, axi.b, dut.cpuCd) {
        override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = ddrMemory.write(address.toLong, data)
        override def onWriteStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int, cache : Int): Unit = {
          bytesAccess += len * (1 << size)
        }
      }
      val roa = new Axi4ReadOnlySlaveAgent(axi.ar, axi.r, dut.cpuCd, withReadInterleaveInBurst = false) {
        arDriver.factor = 0.8f
        rDriver.transactionDelay = () => simRandom.nextInt(3)
        baseLatency = 70 * 1000

        override def readByte(address: BigInt, id : Int): Byte = {
          bytesAccess += 1
          ddrMemory.read(address.toLong)
        }
      }
    }

//    for(y <- 0 until 600; x <- 0 until 800){
//      val color = (x & 0xFF) + ((y & 0xFF) << 8)// + (((x+y) & 0xFF) << 16)
//      onAxi.ddrMemory.write(0x40c00000 + x * 4 + y * 4 * 800, color)
////      val color = (x & 0x1F)+((y & 0x3F) << 5)
////      onAxi.ddrMemory.write(0x40c00000 + x*2+y*2*640, color + (color << 16))
//    }
//
//    var cnt = 0
//    def setPix(value : Int) = {
//      onAxi.ddrMemory.write(0x40c00000 + cnt, value)
//      onAxi.ddrMemory.write(0x40c00000 + cnt + 4, value)
//      cnt += 8
//    }
//    setPix(0x00000000)
//    setPix(0x000000ff)
//    setPix(0x0000ff00)
//    setPix(0x00ff0000)
//    setPix(0x00ffffff)
//    onAxi.ddrMemory.write(0x40c00000 + cnt, 0x000000FF); cnt += 4
//    onAxi.ddrMemory.write(0x40c00000 + cnt, 0x00800080); cnt += 4
  }
}
