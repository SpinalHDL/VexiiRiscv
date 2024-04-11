package vexiiriscv.tester




import rvls.spinal.{FileBackend, RvlsBackend}
import spinal.core
import spinal.core._
import spinal.core.sim._
import spinal.core.fiber._
import spinal.lib.{ResetCtrlFiber, StreamPipe}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric.{Node, SlaveBus}
import spinal.lib.bus.tilelink.sim.{Checker, MemoryAgent}
import spinal.lib.com.uart.TilelinkUartFiber
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug.DebugModuleFiber
import spinal.lib.eda.bench.Rtl
import spinal.lib.misc.{Elf, PathTracer, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.misc.test.DualSimTracer
import spinal.lib.sim.SparseMemory
import spinal.lib.system.tag.PMA
import vexiiriscv.ParamSimple
import vexiiriscv.execute.SrcPlugin
import vexiiriscv.misc.TrapPlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.soc.demo.DebugModuleSocFiber
import vexiiriscv.soc.demo.MicroSocSim.args
import vexiiriscv.test.{PeripheralEmulator, VexiiRiscvProbe}

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class TlTbParam {
  var withJtagTap = false
  var withJtagInstruction = false
  var vexiiParam = new ParamSimple()
  var vexiiCount = 1
  var l2Bytes = 128*1024
  var l2Ways = 4
  var l2Enable = false
  var hubEnable = false

  def withDebug = withJtagTap ||  withJtagInstruction
}

class TlTbTop(p : TlTbParam) extends Component {
  import p._
  val asyncReset = in Bool()
  val cd100 = ClockDomain.external("cd100", withReset = false, frequency = FixedFrequency(100 MHz))

  val debugResetCtrl = cd100(new ResetCtrlFiber().addAsyncReset(asyncReset, HIGH))
  val mainResetCtrl  = cd100(new ResetCtrlFiber().addAsyncReset(debugResetCtrl))

  val debug = p.withDebug generate debugResetCtrl.cd(new DebugModuleSocFiber(p.withJtagInstruction){
    mainResetCtrl.addSyncRelaxedReset(dm.ndmreset, HIGH)
  })


  val main = mainResetCtrl.cd on new Area {
    val withCoherency = vexiiParam.lsuL1Coherency
    val mainDataWidth = vexiiParam.memDataWidth

    val vexiis = for (hartId <- 0 until vexiiCount) yield new TilelinkVexiiRiscvFiber(vexiiParam.plugins(hartId))

    val cBus, ioBus = fabric.Node()
    for (vexii <- vexiis) {
      cBus << List(vexii.iBus, vexiiParam.fetchL1Enable.mux(vexii.lsuL1Bus, vexii.dBus))
      if (vexiiParam.fetchL1Enable) ioBus << List(vexii.dBus)
      if (p.withDebug) debug.bindHart(vexii)
    }

    var perfBus: Node = null
    val l2 = (perfBus == null && l2Enable) generate new Area {
      val cache = new CacheFiber()
      cache.parameter.cacheWays = l2Ways
      cache.parameter.cacheBytes = l2Bytes
      cache.up << cBus
      cache.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL, d = StreamPipe.FULL)
      cache.down.setDownConnection(d = StreamPipe.S2M)
      cache.down.forceDataWidth(mainDataWidth)
      perfBus = cache.down
    }

    val hub = (perfBus == null && hubEnable) generate new Area {
      val hub = new HubFiber()
      hub.up << cBus
      hub.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL)
      hub.down.forceDataWidth(mainDataWidth)
      perfBus = hub.down
    }

    val mBusCoherent = perfBus == null
    if(mBusCoherent) perfBus = cBus

    val mBus = new SlaveBus(
      M2sSupport(
        transfers = M2sTransfers.all,
        dataWidth = 512,
        addressWidth = 32
      ),
      S2mParameters(
        List.tabulate(mBusCoherent.mux(4, 0))(i =>
          S2mAgent(
            name = null,
            sinkId = SizeMapping(i * 8, 8),
            emits = S2mTransfers(probe = SizeRange(0x40))
          )
        )
      )
    )
    mBus.node at SizeMapping(0x80000000l, 0x80000000l) of perfBus
    mBus.node.addTags(PMA.MAIN, PMA.EXECUTABLE)

    // Handle all the IO / Peripheral things
    val peripheral = new Area {
      val busXlen = Node().forceDataWidth(vexiiParam.xlen)
      busXlen << ioBus
      busXlen.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)

      val bus32 = Node().forceDataWidth(32)
      bus32 << busXlen

      val clint = new TilelinkClintFiber()
      clint.node at 0x10010000 of busXlen

      val plic = new TilelinkPlicFiber()
      plic.node at 0x10C00000 of bus32

//      val uart = new TilelinkUartFiber()
//      uart.node at 0x10001000 of bus32
//      plic.mapUpInterrupt(1, uart.interrupt)
//      uart.config.initConfig.baudrate = 1000000

      val eBus = new SlaveBus(
        M2sSupport(
          transfers = M2sTransfers.allGetPut,
          dataWidth = 32,
          addressWidth = 12
        ),
        S2mParameters(Nil)
      )
      eBus.node at SizeMapping(0x10000000l, 0x1000) of ioBus

      val vexiiBindings = vexiis.map( vexii => new Area {
        val onPlic = vexii.bind(plic)
        val onClint = vexii.bind(clint)
      })
    }
  }
}

//object MicroSocGen extends App{
//  val report = SpinalVerilog{
//    val p = new TlTbParam()
//    new MicroSoc(p)
//  }
//
//  val h = report.toplevel.main.cpu.logic.core.host
//  val path = PathTracer.impl(h[SrcPlugin].logic.addsub.rs2Patched, h[TrapPlugin].logic.harts(0).trap.pending.state.tval)
//  println(path.report)
//}

object TlTbSim extends App{
  var traceKonata = false
  var traceSpike = false
  var traceRvls = false
  var withRvlsCheck = true
  var traceWave = false
  var dualSim = false
  var elf: File = null
  val bins = ArrayBuffer[(Long, File)]()
  val sim = SimConfig
  sim.withTimeSpec(1 ns, 1 ps)
  val p = new TlTbParam()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    opt[String]("load-elf") action { (v, c) => elf = new File(v)}
    opt[Seq[String]]("load-bin") unbounded() action { (v, c) => bins += java.lang.Long.parseLong(v(0).replace("0x", ""), 16) -> new File(v(1)) }
    opt[Unit]("trace-konata") action { (v, c) => traceKonata = true }
    opt[Unit]("trace-spike") action { (v, c) => traceSpike = true }
    opt[Unit]("check-rvls") action { (v, c) => withRvlsCheck = true}
    opt[Unit]("trace-wave") action { (v, c) => traceWave = true }
    opt[Unit]("dual-sim") action { (v, c) => dualSim = true }
    opt[Unit]("trace-all") action { (v, c) => traceKonata = true; sim.withFstWave; traceSpike = true; traceRvls = true; traceWave = true }
    opt[Int]("vexii-count") action {(v, c) => p.vexiiCount = v }
    opt[Int]("l2-bytes") action {(v, c) => p.l2Bytes = v }
    opt[Int]("l2-ways") action {(v, c) => p.l2Ways = v }
    opt[Unit]("l2-enable") action { (v, c) => p.l2Enable = true }
    opt[Unit]("hub-enable") action { (v, c) => p.hubEnable = true }

    p.vexiiParam.addOptions(this)
  }.parse(args, Unit).nonEmpty)

  sim.withConfig(SpinalConfig(dontCareGenAsZero = true))

  val compiled = sim.withFstWave.compile(new TlTbTop(p))


  DualSimTracer.withCb(compiled, window = 200000 * 10000l, seed = 42, dualSimEnable = dualSim){ (dut, onTrace) =>
    val fr = getForbiddenRandom()
    val fri = fr.get()
    dut.cd100.forkStimulus()
    dut.asyncReset #= true
    delayed(100 ns)(dut.asyncReset #= false)
//    dut.cd100.onSamplings(assert(fri == fr.get()))

    val uartBaudPeriod = hzToLong(1000000 Hz)
//    val uartTx = UartDecoder(
//      uartPin = dut.main.peripheral.uart.logic.uart.txd,
//      baudPeriod = uartBaudPeriod
//    )
//    val uartRx = UartEncoder(
//      uartPin = dut.main.peripheral.uart.logic.uart.rxd,
//      baudPeriod = uartBaudPeriod
//    )

    val tracerFile = traceRvls.option(new FileBackend(new File(currentTestPath(), "tracer.log")))
    val rvls = withRvlsCheck generate new RvlsBackend(new File(currentTestPath)).spinalSimFlusher(hzToLong(1000 Hz))

    val onVexiis = for((vexii, hartId) <- dut.main.vexiis.zipWithIndex) yield new Area{
      val konata = traceKonata.option(
        new vexiiriscv.test.konata.Backend(new File(currentTestPath, s"konata$hartId.log")).spinalSimFlusher(hzToLong(1000 Hz))
      )
      val probe = new VexiiRiscvProbe(
        cpu = vexii.logic.core,
        kb = konata
      )
      probe.trace = false
      if (withRvlsCheck) probe.add(rvls)
      if (p.withJtagTap) probe.checkLiveness = false
    }

    if(p.withJtagTap) {
      spinal.lib.com.jtag.sim.JtagRemote(dut.debug.tap.jtag, hzToLong(dut.cd100.frequency.getValue)*4)
    }


    val mem = SparseMemory(seed = 0, randOffset = 0x80000000l)
    val ma = new MemoryAgent(dut.main.mBus.node.bus, dut.mainResetCtrl.cd , seed = 0, randomProberFactor = 0.2f, memArg = Some(mem))(null)
    ma.driver.driver.setFactor(0.8f)
    val checker = if (ma.monitor.bus.p.withBCE) Checker(ma.monitor)

    if(elf != null) {
      val ef = new Elf(elf, 32)
      ef.load(mem, 0x80000000l)
      onVexiis.foreach(_.probe.backends.foreach(_.loadElf(0, elf)))
    }

    for ((offset, file) <- bins) {
      mem.loadBin(offset-0x80000000l, file)
      if (withRvlsCheck) rvls.loadBin(offset, file)
      tracerFile.foreach(_.loadBin(offset, file))
    }

    val peripheral = new PeripheralEmulator(0, null, null, null, null, cd = dut.mainResetCtrl.cd) {
      override def getClintTime(): Long = 0
      val onBus = bind(dut.main.peripheral.eBus.node.bus, dut.main.peripheral.eBus.node.clockDomain)
    }

    delayed(1)(onVexiis.foreach(_.probe.autoRegions()))


//    dut.cd100.forkSimSpeedPrinter(10)
//    fork{
//      val window = 15l*100000*10000
//      sleep(253002270000l-window)
//      enableSimWave()
//      sleep(window)
//      simSuccess()
//    }

    fork{
      while(true) {
        enableSimWave()
        sleep(1000 * 10000)
        disableSimWave()
        sleep(100000 * 10000)
      }
    }

    onTrace {
      if (!traceWave) disableSimWave()
      if (withRvlsCheck && traceSpike) rvls.debug()
      if (traceKonata) onVexiis.foreach(_.probe.trace = true)

      tracerFile.foreach { f =>
        f.spinalSimFlusher(1000 * 1000000)
        f.spinalSimTime(1000 * 1000000)
        onVexiis.foreach(_.probe.add(f))
      }

//      val r = probe.backends.reverse
//      probe.backends.clear()
//      probe.backends ++= r
    }
  }
}

/*
//TODO coherency
- Check D$ coherent ack delay
- punish double LR

 */