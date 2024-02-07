package vexiiriscv.soc.demo

import rvls.spinal.RvlsBackend
import spinal.core._
import spinal.core.sim._
import spinal.core.fiber._
import spinal.lib.{ResetCtrlFiber, StreamPipe}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.com.uart.TilelinkUartFiber
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.eda.bench.Rtl
import spinal.lib.misc.{Elf, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA
import vexiiriscv.ParamSimple
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.test.VexiiRiscvProbe

import java.io.File
import scala.collection.mutable.ArrayBuffer

class MicroSoc() extends Component {
  val asyncReset = in Bool()
  val cd100 = ClockDomain.external("cd100", withReset = false, frequency = FixedFrequency(100 MHz))
  val cd48 = ClockDomain.external("cd48", withReset = false, frequency = FixedFrequency(48 MHz))

  val debugResetCtrl = cd100 on new ResetCtrlFiber().addAsyncReset(asyncReset, HIGH)
  val peripheralResetCtrl = cd48 on new ResetCtrlFiber().addReset(debugResetCtrl)
  val mainResetCtrl = cd100 on new ResetCtrlFiber().addReset(peripheralResetCtrl)

  val main = mainResetCtrl.cd on new Area {
    val bus = tilelink.fabric.Node()

    val param = new ParamSimple()
    param.withMul = false
    param.withDiv = false
    param.relaxedBranch = true
    val plugins = param.plugins()
    val cpu = new TilelinkVexiiRiscvFiber(plugins)
    bus << cpu.buses

    val ram = new tilelink.fabric.RamFiber(16 KiB)
    ram.up at 0x80000000l of bus
  }

  // Handle all the IO / Peripheral things
  val peripheral = peripheralResetCtrl.cd on new Area {
    val busXlen = Node().forceDataWidth(main.param.xlen)
    busXlen << main.bus

    val bus32 = Node().forceDataWidth(32)
    bus32 << main.bus

    val clint = new TilelinkClintFiber()
    clint.node at 0x10010000 of busXlen

    val plic = new TilelinkPlicFiber()
    plic.node at 0x10C00000 of bus32

    val uart = new TilelinkUartFiber()
    uart.node at 0x10001000 of bus32
    plic.mapUpInterrupt(1, uart.interrupt)

    val cpuClint = main.cpu.bind(clint)
    val cpuPlic = main.cpu.bind(plic)
  }
}

object MicroSocGen extends App{
  SpinalVerilog(new MicroSoc())
}

object MicroSocSynt extends App{
  import spinal.lib.eda.bench._
  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(new MicroSoc()))

  val targets = XilinxStdTargets().take(2)
  Bench(rtls, targets)
}


object MicroSocSim extends App{
  var traceKonata = false
  var withRvlsCheck = false
  var elf: Elf = null
  val sim = SimConfig
  sim.withTimeSpec(1 ns, 1 ps)

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    opt[String]("load-elf") action { (v, c) => elf = new Elf(new File(v), 32) }
    opt[Unit]("trace-konata") action { (v, c) => traceKonata = true }
    opt[Unit]("check-rvls") action { (v, c) => withRvlsCheck = true }
    sim.addOptions(this)
  }.parse(args, Unit).nonEmpty)


  sim.compile(new MicroSoc()).doSimUntilVoid("test", seed = 42){dut =>
    dut.cd100.forkStimulus()
    dut.cd48.forkStimulus()
    dut.asyncReset #= true
    delayed(100 ns)(dut.asyncReset #= false)

    val uartBaudPeriod = hzToLong(115200 Hz)
    val uartTx = UartDecoder(
      uartPin = dut.peripheral.uart.logic.uart.txd,
      baudPeriod = uartBaudPeriod
    )
    val uartRx = UartEncoder(
      uartPin = dut.peripheral.uart.logic.uart.rxd,
      baudPeriod = uartBaudPeriod
    )

    val probe = new VexiiRiscvProbe(
      cpu = dut.main.cpu.logic.core,
      kb = traceKonata.option(new vexiiriscv.test.konata.Backend(
        new File(currentTestPath, "konata.log")
      ).spinalSimFlusher(hzToLong(1000 Hz)))
    )

    if (withRvlsCheck) probe.add(new RvlsBackend(new File(currentTestPath)).spinalSimFlusher(hzToLong(1000 Hz)))

    probe.autoRegions()

    if(elf != null) {
      elf.load(dut.main.ram.thread.logic.mem, 0x80000000l)
      probe.backends.foreach(_.loadElf(0, elf.f))
    }
  }
}
