package vexiiriscv.soc.demo

import rvls.spinal.RvlsBackend
import spinal.core._
import spinal.core.sim._
import spinal.core.fiber._
import spinal.lib.{ResetCtrlFiber, StreamPipe}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.com.uart.TilelinkUartFiber
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.misc.{Elf, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA
import vexiiriscv.ParamSimple
import vexiiriscv.execute.lsu.LsuCachelessPlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.test.VexiiRiscvProbe
import vexiiriscv.test.konata.Backend

import java.io.File

//TODO Cleanup
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
    val plugins = param.plugins()
    val cpu = new TilelinkVexiiRiscvFiber(plugins)
    bus << List(cpu.iBus, cpu.dBus)

    val ram = new tilelink.fabric.RamFiber()
    ram.up at(0x80000000l, 0x10000l) of bus
    ram.up.addTag(PMA.EXECUTABLE)
  }

  // Handle all the IO / Peripheral things
  val peripheral = peripheralResetCtrl.cd on new Area {
    val slowBus = Node()
    slowBus at (0x10000000l, 0x10000000l)  of (main.bus)

    val clint = new TilelinkClintFiber()
    clint.node at 0x10000 of slowBus

    val plic = new TilelinkPlicFiber()
    plic.node at 0xC00000l of slowBus

    val uart = new TilelinkUartFiber()
    uart.node at 0x1000 of slowBus
    plic.mapUpInterrupt(1, uart.interrupt)

    val cpuClint = main.cpu.bind(clint)
    val cpuPlic = main.cpu.bind(plic)
  }
}

object MicroSoc extends App{
  val sc = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
  sc.generateVerilog(new MicroSoc())
}

object MicroSocSim extends App{
  val traceKonata = true
  val withRvlsCheck = true

  val sim = SimConfig.withConfig(
    SpinalConfig()
  )
  sim.withFstWave
  sim.compile(new MicroSoc()).doSimUntilVoid("test", seed = 42){dut =>
    dut.cd100.forkStimulus(10000)
    dut.cd48.forkStimulus(20833)
    dut.asyncReset #= true
    delayed(100000)(dut.asyncReset #= false)

    val uartBaudRate = 115200
    val uartBaudPeriod = (1e12 / uartBaudRate).toLong

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
      ).spinalSimFlusher(10000*1000))
    )

    if (withRvlsCheck) probe.add(new RvlsBackend(new File(currentTestPath)).spinalSimFlusher(10 * 10000))

    probe.autoRegions()

    val elf = new Elf(new File("ext/NaxSoftware/soc/uart/build/rv32ima/uart.elf"), 32)
    elf.load(dut.main.ram.thread.logic.mem, 0x80000000l)
    probe.backends.foreach(_.loadElf(0, elf.f))
  }
}
