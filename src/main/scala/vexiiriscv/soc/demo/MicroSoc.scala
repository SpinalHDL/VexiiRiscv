package vexiiriscv.soc.demo

import rvls.spinal.RvlsBackend
import spinal.core._
import spinal.core.sim._
import spinal.core.fiber._
import spinal.lib.StreamPipe
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.com.uart.TilelinkUartFiber
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.misc.TilelinkClintFiber
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA
import vexiiriscv.ParamSimple
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.test.VexiiRiscvProbe
import vexiiriscv.test.konata.Backend

import java.io.File

/*
//TODO data reorder
//TODO Moving away from stupid hex file ?
//TODO Cleanup

 */

class MicroSoc() extends Component {
  val mainBus = tilelink.fabric.Node()

  val param = new ParamSimple()
  val plugins = param.plugins()
  val cpu = new TilelinkVexiiRiscvFiber(plugins)
  mainBus << List(cpu.iBus, cpu.dBus)

  val ram = new tilelink.fabric.RamFiber()
  ram.up at (0x80000000l, 0x10000l) of mainBus
  ram.loadHex("ext/NaxSoftware/soc/uart/build/rv32ima/uart.hex", 0x80000000l)

  // Handle all the IO / Peripheral things
  val peripheral = new Area {
    val slowBus = Node()
    slowBus at (0x10000000l, 0x10000000l)  of (mainBus)

    val clint = new TilelinkClintFiber()
    clint.node at 0x10000 of slowBus

    val plic = new TilelinkPlicFiber()
    plic.node at 0xC00000l of slowBus

    val uart = new TilelinkUartFiber()
    uart.node at 0x1000 of slowBus
    plic.mapInterrupt(1, uart.interrupt)

    val cpuClint = cpu.bind(clint)
    val cpuPlic = cpu.bind(plic)
  }
}

object MicroSoc extends App{
  val sc = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
  sc.generateVerilog(new MicroSoc())
}

object MicroSocSim extends App{
  val sim = SimConfig.withConfig(
    SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
  )
  sim.withFstWave
  sim.compile(new MicroSoc()).doSimUntilVoid("test", seed = 42){dut =>
    dut.clockDomain.forkStimulus(10000)
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

    val withRvls = true
    val traceKonata = true
    val withRvlsCheck = true

    val rvls = withRvlsCheck generate new RvlsBackend(new File(currentTestPath))
    if (withRvlsCheck) {
      rvls.spinalSimFlusher(10 * 10000)
      rvls.spinalSimTime(10000)
    }
    val konataBackend = traceKonata.option(new Backend(new File(currentTestPath, "konata.log")))
    delayed(1)(konataBackend.foreach(_.spinalSimFlusher(10 * 10000))) // Delayed to ensure this is registred last
    val probe = new VexiiRiscvProbe(dut.cpu.logic.core, konataBackend, withRvls)
    if (withRvlsCheck) probe.add(rvls)
    probe.backends.foreach { b =>
      b.addRegion(0, 0, 0x80000000l, 0x10000) // mem
      b.addRegion(0, 1, 0x10000000l, 0x10000000l) // io
    }
    probe.backends.foreach(_.loadElf(0, new File("ext/NaxSoftware/soc/uart/build/rv32ima/uart.elf")))
  }
}
