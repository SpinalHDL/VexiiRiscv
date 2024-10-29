package vexiiriscv.soc.micro

import rvls.spinal.RvlsBackend
import spinal.core._
import spinal.core.sim._
import spinal.core.fiber._
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.misc.Elf
import vexiiriscv.test.VexiiRiscvProbe

import java.io.File

/**
 * To connect with openocd jtag :
 * - src/openocd -f $VEXIIRISCV/src/main/tcl/openocd/vexiiriscv_sim.tcl
 */

object MicroSocSim extends App{
  var traceKonata = false
  var withRvlsCheck = false
  var elf: File = null
  val sim = SimConfig
  var speedPrinterPeriod = Option.empty[Double]
  sim.withTimeSpec(1 ns, 1 ps)
  val p = new MicroSocParam()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    opt[String]("load-elf") action { (v, c) => elf = new File(v) }
    opt[Unit]("trace-konata") action { (v, c) => traceKonata = true }
    opt[Unit]("check-rvls") action { (v, c) => withRvlsCheck = true }
    opt[Double]("speed-printer") action { (v, c) => speedPrinterPeriod = Some(v) }
    sim.addOptions(this)
    p.addOptions(this)
  }.parse(args, Unit).nonEmpty)
  p.legalize()


  sim.compile(new MicroSoc(p){
    Fiber patch{
      system.ram.thread.logic.mem.simPublic()
    }
  }).doSimUntilVoid("test", seed = 42){dut =>
    dut.socCtrl.systemClkCd.forkStimulus()
    dut.socCtrl.asyncReset #= true
    delayed(100 ns)(dut.socCtrl.asyncReset #= false)

    speedPrinterPeriod.foreach(SimSpeedPrinter(dut.socCtrl.systemClkCd, _))

    val uartBaudPeriod = hzToLong(115200 Hz)
    val uartTx = UartDecoder(
      uartPin = dut.system.peripheral.uart.logic.uart.txd,
      baudPeriod = uartBaudPeriod
    )
    val uartRx = UartEncoder(
      uartPin = dut.system.peripheral.uart.logic.uart.rxd,
      baudPeriod = uartBaudPeriod
    )

    val konata = traceKonata.option(
      new vexiiriscv.test.konata.Backend(new File(currentTestPath, "konata.log")).spinalSimFlusher(hzToLong(1000 Hz))
    )
    val probe = new VexiiRiscvProbe(
      cpu = dut.system.cpu.logic.core,
      kb = konata
    )

    if (withRvlsCheck) probe.add(new RvlsBackend(new File(currentTestPath)).spinalSimFlusher(hzToLong(1000 Hz)))

    probe.autoRegions()

    if(p.socCtrl.withJtagTap) {
      probe.checkLiveness = false
      spinal.lib.com.jtag.sim.JtagRemote(dut.socCtrl.debugModule.tap.jtag, hzToLong(p.socCtrl.systemFrequency)*4)
    }


    if(elf != null) {
      new Elf(elf, p.vexii.xlen).load(dut.system.ram.thread.logic.mem, 0x80000000l)
      probe.backends.foreach(_.loadElf(0, elf))
    }
  }
}