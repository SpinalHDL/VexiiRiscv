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
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug.{DebugModuleFiber, DebugModuleSocFiber}
import spinal.lib.eda.bench.Rtl
import spinal.lib.misc.{Elf, PathTracer, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA
import vexiiriscv.ParamSimple
import vexiiriscv.execute.SrcPlugin
import vexiiriscv.misc.TrapPlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.test.VexiiRiscvProbe

import java.io.File
import scala.collection.mutable.ArrayBuffer

// This class will carry all the parameter of the SoC
class MicroSocParam {
  var ramBytes = 16 KiB
  val vexii = new ParamSimple()
  var demoPeripheral = Option.empty[PeripheralDemoParam]
  val socCtrl = new SocCtrlParam()

  // Provide some sane default
  vexii.fetchForkAt = 1
  vexii.lsuPmaAt = 1
  vexii.lsuForkAt = 1
  vexii.relaxedBranch = true
  socCtrl.withJtagTap = true

  // This is a command line parser utility, so you can customize the SoC using command line arguments to feed parameters
  def addOptions(parser: scopt.OptionParser[Unit]): Unit = {
    import parser._
    vexii.addOptions(parser)
    socCtrl.addOptions(parser)
    opt[Int]("ram-bytes") action { (v, c) => ramBytes = v }
    opt[Int]("demo-peripheral") action { (v, c) => demoPeripheral = Some(new PeripheralDemoParam(
      ledWidth = v
    ))}
  }

  def legalize(): Unit = {
    vexii.privParam.withDebug = socCtrl.withDebug
  }
}


// Lets define our SoC toplevel
class MicroSoc(p : MicroSocParam) extends Component {
  // socCtrl will provide clocking, reset controllers and debugModule (through jtag) to our SoC
  val socCtrl = new SocCtrl(p.socCtrl)

  val system = new ClockingArea(socCtrl.system.cd) {
    // Let's define our main tilelink bus on which the CPU, RAM and peripheral "portal" will be plugged later.
    val mainBus = tilelink.fabric.Node()

    val cpu = new TilelinkVexiiRiscvFiber(p.vexii.plugins())
    if(p.socCtrl.withDebug) socCtrl.debugModule.bindHart(cpu)
    mainBus << cpu.buses
    cpu.dBus.setDownConnection(a = StreamPipe.S2M) // Let's add a bit of pipelining on the cpu.dBus to increase FMax

    val ram = new tilelink.fabric.RamFiber(p.ramBytes)
    ram.up at 0x80000000l of mainBus

    // Handle all the IO / Peripheral things
    val peripheral = new Area {
      // Some peripheral may require to have an access as big as the CPU XLEN, so, lets define a bus which ensure it.
      val busXlen = Node()
      busXlen.forceDataWidth(p.vexii.xlen)
      busXlen << mainBus
      busXlen.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)

      // Most peripheral will work with a 32 bits data bus.
      val bus32 = Node()
      bus32.forceDataWidth(32)
      bus32 << busXlen

      // The clint is a regular RISC-V timer peripheral
      val clint = new TilelinkClintFiber()
      clint.node at 0x10010000 of busXlen

      // The clint is a regular RISC-V interrupt controller
      val plic = new TilelinkPlicFiber()
      plic.node at 0x10C00000 of bus32

      val uart = new TilelinkUartFiber()
      uart.node at 0x10001000 of bus32
      plic.mapUpInterrupt(1, uart.interrupt)

      val demo = p.demoPeripheral.map(new PeripheralDemoFiber(_){
        node at 0x10002000 of bus32
        plic.mapUpInterrupt(2, interrupt)
      })

      val cpuPlic = cpu.bind(plic)
      val cpuClint = cpu.bind(clint)
    }
  }
}

object MicroSocGen extends App{
  val p = new MicroSocParam()

  assert(new scopt.OptionParser[Unit]("MicroSoc") {
    p.addOptions(this)
  }.parse(args, Unit).nonEmpty)
  p.legalize()

  val report = SpinalVerilog(new MicroSoc(p))
}

object MicroSocSynt extends App{
  import spinal.lib.eda.bench._
  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog{
    val p = new MicroSocParam()
    new MicroSoc(p) {
      socCtrl.systemClk.setName("clk")
      setDefinitionName("MicroSoc")
    }
  })

  val targets = ArrayBuffer[Target]()
  //  targets ++=  XilinxStdTargets(withFMax = true, withArea = true)
  //  targets ++= AlteraStdTargets()
  targets ++= EfinixStdTargets(withFMax = true, withArea = true)

  Bench(rtls, targets)
}

/**
 * To connect with openocd jtag :
 * - src/openocd -f $VEXIIRISCV/src/main/tcl/openocd/vexiiriscv_sim.tcl
 */

object MicroSocSim extends App{
  var traceKonata = false
  var withRvlsCheck = false
  var elf: File = null
  val sim = SimConfig
  sim.withTimeSpec(1 ns, 1 ps)
  val p = new MicroSocParam()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    opt[String]("load-elf") action { (v, c) => elf = new File(v) }
    opt[Unit]("trace-konata") action { (v, c) => traceKonata = true }
    opt[Unit]("check-rvls") action { (v, c) => withRvlsCheck = true }
    p.addOptions(this)
    sim.addOptions(this)
  }.parse(args, Unit).nonEmpty)


  sim.compile(new MicroSoc(p){
    Fiber patch{
      system.ram.thread.logic.mem.simPublic()
    }
  }).doSimUntilVoid("test", seed = 42){dut =>
    dut.socCtrl.systemClkCd.forkStimulus()
    dut.socCtrl.asyncReset #= true
    delayed(100 ns)(dut.socCtrl.asyncReset #= false)

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




//  val h = report.toplevel.main.cpu.logic.core.host
//  val path = PathTracer.impl(h[SrcPlugin].logic.addsub.rs2Patched, h[TrapPlugin].logic.harts(0).trap.pending.state.tval)
//  println(path.report)