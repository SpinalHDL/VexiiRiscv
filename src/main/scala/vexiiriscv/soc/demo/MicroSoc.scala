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
import spinal.lib.cpu.riscv.debug.DebugModuleFiber
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

class MicroSocParam {
  var withJtagTap = true
  var withJtagInstruction = false

  def withDebug = withJtagTap ||  withJtagInstruction
}

class DebugModuleSocFiber(withJtagInstruction : Boolean) extends Area{
  val tck = withJtagInstruction generate in(Bool())
  val dm = new DebugModuleFiber()
  val tap = (!withJtagInstruction) generate dm.withJtagTap()
  val instruction = (withJtagInstruction) generate ClockDomain(tck)(dm.withJtagInstruction())

  def bindHart(cpu: RiscvHart) = {
    dm.bindHart(cpu)
  }
}

class MicroSoc(p : MicroSocParam) extends Component {
  val asyncReset = in Bool()
  val cd100 = ClockDomain.external("cd100", withReset = false, frequency = FixedFrequency(100 MHz))

  val debugResetCtrl = cd100(new ResetCtrlFiber().addAsyncReset(asyncReset, HIGH))
  val mainResetCtrl  = cd100(new ResetCtrlFiber().addAsyncReset(debugResetCtrl))

  val debug = p.withDebug generate debugResetCtrl.cd(new DebugModuleSocFiber(p.withJtagInstruction){
    mainResetCtrl.addSyncRelaxedReset(dm.ndmreset, HIGH)
  })


  val main = mainResetCtrl.cd on new Area {
    val sharedBus = tilelink.fabric.Node()

    val param = new ParamSimple()
    param.fetchForkAt = 1
    param.lsuPmaAt = 1
    param.lsuForkAt = 1
    param.relaxedBranch = true
    param.privParam.withDebug = p.withDebug
    
    val plugins = param.plugins()
    val cpu = new TilelinkVexiiRiscvFiber(plugins)
    sharedBus << cpu.buses
    cpu.dBus.setDownConnection(a = StreamPipe.S2M)
    if(p.withDebug) debug.bindHart(cpu)

    val ram = new tilelink.fabric.RamFiber(16 KiB)
    ram.up at 0x80000000l of sharedBus

    // Handle all the IO / Peripheral things
    val peripheral = new Area {
      val busXlen = Node().forceDataWidth(param.xlen)
      busXlen << sharedBus
      busXlen.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)

      val bus32 = Node().forceDataWidth(32)
      bus32 << busXlen

      val clint = new TilelinkClintFiber()
      clint.node at 0x10010000 of busXlen

      val plic = new TilelinkPlicFiber()
      plic.node at 0x10C00000 of bus32

      val uart = new TilelinkUartFiber()
      uart.node at 0x10001000 of bus32
      plic.mapUpInterrupt(1, uart.interrupt)

      val cpuPlic = cpu.bind(plic)
      val cpuClint = cpu.bind(clint)
    }
  }
}

object MicroSocGen extends App{
  val report = SpinalVerilog{
    val p = new MicroSocParam()
    new MicroSoc(p)
  }

  val h = report.toplevel.main.cpu.logic.core.host
  val path = PathTracer.impl(h[SrcPlugin].logic.addsub.rs2Patched, h[TrapPlugin].logic.harts(0).trap.pending.state.tval)
  println(path.report)
}

object MicroSocSynt extends App{
  import spinal.lib.eda.bench._
  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog{
    val p = new MicroSocParam()
    new MicroSoc(p) {
      cd100.readClockWire.setName("clk")
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
  var elf: Elf = null
  val sim = SimConfig
  sim.withTimeSpec(1 ns, 1 ps)
  val p = new MicroSocParam()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    opt[String]("load-elf") action { (v, c) => elf = new Elf(new File(v), 32) }
    opt[Unit]("trace-konata") action { (v, c) => traceKonata = true }
    opt[Unit]("check-rvls") action { (v, c) => withRvlsCheck = true }
    sim.addOptions(this)
  }.parse(args, Unit).nonEmpty)


  sim.compile(new MicroSoc(p){
    hardFork{
      main.ram.thread.logic.mem.simPublic()
    }
  }).doSimUntilVoid("test", seed = 42){dut =>
    dut.cd100.forkStimulus()
    dut.asyncReset #= true
    delayed(100 ns)(dut.asyncReset #= false)

    val uartBaudPeriod = hzToLong(115200 Hz)
    val uartTx = UartDecoder(
      uartPin = dut.main.peripheral.uart.logic.uart.txd,
      baudPeriod = uartBaudPeriod
    )
    val uartRx = UartEncoder(
      uartPin = dut.main.peripheral.uart.logic.uart.rxd,
      baudPeriod = uartBaudPeriod
    )

    val konata = traceKonata.option(
      new vexiiriscv.test.konata.Backend(new File(currentTestPath, "konata.log")).spinalSimFlusher(hzToLong(1000 Hz))
    )
    val probe = new VexiiRiscvProbe(
      cpu = dut.main.cpu.logic.core,
      kb = konata
    )

    if (withRvlsCheck) probe.add(new RvlsBackend(new File(currentTestPath)).spinalSimFlusher(hzToLong(1000 Hz)))

    probe.autoRegions()

    if(p.withJtagTap) {
      probe.checkLiveness = false
      spinal.lib.com.jtag.sim.JtagRemote(dut.debug.tap.jtag, hzToLong(dut.cd100.frequency.getValue)*4)
    }


    if(elf != null) {
      elf.load(dut.main.ram.thread.logic.mem, 0x80000000l)
      probe.backends.foreach(_.loadElf(0, elf.f))
    }
  }
}

