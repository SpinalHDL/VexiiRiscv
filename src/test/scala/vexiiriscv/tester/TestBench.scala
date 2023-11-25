package vexiiriscv.tester

import rvls.spinal.{FileBackend, RvlsBackend}
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.Elf
import spinal.lib.misc.test.DualSimTracer
import spinal.lib.sim.{FlowDriver, SparseMemory, StreamMonitor, StreamReadyRandomizer}
import vexiiriscv._
import vexiiriscv.misc.VexiiRiscvProbe
import vexiiriscv.riscv.Riscv

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import vexiiriscv.misc.konata


class TestOptions{
  var dualSim = false // Double simulation, one ahead of the other which will trigger wave capture of the second simulation when it fail
  var traceIt = false
  var withProbe = true
  var withRvls = new File("ext/rvls/build/apps/rvls.so").exists()
  var withRvlsCheck = withRvls
//  var withKonata = true
  var failAfter, passAfter = Option.empty[Long]
  val bins = ArrayBuffer[(Long, String)]()
  val elfs = ArrayBuffer[String]()

  if(!withRvls) SpinalWarning("RVLS not detected")

  def addOptions(parser : scopt.OptionParser[Unit]): Unit = {
    import parser._
    opt[Unit]("dual-sim") action { (v, c) => dualSim = true }
    opt[Unit]("trace") action { (v, c) => traceIt = true }
    opt[Unit]("no-probe") action { (v, c) => withProbe = false; }
    opt[Unit]("no-rvls-check") action { (v, c) => withRvlsCheck = false;  }
    opt[Long]("failAfter") action { (v, c) => failAfter = Some(v) }
    opt[Long]("passAfter") action { (v, c) => passAfter = Some(v) }
    opt[Seq[String]]("load-bin") unbounded() action { (v, c) => bins += (java.lang.Long.parseLong(v(0), 16) -> v(1)) }
    opt[String]("load-elf") unbounded() action { (v, c) => elfs += v }
  }

  def test(compiled : SimCompiled[VexiiRiscv]): Unit = {
    dualSim match {
      case true => DualSimTracer.withCb(compiled, window = 50000 * 10, seed = 2)(test)
      case false => compiled.doSimUntilVoid(name = s"test", seed = 2) { dut => disableSimWave(); test(dut, f => if (traceIt) f) }
    }
  }

  def test(dut : VexiiRiscv, onTrace : (=> Unit) => Unit = cb => {}) : Unit = {
    val cd = dut.clockDomain
    cd.forkStimulus(10)

    failAfter.map(delayed(_)(simFailure("Reached Timeout")))
    passAfter.map(delayed(_)(simSuccess()))

    val xlen = dut.database(Riscv.XLEN)

    // Rvls will check that the CPUs are doing things right
    val rvls = withRvlsCheck generate new RvlsBackend(new File(simCompiled.compiledPath, currentTestName))
    if (withRvlsCheck) {
      rvls.spinalSimFlusher(10 * 10000)
      rvls.spinalSimTime(10000)
    }

    val konataBackend = new konata.Backend(new File(simCompiled.compiledPath, "konata.log"))
    delayed(1)(konataBackend.spinalSimFlusher(10 * 10000)) // Delayed to ensure this is registred last

    // Collect traces from the CPUs behaviour
    val probe = new VexiiRiscvProbe(dut, konataBackend, withRvls)
    if (withRvlsCheck) probe.add(rvls)
    probe.enabled = withProbe

    // Things to enable when we want to collect traces
    val tracerFile = new FileBackend(new File(new File(simCompiled.compiledPath, currentTestName), "tracer.log"))
    onTrace {
      enableSimWave()
      if (withRvlsCheck) rvls.debug()


      tracerFile.spinalSimFlusher(10 * 10000)
      tracerFile.spinalSimTime(10000)
      probe.add(tracerFile)
      val r = probe.backends.reverse
      probe.backends.clear()
      probe.backends ++= r
    }

    probe.backends.foreach { b =>
      b.addRegion(0, 0, 0x20000000l, 0xE0000000l) // mem
      b.addRegion(0, 1, 0x10000000l, 0x10000000l) // io
    }

    val mem = SparseMemory(seed = 0)
    // Load the binaries
    for ((offset, file) <- bins) {
      mem.loadBin(offset - 0x80000000l, file)
      if (withRvlsCheck) rvls.loadBin(offset, new File(file))
      tracerFile.loadBin(0, new File(file))
    }

    // load elfs
    for (file <- elfs) {
      val elf = new Elf(new File(file), xlen)
      elf.load(mem, 0)
      if (withRvlsCheck) rvls.loadElf(0, elf.f)
      tracerFile.loadElf(0, elf.f)

      if (elf.getELFSymbol("pass") != null && elf.getELFSymbol("fail") != null) {
        val passSymbol = elf.getSymbolAddress("pass")
        val failSymbol = elf.getSymbolAddress("fail")
        probe.commitsCallbacks += { (hartId, pc) =>
          if (pc == passSymbol) delayed(1) {
            simSuccess()
          }
          if (pc == failSymbol) delayed(1)(simFailure("Software reach the fail symbole :("))
        }
      }
    }

    val fcp = dut.host.get[fetch.CachelessPlugin].map { p =>
      val bus = p.logic.bus
      val cmdReady = StreamReadyRandomizer(bus.cmd, cd)

      case class Cmd(address: Long, id: Int)
      val pending = mutable.ArrayBuffer[Cmd]()

      val cmdMonitor = StreamMonitor(bus.cmd, cd) { p =>
        pending += Cmd(p.address.toLong, p.id.toInt)
      }
      val rspDriver = FlowDriver(bus.rsp, cd) { p =>
        val doIt = pending.nonEmpty
        if (doIt) {
          val cmd = pending.randomPop()
          p.word #= mem.readBytes(cmd.address, p.p.dataWidth / 8)
          p.id #= cmd.id
          p.error #= false
        }
        doIt
      }

      cmdReady.setFactor(2.0f)
      rspDriver.setFactor(2.0f)
    }
  }
}

object TestBench extends App{
  val testOpt = new TestOptions()

  val simConfig = SpinalSimConfig()
  simConfig.withFstWave

  val param = new ParamSimple()
  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    testOpt.addOptions(this)
  }.parse(args, Unit).nonEmpty)

  val compiled = simConfig.compile(VexiiRiscv(param.plugins()))
  testOpt.test(compiled)
  Thread.sleep(100)
}
