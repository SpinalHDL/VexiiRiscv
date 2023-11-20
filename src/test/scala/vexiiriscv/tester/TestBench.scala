package vexiiriscv.tester

import rvls.spinal.{FileBackend, RvlsBackend}
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.Elf
import spinal.lib.misc.test.DualSimTracer
import spinal.lib.sim.{FlowDriver, SparseMemory, StreamMonitor, StreamReadyRandomizer}
import vexiiriscv._
import vexiiriscv.riscv.Riscv

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class TestOptions{
  var dualSim = false // Double simulation, one ahead of the other which will trigger wave capture of the second simulation when it fail
  var traceIt = false
  var withRvls = true
  var failAfter, passAfter = Option.empty[Long]
  val bins = ArrayBuffer[(Long, String)]()
  val elfs = ArrayBuffer[String]()

  def addOptions(parser : scopt.OptionParser[Unit]): Unit = {
    import parser._
    opt[Unit]("dual-sim") action { (v, c) => dualSim = true }
    opt[Unit]("trace") action { (v, c) => traceIt = true }
    opt[Unit]("no-rvls") action { (v, c) => withRvls = false }
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
    val rvls = withRvls generate new RvlsBackend(new File(simCompiled.compiledPath, currentTestName))
    if (withRvls) {
      rvls.spinalSimFlusher(10 * 10000)
      rvls.spinalSimTime(10000)
    }

    // Things to enable when we want to collect traces
    onTrace {
      enableSimWave()
      if (withRvls) rvls.debug()

      val tracerFile = new FileBackend(new File(new File(simCompiled.compiledPath, currentTestName), "tracer.log"))
      tracerFile.spinalSimFlusher(10 * 10000)
      tracerFile.spinalSimTime(10000)
//      naxes.foreach { hart =>
//        hart.add(tracerFile)
//        val r = hart.backends.reverse
//        hart.backends.clear()
//        hart.backends ++= r
//      }
    }

    val mem = SparseMemory(seed = 0)
    // Load the binaries
    for ((offset, file) <- bins) {
      mem.loadBin(offset - 0x80000000l, file)
      if (withRvls) rvls.loadBin(offset, new File(file))
    }

    // load elfs
    for (file <- elfs) {
      val elf = new Elf(new File(file), xlen)
      elf.load(mem, 0)
      if (withRvls) rvls.loadElf(0, elf.f)

      if (elf.getELFSymbol("pass") != null && elf.getELFSymbol("fail") != null) {
        val passSymbol = elf.getSymbolAddress("pass")
        val failSymbol = elf.getSymbolAddress("fail")
//          naxes.foreach { nax =>
//            nax.commitsCallbacks += { (hartId, pc) =>
//              if (pc == passSymbol) delayed(1) {
//                dut.naxes.foreach { nax =>
//                  println(s"Hart $hartId")
//                  nax.plugins.foreach {
//                    case p: FetchCachePlugin => println("- i$ refill = " + p.logic.refill.pushCounter.toLong)
//                    case p: DataCachePlugin => println("- d$ refill = " + p.logic.cache.refill.pushCounter.toLong)
//                    case _ =>
//                  }
//                }
//
//                simSuccess()
//              }
//              if (pc == failSymbol) delayed(1)(simFailure("Software reach the fail symbole :("))
//            }
//          }
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
}