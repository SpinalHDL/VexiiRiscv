package vexiiriscv.tester

import rvls.spinal.{FileBackend, RvlsBackend}
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.Elf
import spinal.lib.misc.test.DualSimTracer
import spinal.lib.sim.{FlowDriver, SparseMemory, StreamMonitor, StreamReadyRandomizer}
import vexiiriscv._
import vexiiriscv.fetch.PcService
import vexiiriscv.riscv.Riscv

import java.io.{File, IOException, InputStream, OutputStream, OutputStreamWriter, PrintStream, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import vexiiriscv.test.konata.Backend
import vexiiriscv.test.{PeripheralEmulator, VexiiRiscvProbe}

import java.net.{ServerSocket, Socket}
import java.util.Scanner


class TestOptions{
  var dualSim = false // Double simulation, one ahead of the other which will trigger wave capture of the second simulation when it fail
  var traceWave = false
  var traceKonata = false
  var traceRvlsLog = false
  var traceSpikeLog = false
  var printStats = false
  var withProbe = true
  var simSpeedPrinter = Option.empty[Double]
  var withRvls = new File("ext/rvls/build/apps/rvls.so").exists()
  var withRvlsCheck = withRvls
//  var withKonata = true
  var failAfter, passAfter = Option.empty[Long]
  var startSymbol = Option.empty[String]
  var startSymbolOffset = 0l
  val bins = ArrayBuffer[(Long, File)]()
  val elfs = ArrayBuffer[File]()
  var testName = Option.empty[String]

  def getTestName() = testName.getOrElse("test")

  if(!withRvls) SpinalWarning("RVLS not detected")

  def addElf(f : File) : this.type = { elfs += f; this }
  def setFailAfter(time : Long) : this.type = { failAfter = Some(time); this }


  def addOptions(parser : scopt.OptionParser[Unit]): Unit = {
    import parser._
    opt[String]("name") action { (v, c) => testName = Some(v) }
    opt[Unit]("dual-sim") action { (v, c) => dualSim = true }
    opt[Unit]("with-wave") action { (v, c) => traceWave = true }
    opt[Unit]("with-konata") action { (v, c) => traceKonata = true }
    opt[Unit]("with-rvls-log") action { (v, c) => traceRvlsLog = true }
    opt[Unit]("with-spike-log") action { (v, c) => traceSpikeLog = true }
    opt[Unit]("print-stats") action { (v, c) => printStats = true }
    opt[Unit]("trace-all") action { (v, c) => traceRvlsLog = true; traceKonata = true; traceWave = true; traceSpikeLog = true; printStats = true }
    opt[Unit]("no-probe") action { (v, c) => withProbe = false; }
    opt[Unit]("no-rvls-check") action { (v, c) => withRvlsCheck = false;  }
    opt[Long]("fail-after") action { (v, c) => failAfter = Some(v) }
    opt[Long]("pass-after") action { (v, c) => passAfter = Some(v) }
    opt[Double]("sim-speed-printer") action { (v, c) => simSpeedPrinter = Some(v) }
    opt[Seq[String]]("load-bin") unbounded() action { (v, c) => bins += java.lang.Long.parseLong(v(0), 16) -> new File(v(1)) }
    opt[String]("load-elf") unbounded() action { (v, c) => elfs += new File(v) }
    opt[String]("start-symbol") action { (v, c) => startSymbol = Some(v) }
    opt[Long]("start-symbol-offset") action { (v, c) => startSymbolOffset = v }
  }

  def test(compiled : SimCompiled[VexiiRiscv]): Unit = {
    dualSim match {
      case true => DualSimTracer.withCb(compiled, window = 50000 * 10, seed = 2)(test)
      case false => compiled.doSimUntilVoid(name = getTestName(), seed = 2) { dut => disableSimWave(); test(dut, f => f) }
    }
  }

  def test(dut : VexiiRiscv, onTrace : (=> Unit) => Unit = cb => {}) : Unit = {
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    simSpeedPrinter.foreach(cd.forkSimSpeedPrinter)

    failAfter.map(delayed(_)(simFailure("Reached Timeout")))
    passAfter.map(delayed(_)(simSuccess()))

//    fork{
//      while(true){
//        enableSimWave()
//        sleep(1000)
//        disableSimWave()
//        sleep(100000)
//      }
//    }

    val xlen = dut.database(Riscv.XLEN)

    // Rvls will check that the CPUs are doing things right
    val rvls = withRvlsCheck generate new RvlsBackend(new File(currentTestPath))
    if (withRvlsCheck) {
      rvls.spinalSimFlusher(10 * 10000)
      rvls.spinalSimTime(10000)
    }

    val konataBackend = traceKonata.option(new Backend(new File(currentTestPath, "konata.log")))
    delayed(1)(konataBackend.foreach(_.spinalSimFlusher(10 * 10000))) // Delayed to ensure this is registred last

    // Collect traces from the CPUs behaviour
    val probe = new VexiiRiscvProbe(dut, konataBackend, withRvls)
    if (withRvlsCheck) probe.add(rvls)
    probe.enabled = withProbe
    probe.trace = false

    // Things to enable when we want to collect traces
    val tracerFile = traceRvlsLog.option(new FileBackend(new File(currentTestPath, "tracer.log")))
    onTrace {
      if(traceWave) enableSimWave()
      if (withRvlsCheck && traceSpikeLog) rvls.debug()
      if(traceKonata) probe.trace = true

      tracerFile.foreach{f =>
        f.spinalSimFlusher(10 * 10000)
        f.spinalSimTime(10000)
        probe.add(f)
      }

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
      if (withRvlsCheck) rvls.loadBin(offset, file)
      tracerFile.foreach(_.loadBin(0, file))
    }

    // load elfs
    for (file <- elfs) {
      val elf = new Elf(file, xlen)
      elf.load(mem, 0)
      if (withRvlsCheck) rvls.loadElf(0, elf.f)
      tracerFile.foreach(_.loadElf(0, elf.f))

      startSymbol.foreach(symbol => fork{
        val pc = elf.getSymbolAddress(symbol) + startSymbolOffset

        waitUntil(cd.resetSim.toBoolean == false); sleep(1)
        println(f"set harts pc to 0x$pc%x")
        dut.host[PcService].simSetPc(pc)
        for(hartId <- probe.hartsIds) probe.backends.foreach(_.setPc(hartId, pc))
      })

      val withPass = elf.getELFSymbol("pass") != null
      val withFail = elf.getELFSymbol("fail") != null
      if (withPass || withFail) {
        val passSymbol = if(withPass) elf.getSymbolAddress("pass") else -1
        val failSymbol = if(withFail) elf.getSymbolAddress("fail") else -1
        probe.commitsCallbacks += { (hartId, pc) =>
          if (pc == passSymbol) delayed(1)(simSuccess())
          if (pc == failSymbol) delayed(1)(simFailure("Software reach the fail symbole :("))
        }
      }
    }

    val peripheral = new PeripheralEmulator(0x10000000, null, null){
      override def getClintTime(): Long = probe.cycle
    }

    val fclp = dut.host.get[fetch.CachelessPlugin].map { p =>
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

    val lsclp = dut.host.get[execute.LsuCachelessPlugin].map { p =>
      val bus = p.logic.bus
      val cmdReady = StreamReadyRandomizer(bus.cmd, cd)

      case class Access(write : Boolean, address: Long, data : Array[Byte], bytes : Int, io : Boolean)
      val pending = mutable.Queue[Access]()

      val cmdMonitor = StreamMonitor(bus.cmd, cd) { p =>
        val bytes = 1 << bus.cmd.size.toInt
        val address = p.address.toLong
        val offset = address.toInt & (bytes-1)
        pending.enqueue(Access(p.write.toBoolean, address, p.data.toBytes.drop(offset).take(bytes), bytes, p.io.toBoolean))
      }
      val rspDriver = FlowDriver(bus.rsp, cd) { p =>
        val doIt = pending.nonEmpty
        if (doIt) {
          val cmd = pending.dequeue()
          if (cmd.io) {
            p.error #= peripheral.access(cmd.write, cmd.address, cmd.data)
            if(!cmd.write) {
              val bytes = new Array[Byte](p.p.dataWidth / 8)
              simRandom.nextBytes(bytes)
              Array.copy(cmd.data, 0, bytes, cmd.address.toInt & (p.p.dataWidth / 8 - 1), cmd.data.size)
              p.data #= bytes
            }
          } else {
            p.error #= false
            cmd.write match {
              case true => {
                mem.write(cmd.address, cmd.data)
                p.data.randomize()
              }
              case false => {
                val bytes = new Array[Byte](p.p.dataWidth / 8)
                simRandom.nextBytes(bytes)
                mem.readBytes(cmd.address, cmd.bytes, bytes, cmd.address.toInt & (p.p.dataWidth / 8 - 1))
                p.data #= bytes
              }
            }
          }
        }
        doIt
      }

      cmdReady.setFactor(2.0f)
      rspDriver.setFactor(2.0f)
    }

    if(printStats) onSimEnd{
      println(probe.getStats())
    }
  }
}

object TestBench extends App{
  doIt()

  def doIt(param : ParamSimple = new ParamSimple()) {
    val testOpt = new TestOptions()

    val genConfig = SpinalConfig()
    genConfig.includeSimulation

    val simConfig = SpinalSimConfig()
    simConfig.withFstWave
    simConfig.withTestFolder
    simConfig.withConfig(genConfig)

    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      testOpt.addOptions(this)
      param.addOptions(this)
    }.parse(args, Unit).nonEmpty)

    println(s"With Vexiiriscv parm :\n - ${param.getName()}")
    val compiled = TestBench.synchronized { // To avoid to many calls at the same time
      simConfig.compile(VexiiRiscv(param.plugins()))
    }
    testOpt.test(compiled)
    Thread.sleep(10)
  }
}

object TestBenchDebug extends App{
  val param = new ParamSimple()
  param.regFileSync = true
  TestBench.doIt()
}

//echo '--load-elf ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf   --with-all' | nc localhost  8189
object TestBenchServer extends App{
  val simConfig = SpinalSimConfig()
  simConfig.withFstWave
  simConfig.withTestFolder

  val param = new ParamSimple()
  val compiled = simConfig.compile(VexiiRiscv(param.plugins()))
  val serverSocket = new ServerSocket(8189)
  var i = 0
  println("Waiting for connections")
  while (true) {
    val incoming = serverSocket.accept
    new TestBenchServerConnection(incoming, compiled)
    i += 1
  }
}


class TestBenchServerConnection(incoming: Socket, compiled : SimCompiled[VexiiRiscv]) extends Thread {
  this.start()
  override def run() = {
    try try {
      val inputStream = incoming.getInputStream
      val outputStream = incoming.getOutputStream
      val in = new Scanner(inputStream)
      val out = new PrintWriter(outputStream, true) /* autoFlush */
      var command = ""
      command = in.nextLine
      out.println("got " + command)
      println("got " + command)
      val args = command.split("\\s+")
      Console.withOut(outputStream) {
        Console.withErr(outputStream) {
          val testOpt = new TestOptions()
          assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
            help("help").text("prints this usage text")
            testOpt.addOptions(this)
          }.parse(args, Unit).nonEmpty)
          testOpt.test(compiled)
          Thread.sleep(100)
        }
      }
    } catch {
      case e: InterruptedException =>
        throw new RuntimeException(e)
    } finally incoming.close()
    catch {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }
}