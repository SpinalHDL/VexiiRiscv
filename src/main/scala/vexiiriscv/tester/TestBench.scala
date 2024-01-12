package vexiiriscv.tester

import rvls.spinal.{FileBackend, RvlsBackend}
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.Elf
import spinal.lib.misc.test.DualSimTracer
import spinal.lib.sim.{FlowDriver, SparseMemory, StreamMonitor, StreamReadyRandomizer}
import vexiiriscv._
import vexiiriscv.fetch.PcService
import vexiiriscv.misc.PrivilegedPlugin
import vexiiriscv.riscv.Riscv
import vexiiriscv.test.konata.Backend
import vexiiriscv.test.{PeripheralEmulator, VexiiRiscvProbe}

import java.io.{File, IOException, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TestArgs{
  val args = ArrayBuffer[String]()
  def dualSim() : this.type = {args ++= List("--dual-sim"); this }
  def withWave() : this.type = {args ++= List("--with-wave"); this }
  def withKonata() : this.type = {args ++= List("--with-konata"); this }
  def withRvlsLog() : this.type = {args ++= List("--with-rvls-log"); this }
  def withSpikeLog() : this.type = {args ++= List("--with-spike-log"); this }
  def printStats() : this.type = {args ++= List("--print-stats"); this }
  def traceAll() : this.type = {args ++= List("--trace-all"); this }
  def noProbe() : this.type = {args ++= List("--no-probe"); this }
  def noRvlsCheck() : this.type = {args ++= List("--no-rvls-check"); this }
  def noStdin() : this.type = {args ++= List("--no-stdin"); this }
  def fsmSuccess() : this.type = {args ++= List("--fsm-success"); this }

  def name(name : String) : this.type = {args ++= List("--name", name); this }
  def failAfter(value : Long) : this.type = {args ++= List("--fail-after", value.toString); this }
  def passAfter(value : Long) : this.type = {args ++= List("--pass-after", value.toString); this }
  def simSpeedPrinter(value : Double) : this.type = {args ++= List("--sim-speed-printer", value.toString); this }
  def loadElf(value : String) : this.type = {args ++= List("--load-elf", value); this }
  def loadElf(value : File) : this.type = loadElf(value.getAbsolutePath)
  def startSymbol(value : String) : this.type = {args ++= List("--start-symbol", value); this }
  def passSymbol(value : String) : this.type = {args ++= List("--pass-symbol", value); this }
  def startSymbolOffset(value : Int) : this.type = {args ++= List("--start-symbol-offset", value.toString); this }
  def fsmGetc(value : String) : this.type = {args ++= List("--fsm-getc", value); this }
  def fsmPutc(value : String) : this.type = {args ++= List("--fsm-putc", value); this }
  def fsmPutcLr() : this.type = {args ++= List("--fsm-putc-lr"); this }
  def fsmSleep(value : Long) : this.type = {args ++= List("--fsm-sleep", value.toString); this }
  def ibusReadyFactor(value : Double) : this.type = {args ++= List("--ibus-ready-factor", value.toString); this }
  def dbusReadyFactor(value : Double) : this.type = {args ++= List("--dbus-ready-factor", value.toString); this }

  def loadBin(address : Long, file : String) : this.type = {args ++= List("--load-bin", f"0x${address.toHexString},$file"); this }
}

trait FsmHal{
  def putc(value : String) : Unit
  def next() : Unit
}
trait FsmTask{
  def start(hal : FsmHal) : Unit = {}
  def getc(hal : FsmHal, c : Char) : Unit = {}
}
class FsmGetc(value : String) extends FsmTask{
  val buffer = new StringBuilder()
  override def getc(hal : FsmHal, c: Char): Unit = {
    buffer += c
    if(buffer.toString().endsWith(value)){
      hal.next()
    }
  }
}
class FsmPutc(value : String) extends FsmTask{
  override def start(hal: FsmHal): Unit = {
    hal.putc(value)
    hal.next()
  }
}
class FsmSleep(value : Long) extends FsmTask{
  override def start(hal: FsmHal): Unit = {
    delayed(value) {
      hal.next()
    }
  }
}
class FsmSuccess extends FsmTask{
  override def start(hal: FsmHal): Unit = simSuccess()
}

class TestOptions{
  var dualSim = false // Double simulation, one ahead of the other which will trigger wave capture of the second simulation when it fail
  var traceWave = false
  var traceKonata = false
  var traceRvlsLog = false
  var traceSpikeLog = false
  var printStats = false
  var withProbe = true
  var withStdIn = true
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
  var passSymbolName = "pass"
  val fsmTasks = mutable.Queue[FsmTask]()
  var ibusReadyFactor = 1.01f
  var dbusReadyFactor = 1.01f

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
    opt[Unit]("no-stdin") action { (v, c) => withStdIn = false }
    opt[Unit]("print-stats") action { (v, c) => printStats = true }
    opt[Unit]("trace-all") action { (v, c) => traceRvlsLog = true; traceKonata = true; traceWave = true; traceSpikeLog = true; printStats = true }
    opt[Unit]("no-probe") action { (v, c) => withProbe = false; }
    opt[Unit]("no-rvls-check") action { (v, c) => withRvlsCheck = false;  }
    opt[Long]("fail-after") action { (v, c) => failAfter = Some(v) }
    opt[Long]("pass-after") action { (v, c) => passAfter = Some(v) }
    opt[Double]("sim-speed-printer") action { (v, c) => simSpeedPrinter = Some(v) }
    opt[Seq[String]]("load-bin") unbounded() action { (v, c) => bins += java.lang.Long.parseLong(v(0).replace("0x",""), 16) -> new File(v(1)) }
    opt[String]("load-elf") unbounded() action { (v, c) => elfs += new File(v) }
    opt[String]("start-symbol") action { (v, c) => startSymbol = Some(v) }
    opt[String]("pass-symbol") action { (v, c) => passSymbolName = v }
    opt[Long]("start-symbol-offset") action { (v, c) => startSymbolOffset = v }
    opt[Double]("ibus-ready-factor") unbounded() action { (v, c) => ibusReadyFactor = v.toFloat }
    opt[Double]("dbus-ready-factor") unbounded() action { (v, c) => dbusReadyFactor = v.toFloat }

    opt[String]("fsm-putc") unbounded() action { (v, c) => fsmTasks += new FsmPutc(v) }
    opt[Unit]("fsm-putc-lr") unbounded() action { (v, c) => fsmTasks += new FsmPutc("\n") }
    opt[String]("fsm-getc") unbounded() action { (v, c) => fsmTasks += new FsmGetc(v) }
    opt[Long]("fsm-sleep") unbounded() action { (v, c) => fsmTasks += new FsmSleep(v) }
    opt[Unit]("fsm-success") unbounded() action { (v, c) => fsmTasks += new FsmSuccess() }
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
      mem.loadBin(offset, file)
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

      val withPass = elf.getELFSymbol(passSymbolName) != null
      val withFail = elf.getELFSymbol("fail") != null
      if (withPass || withFail) {
        def trunkPc(pc : Long) = (xlen == 32).mux(pc & 0xFFFFFFFFl, pc)
        val passSymbol = if(withPass) trunkPc(elf.getSymbolAddress(passSymbolName)) else -1
        val failSymbol = if(withFail) trunkPc(elf.getSymbolAddress("fail")) else -1
        probe.commitsCallbacks += { (hartId, pc) =>
          if (pc == passSymbol) delayed(1)(simSuccess())
          if (pc == failSymbol) delayed(1)(simFailure("Software reach the fail symbole :("))
        }
      }
    }

    val priv = dut.host[PrivilegedPlugin].logic.harts(0)
    val peripheral = new PeripheralEmulator(0x10000000, priv.int.m.external, (priv.int.s != null) generate priv.int.s.external, msi = priv.int.m.software, mti = priv.int.m.timer, cd = cd){
      override def getClintTime(): Long = probe.cycle
    }
    peripheral.withStdIn = withStdIn


    val fclp = dut.host.get[fetch.FetchCachelessPlugin].map { p =>
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
          p.error #= cmd.address < 0x10000000
        }
        doIt
      }

      //TODO backpresure
      cmdReady.setFactor(ibusReadyFactor)
      rspDriver.setFactor(ibusReadyFactor)
    }

    val lsclp = dut.host.get[execute.LsuCachelessPlugin].map { p =>
      val bus = p.logic.bus
      val cmdReady = StreamReadyRandomizer(bus.cmd, cd)
      bus.cmd.ready #= true
      var reserved = false

      case class Access(write : Boolean, address: Long, data : Array[Byte], bytes : Int, io : Boolean, hartId : Int, uopId : Int, amoEnable : Boolean, amoOp : Int)
      val pending = mutable.Queue[Access]()

      val cmdMonitor = StreamMonitor(bus.cmd, cd) { p =>
        val bytes = 1 << bus.cmd.size.toInt
        val address = p.address.toLong
        val offset = address.toInt & (bytes-1)
        pending.enqueue(
          Access(
            p.write.toBoolean,
            address,
            p.data.toBytes.drop(offset).take(bytes),
            bytes,
            p.io.toBoolean,
            p.hartId.toInt,
            p.uopId.toInt,
            if(p.amoEnable != null) p.amoEnable.toBoolean else false,
            if(p.amoOp != null) p.amoOp.toInt else 0
          )
        )
      }
      val rspDriver = FlowDriver(bus.rsp, cd) { p =>
        val doIt = pending.nonEmpty
        if (doIt) {
          val cmd = pending.dequeue()

          def read(dst : Array[Byte], offset : Int): Boolean = {
            if (cmd.io) {
              assert(!cmd.amoEnable, "io amo not supported in testbench yet")
              peripheral.access(false, cmd.address, dst)
            } else {
              mem.readBytes(cmd.address, cmd.bytes, dst, offset)
              false
            }
          }

          def write(): Boolean = {
            if (cmd.io) {
              assert(!cmd.amoEnable, "io amo not supported in testbench yet")
              peripheral.access(cmd.write, cmd.address, cmd.data)
            } else {
              mem.write(cmd.address, cmd.data)
              false
            }
          }

          val bytes = new Array[Byte](p.p.dataWidth / 8)
          var error = false
          var scMiss = simRandom.nextBoolean()
          simRandom.nextBytes(bytes)
          if(!cmd.amoEnable) {
            if (cmd.write) {
              error = write()
              reserved = false
            } else {
              error = read(bytes, cmd.address.toInt & (p.p.dataWidth / 8 - 1))
            }
          } else {
            import vexiiriscv.execute.CachelessBusAmo._
            cmd.amoOp match {
              case LR => {
                error = read(bytes, cmd.address.toInt & (p.p.dataWidth / 8 - 1))
                reserved = true
              }
              case SC => {
                if(reserved) error = write()
                scMiss = !reserved
                reserved = false
              }
              case amoOp => {
                reserved = false
                def bytesToLong(a : Array[Byte]) = a.zipWithIndex.map{case (v, i) => (v.toLong & 0xFFl) << i*8}.reduce(_ | _) << cmd.bytes*8 >> cmd.bytes*8
                def unsigned(v : Long) = BigInt(v) & ((BigInt(1) << cmd.bytes*8)-1)
                val memBytes = new Array[Byte](cmd.bytes); error = read(memBytes, 0)
                val memLong = bytesToLong(memBytes)
                val rfLong = bytesToLong(cmd.data)

                var memWrite = amoOp match {
                  case AMOSWAP => rfLong
                  case AMOADD  => rfLong + memLong
                  case AMOXOR  => rfLong ^ memLong
                  case AMOAND  => rfLong & memLong
                  case AMOOR   => rfLong | memLong
                  case AMOMIN  => rfLong min memLong
                  case AMOMAX  => rfLong max memLong
                  case AMOMINU => (unsigned(rfLong) min unsigned(memLong)).toLong
                  case AMOMAXU => (unsigned(rfLong) max unsigned(memLong)).toLong
                }

                probe.harts(cmd.hartId).microOp(cmd.uopId).storeData = memWrite

                if(!error){
                  Array.copy(memBytes, 0, bytes, cmd.address.toInt & (p.p.dataWidth / 8 - 1), cmd.bytes)
                  for(i <- 0 until cmd.bytes) cmd.data(i) = (memWrite >> i*8).toByte
                  write()
                }
              }
            }
          }
          p.data #= bytes
          p.error #= error
          if(p.scMiss != null) p.scMiss #= scMiss

//          if (cmd.io) {
//            assert(!cmd.amoEnable, "io amo not supported in testbench yet")
//            p.error #= peripheral.access(cmd.write, cmd.address, cmd.data)
//            if(!cmd.write) {
//              val bytes = new Array[Byte](p.p.dataWidth / 8)
//              simRandom.nextBytes(bytes)
//              Array.copy(cmd.data, 0, bytes, cmd.address.toInt & (p.p.dataWidth / 8 - 1), cmd.data.size)
//              p.data #= bytes
//            }
//          } else {
//            p.error #= false
//            cmd.write match {
//              case true => {
//                mem.write(cmd.address, cmd.data)
//                p.data.randomize()
//              }
//              case false => {
//                val bytes = new Array[Byte](p.p.dataWidth / 8)
//                simRandom.nextBytes(bytes)
//                mem.readBytes(cmd.address, cmd.bytes, bytes, cmd.address.toInt & (p.p.dataWidth / 8 - 1))
//                p.data #= bytes
//              }
//            }
//          }
          if(cmd.address < 0x10000000) p.error #= true
        }
        doIt
      }

      //TODO backpresure
      cmdReady.setFactor(dbusReadyFactor)
      rspDriver.setFactor(dbusReadyFactor)



      val hal = new FsmHal{
        override def next(): Unit = {
          if (fsmTasks.nonEmpty) fsmTasks.dequeue()
          if (fsmTasks.nonEmpty) fsmTasks.head.start(this)
        }
        override def putc(value: String): Unit = peripheral.getcQueue ++= value.map(_.toByte)
      }
      if (fsmTasks.nonEmpty) fsmTasks.head.start(hal)
      peripheral.putcListeners += (c => if (fsmTasks.nonEmpty) fsmTasks.head.getc(hal, c))
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