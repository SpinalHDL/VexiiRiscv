package vexiiriscv.tester

import rvls.spinal.{FileBackend, RvlsBackend}
import spinal.core._
import spinal.core.sim._
import spinal.lib.{CheckSocketPort, DoCmd}
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.tilelink.{M2sTransfers, SizeRange}
import spinal.lib.bus.tilelink.sim.{Checker, MemoryAgent, TransactionA}
import spinal.lib.com.jtag.sim.{JtagRemote, JtagTcp}
import spinal.lib.misc.Elf
import spinal.lib.misc.plugin.Hostable
import spinal.lib.misc.test.DualSimTracer
import spinal.lib.sim.{FlowDriver, SparseMemory, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.lib.system.tag.{MemoryTransfers, PmaRegion}
import vexiiriscv._
import vexiiriscv.execute.lsu.{LsuCachelessPlugin, LsuL1, LsuL1Plugin, LsuL1TlPlugin, LsuPlugin}
import vexiiriscv.fetch.{FetchCachelessPlugin, FetchL1Plugin, PcService}
import vexiiriscv.misc.{EmbeddedRiscvJtag, PrivilegedPlugin}
import vexiiriscv.riscv.Riscv
import vexiiriscv.test.konata.Backend
import vexiiriscv.test.{PeripheralEmulator, VexiiRiscvProbe}

import java.io.{File, IOException, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.nio.ByteBuffer
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
  def loadU32(address : Long, value : Long) : this.type = {args ++= List("--load-u32", f"0x${address.toHexString},0x${value.toHexString}"); this }
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
  val u32s = ArrayBuffer[(Long, Int)]()
  val elfs = ArrayBuffer[File]()
  var testName = Option.empty[String]
  var passSymbolName = "pass"
  val fsmTasksGen = mutable.Queue[() => FsmTask]()
  var ibusReadyFactor = 1.01f
  var ibusBaseLatency = 0
  var dbusReadyFactor = 1.01f
  var dbusBaseLatency = 0
  var seed = 2
  var jtagRemote = false
  var spawnProcess = Option.empty[String]

//  traceRvlsLog = true; traceKonata = true; traceWave = true; traceSpikeLog = true; printStats = true

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
    opt[Seq[String]]("load-bin") unbounded() action { (v, c) => bins += java.lang.Long.parseLong(v(0).replace("0x", ""), 16) -> new File(v(1)) }
    opt[Seq[String]]("load-u32") unbounded() action { (v, c) => u32s += java.lang.Long.parseLong(v(0).replace("0x", ""), 16) -> java.lang.Integer.parseInt(v(1).replace("0x", ""), 16) }
    opt[String]("load-elf") unbounded() action { (v, c) => elfs += new File(v) }
    opt[String]("start-symbol") action { (v, c) => startSymbol = Some(v) }
    opt[String]("pass-symbol") action { (v, c) => passSymbolName = v }
    opt[Long]("start-symbol-offset") action { (v, c) => startSymbolOffset = v }
    opt[Double]("ibus-ready-factor") unbounded() action { (v, c) => ibusReadyFactor = v.toFloat }
    opt[Double]("dbus-ready-factor") unbounded() action { (v, c) => dbusReadyFactor = v.toFloat }
    opt[Unit]("jtag-remote") unbounded() action { (v, c) => jtagRemote = true }
    opt[Int]("memory-latency") action { (v, c) => dbusBaseLatency = v; ibusBaseLatency = v }

    opt[String]("fsm-putc") unbounded() action { (v, c) => fsmTasksGen += (() => new FsmPutc(v)) }
    opt[Unit]("fsm-putc-lr") unbounded() action { (v, c) => fsmTasksGen += (() => new FsmPutc("\n")) }
    opt[String]("fsm-getc") unbounded() action { (v, c) => fsmTasksGen += (() => new FsmGetc(v)) }
    opt[Long]("fsm-sleep") unbounded() action { (v, c) => fsmTasksGen += (() => new FsmSleep(v)) }
    opt[Unit]("fsm-success") unbounded() action { (v, c) => fsmTasksGen += (() => new FsmSuccess()) }
    opt[Int]("seed") action { (v, c) => seed = v }
    opt[Unit]("rand-seed") action { (v, c) => seed = scala.util.Random.nextInt() }

    opt[String]("spawn-process") unbounded() action { (v, c) => spawnProcess = Some(v) }
  }

  def test(compiled : SimCompiled[VexiiRiscv]): Unit = {
    dualSim match {
      case true => DualSimTracer.withCb(compiled, window = 200000 * 10, seed=seed)(test)
      case false => compiled.doSimUntilVoid(name = getTestName(), seed=seed) { dut => disableSimWave(); test(dut, f => f) }
    }
  }

  def test(dut : VexiiRiscv, onTrace : (=> Unit) => Unit = cb => {}) : Unit = {
//    traceRvlsLog = true; traceKonata = true; traceWave = true; traceSpikeLog = true; printStats = true;
    val fsmTasks =  mutable.Queue[FsmTask]()
    for(gen <- fsmTasksGen) fsmTasks += gen()
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    simSpeedPrinter.foreach(cd.forkSimSpeedPrinter)

    failAfter.foreach(delayed(_)(simFailure("Reached Timeout")))
    passAfter.foreach(delayed(_)(simSuccess()))

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
    val rvls = withRvlsCheck generate new RvlsBackend(new File(currentTestPath()))
    if (withRvlsCheck) {
      rvls.spinalSimFlusher(10 * 10000)
      rvls.spinalSimTime(10000)
    }

    val konataBackend = traceKonata.option(new Backend(new File(currentTestPath(), "konata.log")))
    delayed(1)(konataBackend.foreach(_.spinalSimFlusher(10 * 10000))) // Delayed to ensure this is registred last

    // Collect traces from the CPUs behaviour
    val probe = new VexiiRiscvProbe(dut, konataBackend, withRvls)
    if (withRvlsCheck) probe.add(rvls)
    probe.enabled = withProbe
    probe.trace = false

    // Things to enable when we want to collect traces
    val tracerFile = traceRvlsLog.option(new FileBackend(new File(currentTestPath(), "tracer.log")))
    onTrace {
      if (traceWave) enableSimWave()
      if (withRvlsCheck && traceSpikeLog) rvls.debug()
      if (traceKonata) probe.trace = true

      tracerFile.foreach{f =>
        f.spinalSimFlusher(10 * 10000)
        f.spinalSimTime(10000)
        probe.add(f)
      }

      val r = probe.backends.reverse
      probe.backends.clear()
      probe.backends ++= r
    }

    val regions = dut.host.services.collectFirst {
      case p: LsuCachelessPlugin => p.regions.get
      case p: LsuL1Plugin => p.regions.get
    }.get

    for(region <- regions){
      probe.backends.foreach { b =>
        val mapping = region.mapping match {
          case sm : SizeMapping => sm
        }
        if(mapping.base != 0x1000) {
          b.addRegion(0, region.isMain.mux(0, 1), mapping.base.toLong, mapping.size.toLong)
        }
      }
    }


    val mem = SparseMemory(seed = 0)
    // Load the binaries
    for ((offset, file) <- bins) {
      mem.loadBin(offset, file)
      if (withRvlsCheck) rvls.loadBin(offset, file)
      tracerFile.foreach(_.loadBin(offset, file))
    }

    for ((offset, value) <- u32s) {
      mem.write(offset, value)
      val array = ByteBuffer.allocate(4).putInt(value).array.reverse
      if (withRvlsCheck) rvls.loadBytes(offset, array)
      tracerFile.foreach(_.loadBytes(offset, array))
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
          if (pc == failSymbol) delayed(1)(simFailure("Software reached the fail symbol :("))
        }
      }
    }

    val host = dut.host[PrivilegedPlugin]
    val priv = host.hart(0)
    val peripheral = new PeripheralEmulator(0x10000000, priv.int.m.external, (priv.int.s != null) generate priv.int.s.external, msi = priv.int.m.software, mti = priv.int.m.timer, cd = cd){
      override def getClintTime(): Long = probe.cycle
    }
    peripheral.withStdIn = withStdIn


    var forceProbe = Option.empty[Long => Unit]

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
          p.error #= cmd.address < 0x20000000
        }
        doIt
      }

      cmdReady.setFactor(ibusReadyFactor)
      rspDriver.setFactor(ibusReadyFactor)
    }

    val fl1p = dut.host.get[fetch.FetchL1Plugin].map { p =>
      val bus = p.logic.bus
      val cmdReady = StreamReadyRandomizer(bus.cmd, cd)

      case class Rsp(data: Array[Byte], error : Boolean, id : Int)
      val pending = mutable.Queue[Rsp]()

      val cmdMonitor = StreamMonitor(bus.cmd, cd) { pay =>
        val address = pay.address.toLong
        val id = pay.id.toInt
        def doIt() = delayed(ibusBaseLatency*10) {
          for (i <- 0 until p.logic.memWordPerLine) {
            pending += Rsp(mem.readBytes(address + i * p.logic.bytePerMemWord, p.memDataWidth / 8), address < 0x10000000, id)
          }
        }

        forceProbe match {
          case Some(probe) => fork{
            probe(address)
            doIt()
          }
          case None => doIt()
        }
      }
      val rspDriver = StreamDriver(bus.rsp, cd) { p =>
        val doIt = pending.nonEmpty
        if (doIt) {
          val rsp = pending.dequeue()
          p.data #= rsp.data
          p.error #= rsp.error
          p.id #= rsp.id
        }
        doIt
      }

      cmdReady.setFactor(ibusReadyFactor)
      rspDriver.setFactor(ibusReadyFactor)
    }

    val lsclp = dut.host.get[execute.lsu.LsuCachelessBusProvider].map { p =>
      val bus = p.getLsuCachelessBus()
      val cmdReady = StreamReadyRandomizer(bus.cmd, cd)
      bus.cmd.ready #= true
      var reserved = false

      case class Access(id : Int, write : Boolean, address: Long, data : Array[Byte], bytes : Int, io : Boolean, hartId : Int, uopId : Int, amoEnable : Boolean, amoOp : Int)
      val pending = mutable.Queue[Access]()

      val cmdMonitor = StreamMonitor(bus.cmd, cd) { p =>
        val bytes = 1 << bus.cmd.size.toInt
        val address = p.address.toLong
        val offset = address.toInt & (bytes-1)
        pending.enqueue(
          Access(
            p.id.toInt,
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
            import vexiiriscv.execute.lsu.LsuCachelessBusAmo._
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
          p.id #= cmd.id
          if(p.scMiss != null) p.scMiss #= scMiss
          if(cmd.address < 0x10000000) p.error #= true
        }
        doIt
      }

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


    val lsul1 = dut.host.get[LsuL1TlPlugin] map (p => new Area{
      val ma = new MemoryAgent(p.bus, cd, seed = 0, randomProberFactor = if(dbusReadyFactor < 1.0) 0.2f else 0.0f, memArg = Some(mem))(null) {
        driver.driver.setFactor(dbusReadyFactor)
        val checker = if (monitor.bus.p.withBCE) Checker(monitor)
        override def checkAddress(address: Long) = address >= 0x20000000 || address >= 0x1000 && address < 0x2000
        override def delayOnA(a: TransactionA) = {
//          if(a.address == 0x81820000l){
//            println(f"miaou ${mem.readByteAsInt(0x817FFFF3l)}%x")
//            println(s"\n!! $simTime ${a.opcode.getName} ${a.data}")
//          }
          if(dbusBaseLatency != 0) cd.waitSampling(dbusBaseLatency)
          if(dbusReadyFactor < 1.0) super.delayOnA(a)
        }
        forceProbe = Some { address =>
          this.reserve(address)
          this.handleCoherency(
            address = address,
            isAquire = false,
            sourceAgent = null,
            cap = 2,
            allowProbePerm = false
          )
          this.release(address)
        }
      }
    })

    dut.host.services.foreach{
      case p : EmbeddedRiscvJtag => {
        p.debugCd.resetSim #= true
        delayed(20) (p.debugCd.resetSim #= false)
        if (jtagRemote) {
          CheckSocketPort.reserve(JtagRemote.defaultPort)
          onSimEnd(CheckSocketPort.release(JtagRemote.defaultPort))
          while (!CheckSocketPort(JtagRemote.defaultPort)) {
            Thread.sleep(100)
          }
          JtagRemote(p.logic.jtag, 20)
          probe.checkLiveness = false
        }
      }
      case _ =>
    }

    spawnProcess.foreach{ v =>
      delayed(10000){
        val p = DoCmd.startCmd(v)
        onSimEnd(if(p.isAlive())p.destroy())
        periodicaly(10*1000) {
          if (!p.isAlive()) {
            p.exitValue() match {
              case 0 => simSuccess()
              case _ => simFailure()
            }
          }
        }
      }
    }

    if(printStats) onSimEnd{
      println(probe.getStats())
    }
  }
}

object TestBench extends App{
  doIt()

  def paramToPlugins(param : ParamSimple): ArrayBuffer[Hostable] = {
    val ret = param.plugins()
    ret.collectFirst{case p : LsuL1Plugin => p}.foreach{p =>
      p.ackIdWidth = 8
      p.probeIdWidth = log2Up(p.writebackCount)
      ret  += new LsuL1TlPlugin
    }
    val regions = ArrayBuffer(
      new PmaRegion{
        override def mapping: AddressMapping = SizeMapping(0x80000000l, (1l << param.physicalWidth) - 0x80000000l)
        override def transfers: MemoryTransfers = M2sTransfers(
          get = SizeRange.all,
          putFull = SizeRange.all
        )
        override def isMain: Boolean = true
        override def isExecutable: Boolean = true
      },
      new PmaRegion{
        override def mapping: AddressMapping = SizeMapping(0x10000000l, 0x10000000l)
        override def transfers: MemoryTransfers = M2sTransfers(
          get = SizeRange.all,
          putFull = SizeRange.all
        )
        override def isMain: Boolean = false
        override def isExecutable: Boolean = true
      },
      new PmaRegion{
        override def mapping: AddressMapping = SizeMapping(0x1000, 0x1000)
        override def transfers: MemoryTransfers = M2sTransfers(
          get = SizeRange.all,
          putFull = SizeRange.all
        )
        override def isMain: Boolean = true
        override def isExecutable: Boolean = true
      }
    )
    ret.foreach{
      case p: FetchCachelessPlugin => p.regions.load(regions)
      case p: LsuCachelessPlugin => p.regions.load(regions)
      case p: FetchL1Plugin => p.regions.load(regions)
      case p: LsuPlugin => p.ioRegions.load(regions)
      case p: LsuL1Plugin => p.regions.load(regions)
      case p: EmbeddedRiscvJtag => p.debugCd = ClockDomain.current.copy(reset = Bool().setName("debugReset"))
      case _ =>
    }

    ret
  }

  def doIt(param : ParamSimple = new ParamSimple()) {
    val testOpt = new TestOptions()

    val genConfig = SpinalConfig()
//    genConfig.includeSimulation

    val simConfig = SpinalSimConfig()
//    simConfig.withIVerilog
//    simConfig.withVcdWave
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
      simConfig.compile(VexiiRiscv(paramToPlugins(param)))
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
  simConfig.withConfig(SpinalConfig(dontCareGenAsZero = true)) //TODO dontCareGenAsZero = true required as verilator isn't deterministic on that :())

  val param = new ParamSimple()
  val compiled = simConfig.compile(VexiiRiscv(TestBench.paramToPlugins(param)))
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