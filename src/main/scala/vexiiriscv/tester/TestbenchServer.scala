package vexiiriscv.tester

import spinal.core.{SpinalConfig, assert}
import spinal.core.sim.{SimCompiled, SpinalSimConfig}
import vexiiriscv.{ParamSimple, VexiiRiscv}

import java.io.{IOException, PrintWriter}
import java.net.{InetSocketAddress, SocketAddress, StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel, SocketChannel}
import java.nio.file.{Files, Paths}
import java.util.Scanner
import java.util.concurrent.atomic.AtomicBoolean


class TestBenchServerOptions {
  var address: SocketAddress = new InetSocketAddress("127.0.0.1", 8189)
  def addOptions(parser: scopt.OptionParser[Unit]): Unit = {
    import parser._
    opt[String]("host") action { (value, _) =>
      val parts = value.split(":", 2)
      address = new InetSocketAddress(parts(0), if (parts.length == 2) parts(1).toInt else 8189)
    }
    opt[String]("socket") action { (value, _) => address = UnixDomainSocketAddress.of(Paths.get(value).toAbsolutePath.normalize()) }
  }
}

class TestBenchServerSocket(val channel: ServerSocketChannel, val address: SocketAddress) extends AutoCloseable {
  val closed = new AtomicBoolean(false)
  def accept(): SocketChannel = channel.accept()
  def description: String = channel.getLocalAddress match {
    case socket: UnixDomainSocketAddress => s"unix:${socket.getPath}"
    case address => address.toString.stripPrefix("/")
  }
  override def close(): Unit = if (closed.compareAndSet(false, true)) {
    try {
      channel.close()
    } finally {
      TestBenchServerSocket.cleanup(address)
    }
  }
}

object TestBenchServerSocket {
  def open(address: SocketAddress): TestBenchServerSocket = {
    cleanup(address)
    val family = address match {
      case _: InetSocketAddress => StandardProtocolFamily.INET
      case _: UnixDomainSocketAddress => StandardProtocolFamily.UNIX
      case _ => ???
    }
    val channel = ServerSocketChannel.open(family)
    try {
      channel.bind(address)
      new TestBenchServerSocket(channel, address)
    } catch {
      case exception: Exception =>
        try channel.close() finally cleanup(address)
        throw exception
    }
  }
  def cleanup(address: SocketAddress): Unit = address match {
    case socket: UnixDomainSocketAddress => Files.deleteIfExists(socket.getPath)
    case _ =>
  }
}

/**
 * So, this is a quite special scala App.
 * It allows to start a VexiiRiscv simulation server for a specific configuration.
 *
 * here is an example how you can start the server and spawn a simulation on it :
 *   sbt "runMain vexiiriscv.tester.TestBenchServer --with-rvm"
 * Then wait for :
 *   [info] [Progress] Verilator compilation started
 *   [info] [Progress] Verilator compilation done in 2591.432 ms
 *   [info] Waiting for connections
 * Then, in another terminal, you can spawn a new simulation via for instance :
 *   echo '--load-elf ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf' | nc localhost 8189
 *
 * The advantage of this approach over running the sbt "runMain vexiiriscv.tester.TestBench" is that you can
 * "warm up" a simulator, meaning running the generation and compilation of the hardware, and then invoke the simulations
 * many time on it with close to zero startup time.
 * This was done to run the many embench benchmarks efficiently.
 */
object TestBenchServer extends App{
  val simConfig = SpinalSimConfig()
  simConfig.withFstWave
  simConfig.withTestFolder
  simConfig.withConfig(SpinalConfig(dontCareGenAsZero = true)) //TODO dontCareGenAsZero = true required as verilator isn't deterministic on that :())

  val param = new ParamSimple()
  val serverOptions = new TestBenchServerOptions()
  assert(new scopt.OptionParser[Unit]("TestBenchServer") {
    help("help").text("prints this usage text")
    serverOptions.addOptions(this)
    param.addOptions(this)
  }.parse(args, ()).nonEmpty)

  val compiled = simConfig.compile(TestBench.makeDut(param, 1))
  val serverSocket = TestBenchServerSocket.open(serverOptions.address)
  Runtime.getRuntime.addShutdownHook(new Thread("testbench-server-cleanup") {
    override def run() = serverSocket.close()
  })
  var i = 0
  println(s"Waiting for connections on ${serverSocket.description}")
  try while (true) {
    val incoming = serverSocket.accept()
    new TestBenchServerConnection(incoming, compiled)
    i += 1
  } finally {
    serverSocket.close()
  }
}

class TestBenchServerConnection(incoming: SocketChannel, compiled : SimCompiled[TestBenchDut]) extends Thread {
  this.start()
  override def run() = {
    try try {
      val inputStream = Channels.newInputStream(incoming)
      val outputStream = Channels.newOutputStream(incoming)
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
          }.parse(args, ()).nonEmpty)
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
