package vexiiriscv.tester

import spinal.core.{SpinalConfig, assert}
import spinal.core.sim.{SimCompiled, SpinalSimConfig}
import vexiiriscv.{ParamSimple, VexiiRiscv}

import java.io.{IOException, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.util.Scanner

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
 * The advantage of this aproache over runnint the  sbt "runMain vexiiriscv.tester.TestBench" is that you can
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
  assert(new scopt.OptionParser[Unit]("TestBenchServer") {
    help("help").text("prints this usage text")
    param.addOptions(this)
  }.parse(args, ()).nonEmpty)

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
