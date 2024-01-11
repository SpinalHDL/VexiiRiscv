package vexiiriscv.tester

import org.apache.commons.io.FileUtils
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.plugin.Hostable
import spinal.lib.misc.test.{AsyncJob, MultithreadedFunSuite}
import vexiiriscv.riscv.Riscv
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.reflect.io.Path.jfile2path

class RegressionSingle(compiled : SimCompiled[VexiiRiscv]){
  val dut = compiled.dut
  val xlen = dut.database(Riscv.XLEN)

  def newTest() = {
    val t = new TestOptions()
//    t.traceWave = true
    t
  }
  val tests = ArrayBuffer[TestOptions]()

  val nsf = new File("ext/NaxSoftware")

  val rejectedTests = mutable.LinkedHashSet("rv32ui-p-simple", "rv32ui-p-fence_i", "rv64ui-p-simple", "rv64ui-p-fence_i", "rv32ua-p-lrsc", "rv64ua-p-lrsc")

  //rvi tests
  val riscvTestsFile = new File(nsf, "riscv-tests")
  val riscvTests = riscvTestsFile.listFiles().sorted
  val rvi = riscvTests.filter{t => val n = t.getName; n.startsWith(s"rv${xlen}ui-p-") && !n.contains(".") && !rejectedTests.contains(n) }
  val rvm = riscvTests.filter { t => val n = t.getName; n.startsWith(s"rv${xlen}um-p-") && !n.contains(".") && !rejectedTests.contains(n)  }
  val rva = riscvTests.filter { t => val n = t.getName; n.startsWith(s"rv${xlen}ua-p-") && !n.contains(".") && !rejectedTests.contains(n)  }

  val riscvTestsFrom2 = ArrayBuffer[File]()
  riscvTestsFrom2 ++= rvi
  riscvTestsFrom2 ++= rvm
  if(dut.database(Riscv.RVA)) riscvTestsFrom2 ++= rva

  for(elf <- riscvTestsFrom2) {
    val t = newTest()
    t.elfs += elf
    t.failAfter = Some(100000)
    t.startSymbol = Some("test_2")
    t.testName = Some("riscv-tests/" + elf.getName)
    tests += t
  }

  {
    val t = newTest()
    t.elfs += new File(nsf, s"riscv-tests/rv${xlen}ua-p-lrsc")
    t.failAfter = Some(1000000)
    t.startSymbol = Some("test_2")
    t.passSymbolName = "test_5"
    t.testName = Some(s"riscv-tests/rv${xlen}ua-p-lrsc")
    tests += t
  }
  {
    val t = newTest()
    t.elfs += new File(nsf, s"riscv-tests/rv${xlen}ua-p-lrsc")
    t.failAfter = Some(100000)
    t.startSymbol = Some("test_6")
    t.testName = Some(s"riscv-tests/rv${xlen}ua-p-lrsc")
    tests += t
  }

  val archTests = new File(nsf, s"riscv-arch-test/rv${xlen}i_m/I").listFiles().filter(_.getName.endsWith(".elf"))
  for (elf <- archTests) {
    val t = newTest()
    t.elfs += elf
    t.failAfter = Some(10000000)
    t.testName = Some("riscv-arch-test/I/" + elf.getName.replace(".elf",""))
    tests += t
  }


  val regulars = List("dhrystone", "coremark")
  for(name <- regulars){
    val t = newTest()
    t.elfs += new File(nsf, s"baremetal/$name/build/rv${xlen}ima/$name.elf")
    t.failAfter = Some(300000000)
    t.testName = Some(name)
    tests += t
  }

  implicit val ec = ExecutionContext.global
  val jobs = ArrayBuffer[AsyncJob]()
  for(t <- tests){
    val testPath = new File(compiled.simConfig.getTestPath(t.testName.get))
    val passFile = new File(testPath, "PASS")
    val failFile = new File(testPath, "FAIL")
    val testName = t.testName.get
    if(!passFile.exists()){
      val stdoutHost = Console.out
      val job = new AsyncJob(toStdout = false, logsPath = testPath)({
        t.test(compiled)
        FileUtils.deleteQuietly(failFile)
        val bf = new BufferedWriter(new FileWriter(passFile))
        bf.flush()
        bf.close()
        stdoutHost.println(s"PASS $testName")
      }){
        override def onFail() = {
          val bf = new BufferedWriter(new FileWriter(failFile))
          bf.flush()
          bf.close()
          stdoutHost.println(s"FAIL $testName")
        }
      }
      jobs += job
    }
  }
  jobs.foreach(_.join())
}

object RegressionSingle extends App{
  def test(name : String, plugins : => Seq[Hostable]): Unit = {
    val simConfig = SpinalSimConfig()
    simConfig.withFstWave
    simConfig.setTestPath("regression/$COMPILED_tests/$TEST")

    val param = new ParamSimple()
    val compiled = SpinalConfig.synchronized(simConfig.compile(VexiiRiscv(plugins).setDefinitionName(s"VexiiRiscv_$name")))
    val regression = new RegressionSingle(compiled)
    println("*" * 80)
    val fails = regression.jobs.filter(_.failed)
    if (fails.size == 0) {
      println("PASS"); return
    }
    println(s"FAILED ${fails.size}/${regression.jobs.size}")
    for (fail <- fails) {
      println("- " + fail.logsFile.getAbsolutePath)
    }
    Thread.sleep(10)
    throw new Exception()
  }

  def test(ps : ParamSimple): Unit = {
    test(ps.getName(), ps.plugins())
  }

  def test(args : String) : Unit = test(args.split(" "))
  def test(args : Seq[String]): Unit = {
    val param = new ParamSimple()
    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      param.addOptions(this)
    }.parse(args, Unit).nonEmpty)
    test(param)
  }

  test(args)
}


class Regression extends MultithreadedFunSuite(sys.env.getOrElse("VEXIIRISCV_REGRESSION_THREAD_COUNT", "0").toInt){
  FileUtils.deleteQuietly(new File("regression"))

  def addTest(args: String): Unit = addTest(args.split("\\s+"))
  def addTest(args: Seq[String]): Unit = {
    val param = new ParamSimple()
    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      param.addOptions(this)
    }.parse(args, Unit).nonEmpty)

    testMp(param.getName()) {
      RegressionSingle.test(param)
    }
  }

  val md = "--with-mul --with-div"
  for(issues <- 1 to 2; rf <- List("", "--regfile-async"); bpf <- List(0,1,2,3,100); xlen <- List(64, 32)){
    val base = s"--decoders $issues --lanes $issues --xlen $xlen $md --allow-bypass-from $bpf"
    addTest(s"$base $rf")
    if(bpf == 0 || bpf == 100) {
      addTest(s"$base $rf --with-btb")
      addTest(s"$base $rf --with-btb --with-ras")
      addTest(s"$base $rf --with-btb --with-ras --with-gshare")
    }
    if(bpf == 0) {
      addTest(s"$base $rf --with-late-alu")
      addTest(s"$base $rf --with-btb --with-ras --with-gshare --with-late-alu")
    }
  }

}