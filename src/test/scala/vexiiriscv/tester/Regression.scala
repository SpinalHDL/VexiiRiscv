package vexiiriscv.tester

import org.apache.commons.io.FileUtils
import spinal.core.sim._
import spinal.lib.misc.test.AsyncJob
import vexiiriscv.riscv.Riscv
import vexiiriscv.tester.Regression.simConfig
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext

class Regression(compiled : SimCompiled[VexiiRiscv]){
  val dut = compiled.dut
  val xlen = dut.database(Riscv.XLEN)

  def newTest() = {
    val t = new TestOptions()
//    t.traceWave = true
    t
  }
  val tests = ArrayBuffer[TestOptions]()

  val nsf = new File("ext/NaxSoftware")

  //rvi tests
  val riscvTestsFile = new File(nsf, "riscv-tests")
  val riscvTests = riscvTestsFile.list().sorted
  val rvi = riscvTests.filter(t => t.startsWith(s"rv${xlen}ui-p-") && !t.contains(".")).map(new File(riscvTestsFile, _))
  for(elf <- rvi) {
    val t = newTest()
    t.elfs += elf
    t.failAfter = Some(100000)
    t.startSymbol = Some("test_2")
    t.testName = Some(elf.getName)
    tests += t
  }

  val rvm = riscvTests.filter(t => t.startsWith(s"rv${xlen}um-p-") && !t.contains(".")).map(new File(riscvTestsFile, _))
  for (elf <- rvm) {
    val t = newTest()
    t.elfs += elf
    t.failAfter = Some(100000)
    t.startSymbol = Some("test_2")
    t.testName = Some(elf.getName)
    tests += t
  }

  {
    val t = newTest()
    t.elfs += new File(nsf, "baremetal/dhrystone/build/rv32ima/dhrystone.elf")
    t.failAfter = Some(10000000)
    t.testName = Some("dhrystone")
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

object Regression extends App{
  val simConfig = SpinalSimConfig()
  simConfig.withFstWave
  simConfig.setTestPath("$WORKSPACE/$COMPILED_tests/$TEST")

  val param = new ParamSimple()
  val compiled = simConfig.compile(VexiiRiscv(param.plugins()))
  val regression = new Regression(compiled)
  println("*"*80)
  val fails = regression.jobs.filter(_.failed)
  if(fails.size == 0){ println("PASS"); System.exit(0) }
  println(s"FAILED ${fails.size}/${regression.jobs.size}")
  for(fail <- fails){
    println("- " + fail.logsFile.getAbsolutePath)
  }
  System.exit(regression.jobs.count(_.failed))
}
