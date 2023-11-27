package vexiiriscv.tester

import spinal.core.sim._
import vexiiriscv.riscv.Riscv
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}

import java.io.File
import scala.collection.mutable.ArrayBuffer

object RegressionConsts{

}

class Regression(compiled : SimCompiled[VexiiRiscv]){
  val dut = compiled.dut
  val xlen = dut.database(Riscv.XLEN)

  def newTest() = {
    val t = new TestOptions()
    t.traceIt = true
    t
  }
  val tests = ArrayBuffer[TestOptions]()

  //rvi tests
  val f = new File("ext/NaxSoftware/riscv-tests")
  val riscvTests = f.list()
  val rvi = riscvTests.filter(t => t.startsWith(s"rv${xlen}ui-p-") && !t.contains(".")).map(new File(f, _))
  for(elf <- rvi) {
    val t = newTest()
    t.elfs += elf
    t.failAfter = Some(100000)
    t.startSymbol = Some("test_2")
    tests += t
  }

  for(t <- tests){
    t.test(compiled)
  }
}

object Regression extends App{
  val simConfig = SpinalSimConfig()
  simConfig.withFstWave

  val param = new ParamSimple()
  val compiled = simConfig.compile(VexiiRiscv(param.plugins()))
  val regression = new Regression(compiled)
}
