package vexiiriscv.soc.micro

import spinal.core.SpinalVerilog

import scala.collection.mutable.ArrayBuffer

object MicroSocSynt extends App{
  import spinal.lib.eda.bench._
  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog{
    val p = new MicroSocParam()
    new MicroSoc(p) {
      socCtrl.systemClk.setName("clk")
      setDefinitionName("MicroSoc")
    }
  })

  val targets = ArrayBuffer[Target]()
  //  targets ++=  XilinxStdTargets(withFMax = true, withArea = true)
  //  targets ++= AlteraStdTargets()
  targets ++= EfinixStdTargets(withFMax = true, withArea = true)

  Bench(rtls, targets)
}

