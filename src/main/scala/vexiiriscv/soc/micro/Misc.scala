package vexiiriscv.soc.micro

import spinal.core.SpinalVerilog

import scala.collection.mutable.ArrayBuffer

object MicroSocSynt extends App {
  import spinal.lib.eda.bench._
  val p = new MicroSocParam()

  var testXilinx = false
  var testAltera = false
  var testEfinix = false

  assert(new scopt.OptionParser[Unit]("MicroSoc") {
    p.addOptions(this)
    opt[Boolean]("bench-xilinx").action { (v, c) => testXilinx = v  }
      .text("Enable Xilinx benchmark target")
    opt[Boolean]("bench-altera").action { (v, c) => testAltera = v  }
      .text("Enable Altera benchmark target")
    opt[Boolean]("bench-efinix").action { (v, c) => testEfinix = v  }
      .text("Enable Efinix benchmark target")
  }.parse(args, ()).nonEmpty)
  p.legalize()

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog {
    new MicroSoc(p) {
      socCtrl.systemClk.setName("clk")
      setDefinitionName("MicroSoc")
    }
  })

  val targets = ArrayBuffer[Target]()
  if (testXilinx) targets ++=  XilinxStdTargets(withFMax = true, withArea = true)
  if (testAltera) targets ++= AlteraStdTargets()
  if (testEfinix) targets ++= EfinixStdTargets(withFMax = true, withArea = true)

  Bench(rtls, targets)
}

