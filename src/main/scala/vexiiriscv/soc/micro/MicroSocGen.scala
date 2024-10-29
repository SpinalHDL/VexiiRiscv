package vexiiriscv.soc.micro

import spinal.core._
import scala.collection.mutable.ArrayBuffer


object MicroSocGen extends App{
  val p = new MicroSocParam()

  assert(new scopt.OptionParser[Unit]("MicroSoc") {
    p.addOptions(this)
  }.parse(args, Unit).nonEmpty)
  p.legalize()

  val report = SpinalVerilog(new MicroSoc(p))
}

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





//  val h = report.toplevel.main.cpu.logic.core.host
//  val path = PathTracer.impl(h[SrcPlugin].logic.addsub.rs2Patched, h[TrapPlugin].logic.harts(0).trap.pending.state.tval)
//  println(path.report)