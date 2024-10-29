package vexiiriscv.soc.micro

import spinal.core._


object MicroSocGen extends App{
  val p = new MicroSocParam()

  assert(new scopt.OptionParser[Unit]("MicroSoc") {
    p.addOptions(this)
  }.parse(args, Unit).nonEmpty)
  p.legalize()

  val report = SpinalVerilog(new MicroSoc(p))
}




//  val h = report.toplevel.main.cpu.logic.core.host
//  val path = PathTracer.impl(h[SrcPlugin].logic.addsub.rs2Patched, h[TrapPlugin].logic.harts(0).trap.pending.state.tval)
//  println(path.report)