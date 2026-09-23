package vexiiriscv.soc.micro

import spinal.core._
import spinal.lib.ResetCtrlFiber
import spinal.lib.cpu.riscv.debug.DebugModuleSocFiber


class SocCtrlParam {
  var withJtagTap = false
  var withJtagInstruction = false
  var withSwd = false
  var systemFrequency = 100 MHz
  def withDebug = withJtagTap ||  withJtagInstruction || withSwd

  def addOptions(parser: scopt.OptionParser[Unit]) = {
    import parser._
    opt[Int]("system-frequency") action { (v, c) => systemFrequency = v Hz }
    opt[Boolean]("jtag-tap") action { (v, c) => withJtagTap = v  }
    opt[Boolean]("jtag-instruction") action { (v, c) => withJtagInstruction = v}
    opt[Boolean]("swd") text("SWD debug transport (custom DTM). Use it with --jtag-tap=false") action { (v, c) => withSwd = v}
  }
}

class SocCtrl(p: SocCtrlParam) extends Area {
  val systemClk = in Bool()
  val systemClkCd = ClockDomain(systemClk, frequency = FixedFrequency(p.systemFrequency))

  val asyncReset = in Bool()
  val debug = systemClkCd(new ResetCtrlFiber().addAsyncReset(asyncReset, HIGH))
  val system  = systemClkCd(new ResetCtrlFiber().addAsyncReset(debug))

  assert(!(p.withSwd && (p.withJtagTap || p.withJtagInstruction)), "--swd=true can't be combined with the JTAG transports (add --jtag-tap=false)")
  val debugModule = p.withDebug generate debug.cd(new DebugModuleSocFiber(p.withJtagTap, p.withJtagInstruction){
    val swd = p.withSwd generate dm.withSwdTransport()
    system.addSyncRelaxedReset(dm.ndmreset, HIGH)
  })
}