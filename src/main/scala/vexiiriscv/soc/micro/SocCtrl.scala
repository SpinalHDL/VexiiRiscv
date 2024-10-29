package vexiiriscv.soc.micro

import spinal.core._
import spinal.lib.ResetCtrlFiber
import spinal.lib.cpu.riscv.debug.DebugModuleSocFiber


class SocCtrlParam{
  var withJtagTap = false
  var withJtagInstruction = false
  var systemFrequency = 100 MHz
  def withDebug = withJtagTap ||  withJtagInstruction

  def addOptions(parser: scopt.OptionParser[Unit]) = {
    import parser._
    opt[Int]("system-frequency") action { (v, c) => systemFrequency = v Hz }
    opt[Boolean]("jtag-tap") action { (v, c) => withJtagTap = v  }
    opt[Boolean]("tag-instruction") action { (v, c) => withJtagInstruction = v}
  }
}

class SocCtrl(p: SocCtrlParam) extends Area{
  val systemClk = in Bool()
  val systemClkCd = ClockDomain(systemClk, frequency = FixedFrequency(p.systemFrequency))

  val asyncReset = in Bool()
  val debug = systemClkCd(new ResetCtrlFiber().addAsyncReset(asyncReset, HIGH))
  val system  = systemClkCd(new ResetCtrlFiber().addAsyncReset(debug))

  val debugModule = p.withDebug generate debug.cd(new DebugModuleSocFiber(p.withJtagTap, p.withJtagInstruction){
    system.addSyncRelaxedReset(dm.ndmreset, HIGH)
  })
}