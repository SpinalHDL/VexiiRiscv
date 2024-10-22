package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.CsrService

case class PrefetchCmd() extends Bundle {
  val pc = Global.PC()
}

case class FetchProbe() extends Bundle {
  val pc = Global.PC()
  val refill = Bool()
}


abstract class PrefetcherPlugin extends FiberPlugin {
  val io = during build new Area{
    val probe = Stream(FetchProbe())
    val cmd = Stream(PrefetchCmd())
  }
}


class PrefetcherNextLinePlugin(lineSize : Int) extends PrefetcherPlugin{
  val logic = during setup new Area{
    val cp = host[CsrService]
    val earlyLock = retains(cp.csrLock)
    awaitBuild()

    val csr = new Area {
      val disable = RegInit(False)
      cp.readWrite(0x7FF, 0 -> disable)
    }

    earlyLock.release()

    val probeAddressNext = KeepAttribute(io.probe.pc + lineSize)
    val address = Reg(Global.PC)
    val addressHit = address.dropLow(log2Up(lineSize)) === io.probe.pc.dropLow(log2Up(lineSize))

    val unbuffered = Stream(PrefetchCmd())
    unbuffered.valid := False
    unbuffered.pc := probeAddressNext
    when(io.probe.valid){
      when(io.probe.refill || addressHit){
        unbuffered.valid setWhen(!csr.disable)
        address := probeAddressNext
      }
    }
    io.cmd << unbuffered.stage()
  }
}