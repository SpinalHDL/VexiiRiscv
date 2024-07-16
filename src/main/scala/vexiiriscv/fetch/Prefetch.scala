package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global

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
  val logic = during build new Area{
    val filtred = io.probe.continueWhen(io.probe.refill)
    val translated = PrefetchCmd()
    translated.pc := filtred.pc + lineSize
    val serialized = filtred.map(p => translated)
    io.cmd << serialized.stage()
  }
}