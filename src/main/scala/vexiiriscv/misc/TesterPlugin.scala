package vexiiriscv.misc

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.CsrService
import vexiiriscv.execute.lsu.LsuPlugin

class TesterPlugin extends FiberPlugin{
  val logic = during build{
    val l = List(
      host[TrapPlugin].api.harts(0).holdPrivChange,
      host[LsuPlugin].logic.onCtrl.fenceTrap.enable,
      host[CsrService].bus.decode.fence
    )
    val counter = Reg(UInt(12 bits)) init(0)
    counter := counter + 1
    when(counter(8, 2 bits) === 0) {
      l.foreach(_:= True)
    }
  }
}
