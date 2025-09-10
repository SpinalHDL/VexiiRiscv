package vexiiriscv.misc

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.CsrService
import vexiiriscv.execute.lsu.LsuPlugin

import scala.collection.mutable.ArrayBuffer

class TesterPlugin extends FiberPlugin{
  val logic = during build{
    val l = ArrayBuffer(
      host[TrapPlugin].api.harts(0).holdPrivChange,
      host[CsrService].bus.decode.fence
    )
    host.get[LsuPlugin].map(l += _.logic.onCtrl.fenceTrap.enable)
    val counter = Reg(UInt(12 bits)) init(0)
    counter := counter + 1
    when(counter(6, 2 bits) === 0) {
      l.foreach(_:= True)
    }
  }
}
