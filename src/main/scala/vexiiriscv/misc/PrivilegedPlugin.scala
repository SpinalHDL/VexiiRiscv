package vexiiriscv.misc

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.CsrAccessPlugin
import vexiiriscv.riscv.{CSR, Riscv}

class PrivilegedPlugin extends FiberPlugin{
  lazy val cap = host[CsrAccessPlugin]
  setupRetain(cap.csrLock)

  val logic = during build new Area{
    val mtval = Reg(Bits(Riscv.XLEN bits)) init(0)
    cap.readWrite(mtval, CSR.MTVAL)
    cap.csrLock.release()
  }
}
