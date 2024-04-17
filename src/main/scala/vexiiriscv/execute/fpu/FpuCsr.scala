package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.regfile.{RegFileWriter, RegFileWriterService, RegfileService}
import vexiiriscv.riscv._


class FpuCsr() extends FiberPlugin{


  val api = during build new Area{
    val rm = Reg(Bits(3 bits)) init (0)
    val flags = Reg(FpuFlags())
  }

  val logic = during setup new Area{
    val cp = host[CsrService]
    val buildBefore = retains(cp.csrLock)
    awaitBuild()

    assert(Global.HART_COUNT.get == 1)
    api.flags.NV init (False)
    api.flags.DZ init (False)
    api.flags.OF init (False)
    api.flags.UF init (False)
    api.flags.NX init (False)

    cp.readWrite(CSR.FCSR, 5 -> api.rm)
    cp.readWrite(CSR.FCSR, 0 -> api.flags)
    cp.readWrite(CSR.FRM, 0 -> api.rm)
    cp.readWrite(CSR.FFLAGS, 0 -> api.flags)

    buildBefore.release()
  }
}
