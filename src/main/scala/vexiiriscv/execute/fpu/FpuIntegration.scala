package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.{CsrAccessPlugin, CsrService, LaneLayer}
import vexiiriscv.riscv.CSR

class FpuIntegration(val layer : LaneLayer) extends FiberPlugin{
  val api = during build new Area{
    val rm = Reg(Bits(3 bits)) init (0)
  }

  val logic = during setup new Area{
    val cp = host[CsrService]
    val buildBefore = retains(cp.csrLock)

    awaitBuild()

    val state = new Area {
      assert(Global.HART_COUNT.get == 1)
      val flags = Reg(FpuFlags())
      flags.NV init (False)
      flags.DZ init (False)
      flags.OF init (False)
      flags.UF init (False)
      flags.NX init (False)

      cp.readWrite(CSR.FCSR, 5 -> api.rm)
      cp.readWrite(CSR.FCSR, 0 -> flags)
      cp.readWrite(CSR.FRM, 0 -> api.rm)
      cp.readWrite(CSR.FFLAGS, 0 -> flags)
      assert(host[CsrAccessPlugin].layer == layer, "Csr and FPU need to be on the same layer, unless CSR -> RM -> FPU fencing is implemented")
    }

    buildBefore.release()
  }
}
