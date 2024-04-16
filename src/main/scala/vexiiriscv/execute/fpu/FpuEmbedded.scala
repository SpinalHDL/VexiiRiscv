package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.{CsrAccessPlugin, CsrService, LaneLayer}
import vexiiriscv.riscv.{CSR, Riscv}

class FpuEmbedded() extends FiberPlugin{
  val logic = during setup new Area{
    val fe = host[FpuExecute].logic
    awaitBuild()

    val ports = List(fe.floatCmd, fe.intCmd, fe.floatWriteback, fe.integerWriteback)
    for(p <- ports) p.setAsDirectionLess()

    val core = FpuCore(FpuParameter(
      rvd = Riscv.RVD,
      rv64 = Riscv.XLEN.get == 64,
      robIdWidth = fe.robIdWidth,
      portCount = 1,
      withAdd = true,
      withMul = true
    ))

    val port = core.io.ports(0)
    port.floatCmd << fe.floatCmd
    port.intCmd << fe.intCmd
    port.floatWriteback >> fe.floatWriteback
//    port.floatWake >> fe.
    port.intWriteback >> fe.integerWriteback //halfpipe ?
    port.unschedule := False
  }
}
