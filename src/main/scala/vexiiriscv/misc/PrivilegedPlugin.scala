package vexiiriscv.misc

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.CsrAccessPlugin
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._


object PrivilegedConfig{
  def full = PrivilegedConfig(
    withSupervisor = true,
    withUser       = true,
    withUserTrap   = false,
    withRdTime     = true,
    withDebug      = false,
    vendorId       = 0,
    archId         = 5, //As spike
    impId          = 0,
    hartId         = 0,
    debugTriggers  = 0
  )
}

case class PrivilegedConfig(withSupervisor : Boolean,
                            withUser: Boolean,
                            withUserTrap: Boolean,
                            withRdTime : Boolean,
                            withDebug: Boolean,
                            debugTriggers : Int,
                            vendorId: Int,
                            archId: Int,
                            impId: Int,
                            var hartId: Int) {

}


class PrivilegedPlugin(p : PrivilegedConfig) extends FiberPlugin{
  lazy val cap = host[CsrAccessPlugin]
  setupRetain(cap.csrLock)

  val logic = during build new Area{

    val withFs = RVF || p.withSupervisor
    val mstatus = new Area {
      val mie, mpie = RegInit(False)
      val mpp = RegInit(U"00")
      val fs = withFs generate RegInit(U"00")
      val sd = False
      if (RVF) ??? //setup.isFpuEnabled setWhen (fs =/= 0)
      if (withFs) sd setWhen (fs === 3)
    }
    val mtval = Reg(Bits(Riscv.XLEN bits)) init (0)

//    cap.readWrite(CSR.MCAUSE, XLEN - 1 -> cause.interrupt, 0 -> cause.code)
    cap.readWrite(CSR.MSTATUS, 11 -> mstatus.mpp, 7 -> mstatus.mpie, 3 -> mstatus.mie)
    cap.read(CSR.MSTATUS, XLEN - 1 -> mstatus.sd)
//    cap.read(CSR.MIP, 11 -> mip.meip, 7 -> mip.mtip, 3 -> mip.msip)
//    cap.readWrite(CSR.MIE, 11 -> mie.meie, 7 -> mie.mtie, 3 -> mie.msie)
    cap.readWrite(mtval, CSR.MTVAL)
    cap.csrLock.release()
  }
}
