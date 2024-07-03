package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.misc.{PrivilegedPlugin, TrapReason, TrapService}
import vexiiriscv.riscv.{CSR, Rvi}
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.decode.Decode
import vexiiriscv.schedule.ReschedulePlugin

import scala.collection.mutable.ArrayBuffer


object EnvPluginOp extends SpinalEnum{
  val ECALL, EBREAK, PRIV_RET, FENCE_I, SFENCE_VMA, WFI = newElement()
}

class EnvPlugin(layer : LaneLayer,
                executeAt : Int) extends ExecutionUnitElementSimple(layer){
  val OP = Payload(EnvPluginOp())

  val logic = during setup new Logic{
    val sp = host[ReschedulePlugin]
    val ts = host[TrapService]
    val ps = host[PrivilegedPlugin]
    val ioRetainer = retains(sp.elaborationLock, ts.trapLock)
    awaitBuild()

    val age = el.getExecuteAge(executeAt)
    val trapPort = ts.newTrap(age, Execute.LANE_AGE_WIDTH)
    val flushPort = sp.newFlushPort(age, Execute.LANE_AGE_WIDTH, true)

    add(Rvi.ECALL).decode(OP -> EnvPluginOp.ECALL)
    add(Rvi.EBREAK).decode(OP -> EnvPluginOp.EBREAK)
    add(Rvi.MRET).decode(OP -> EnvPluginOp.PRIV_RET)
    if (ps.implementSupervisor) add(Rvi.SRET).decode(OP -> EnvPluginOp.PRIV_RET)
    if (ps.implementUserTrap)   add(Rvi.URET).decode(OP -> EnvPluginOp.PRIV_RET)

    add(Rvi.FENCE_I).decode(OP -> EnvPluginOp.FENCE_I)
    add(Rvi.WFI).decode(OP -> EnvPluginOp.WFI)
    if (ps.implementSupervisor) add(Rvi.SFENCE_VMA).decode(OP -> EnvPluginOp.SFENCE_VMA)

    for (uop <- uopList; spec = layer(uop)) {
      spec.setCompletion(executeAt)
      spec.mayFlushUpTo(executeAt)
    }

    uopRetainer.release()
    ioRetainer.release()

    val exe = new el.Execute(executeAt){
      flushPort.valid := False
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False

      trapPort.valid := False
      trapPort.exception := True
      trapPort.tval := B(PC).andMask(OP === EnvPluginOp.EBREAK) //That's what spike do
      trapPort.code := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      trapPort.arg.assignDontCare()
      trapPort.laneAge := Execute.LANE_AGE

      val privilege = ps.getPrivilege(HART_ID)
      val xretPriv = Decode.UOP(29 downto 28).asUInt
      val commit = False

      val retKo = ps.p.withSupervisor.mux(ps.logic.harts(0).m.status.tsr && privilege === 1 && xretPriv === 1, False)
      val vmaKo = ps.p.withSupervisor.mux(privilege === 1 && ps.logic.harts(0).m.status.tvm || privilege === 0, False)
      
      switch(this(OP)) {
        is(EnvPluginOp.EBREAK) {
          trapPort.code := CSR.MCAUSE_ENUM.BREAKPOINT
        }
        is(EnvPluginOp.ECALL) {
          trapPort.code := B(privilege.resize(Global.CODE_WIDTH) | CSR.MCAUSE_ENUM.ECALL_USER)
        }
        is(EnvPluginOp.PRIV_RET) {
          when(xretPriv <= ps.getPrivilege(HART_ID) && !retKo) {
            commit := True
            trapPort.exception := False
            trapPort.code := TrapReason.PRIV_RET
            trapPort.arg(1 downto 0) := xretPriv.asBits
          }
        }

        is(EnvPluginOp.WFI) {
          when(privilege === 3 || !ps.logic.harts(0).m.status.tw && (Bool(!ps.implementSupervisor) || privilege === 1)) {
            commit := True
            trapPort.exception := False
            trapPort.code := TrapReason.WFI
          }
        }

        is(EnvPluginOp.FENCE_I) {
          commit := True
          trapPort.exception := False
          trapPort.code := TrapReason.FENCE_I
        }

        if (ps.implementSupervisor) {
          is(EnvPluginOp.SFENCE_VMA) {
            when(!vmaKo) {
              commit := True
              trapPort.exception := False
              trapPort.code := TrapReason.SFENCE_VMA
            }
          }
        }
      }

      when(isValid && SEL) {
        flushPort.valid := True
        trapPort.valid := True
        bypass(Global.TRAP) := True
        when(!commit) {
          bypass(COMMIT) := False
        }
      }
    }
  }
}
