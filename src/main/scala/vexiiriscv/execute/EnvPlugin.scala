package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.misc.{PrivilegedPlugin, TrapReason, TrapService}
import vexiiriscv.riscv.{CSR, PrivilegeMode, Rvi}
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.decode.Decode
import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object EnvPluginOp extends SpinalEnum {
  val ECALL, EBREAK, PRIV_RET, FENCE_I, SFENCE_VMA, WFI = newElement()
}

/**
 * Implements a bunch of special instruction which always traps into the TrapPlugin. (ecall, ebreak, mret, fences, efi, ...)
 */
class EnvPlugin(layer : LaneLayer,
                executeAt : Int) extends ExecutionUnitElementSimple(layer) {
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

    val exe = new el.Execute(executeAt) {
      flushPort.valid := False
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False

      trapPort.valid := False
      trapPort.exception := True
      trapPort.tval := B(PC).andMask(OP === EnvPluginOp.EBREAK)  | Decode.UOP.andMask(List(EnvPluginOp.PRIV_RET, EnvPluginOp.WFI, EnvPluginOp.SFENCE_VMA).map(_ === this(OP)).orR).resized
      trapPort.code := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      trapPort.arg.assignDontCare()
      trapPort.laneAge := Execute.LANE_AGE

      val privilege = ps.getPrivilege(HART_ID)
      val isGuest = PrivilegeMode.isGuest(privilege)
      val xretPriv = PrivilegeMode(PrivilegeMode.isGuest(privilege), Decode.UOP(29 downto 28))
      val commit = False

      def privilegeCheck(payloads: mutable.LinkedHashMap[Int, Bool], privilege: SInt): Bool = payloads.map{case (mode, cond) => privilege === mode && cond}.orR

      val retKoMapping = mutable.LinkedHashMap[Int, Bool](
        PrivilegeMode.S -> (ps.logic.harts(0).m.status.tsr && xretPriv === PrivilegeMode.S)
      )
      if (ps.p.withHypervisor) retKoMapping += (
        PrivilegeMode.VS -> (ps.logic.harts(0).h.status.vtsr && xretPriv === PrivilegeMode.VS)
      )
      val retKo = ps.p.withSupervisor.mux(privilegeCheck(retKoMapping, privilege), False)

      val wfiMapping = mutable.LinkedHashMap[Int, Bool](
        PrivilegeMode.S -> True,
        PrivilegeMode.U -> Bool(!ps.implementSupervisor)
      )
      if (ps.p.withHypervisor) wfiMapping += (
        PrivilegeMode.VS -> !ps.logic.harts(0).h.status.vtw,
        PrivilegeMode.VU -> False
      )

      val vmaKoMapping = mutable.LinkedHashMap[Int, Bool](
        PrivilegeMode.S -> ps.logic.harts(0).m.status.tvm,
        PrivilegeMode.U -> True
      )
      if (ps.p.withHypervisor) vmaKoMapping += (
        PrivilegeMode.VS -> ps.logic.harts(0).h.status.vtvm,
        PrivilegeMode.VU -> True
      )
      val vmaKo = ps.p.withSupervisor.mux(privilegeCheck(vmaKoMapping, privilege), False)

      switch(this(OP)) {
        is(EnvPluginOp.EBREAK) {
          trapPort.code := CSR.MCAUSE_ENUM.BREAKPOINT
        }
        is(EnvPluginOp.ECALL) {
          trapPort.code := PrivilegeMode.isGuest(privilege).mux(
            B((privilege.asBits << 1).resize(Global.CODE_WIDTH)),
            B(privilege.resize(Global.CODE_WIDTH) | CSR.MCAUSE_ENUM.ECALL_USER)
          )
        }
        is(EnvPluginOp.PRIV_RET) {
          when(xretPriv <= ps.getPrivilege(HART_ID) && !retKo) {
            commit := True
            trapPort.exception := False
            trapPort.code := TrapReason.PRIV_RET
            trapPort.arg(2 downto 0) := xretPriv.asBits
          }
          if(ps.p.withHypervisor) {
            when(privilege === PrivilegeMode.VU || (privilege === PrivilegeMode.VS && retKoMapping(PrivilegeMode.VS))) {
              trapPort.code := CSR.MCAUSE_ENUM.VIRTUAL_INSTRUCTION
            }
          }
        }

        is(EnvPluginOp.WFI) {
          when(privilege === PrivilegeMode.M || !ps.logic.harts(0).m.status.tw && privilegeCheck(wfiMapping, privilege)) {
            commit := True
            trapPort.exception := False
            trapPort.code := TrapReason.WFI
          }
          if(ps.p.withHypervisor) {
            when(!ps.logic.harts(0).m.status.tw && (privilege === PrivilegeMode.VU || (privilege === PrivilegeMode.VS && !wfiMapping(PrivilegeMode.VS)))) {
              trapPort.code := CSR.MCAUSE_ENUM.VIRTUAL_INSTRUCTION
            }
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
            if(ps.p.withHypervisor) {
              when(privilege === PrivilegeMode.VU || privilege === PrivilegeMode.VS && vmaKoMapping(PrivilegeMode.VS)) {
                trapPort.code := CSR.MCAUSE_ENUM.VIRTUAL_INSTRUCTION
              }
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
