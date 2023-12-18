package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.misc.{PrivilegedPlugin, TrapService}
import vexiiriscv.riscv.{CSR, Rvi}
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.decode.Decode
import vexiiriscv.schedule.ReschedulePlugin


object EnvPluginOp extends SpinalEnum{
  val ECALL, EBREAK, XRET = newElement()
}

class EnvPlugin(layer : LaneLayer,
                executeAt : Int) extends ExecutionUnitElementSimple(layer){
  lazy val sp = host[ReschedulePlugin]
  lazy val ts = host[TrapService]
  lazy val ps = host[PrivilegedPlugin]
  setupRetain(sp.elaborationLock)
  setupRetain(ts.trapLock)

  val OP = Payload(EnvPluginOp())

  val logic = during build new Logic{
    val age = eu.getExecuteAge(executeAt)
    val trapPort = ts.newTrap(age, Execute.LANE_AGE_WIDTH)
    val flushPort = sp.newFlushPort(age, Execute.LANE_AGE_WIDTH, true)

    add(Rvi.ECALL).decode(OP -> EnvPluginOp.ECALL)
    add(Rvi.EBREAK).decode(OP -> EnvPluginOp.EBREAK)
    add(Rvi.MRET).decode(OP -> EnvPluginOp.XRET)

    val uopList = List(Rvi.ECALL, Rvi.EBREAK, Rvi.MRET)
    for (uop <- uopList; spec = layer(uop)) {
      spec.setCompletion(executeAt)
      spec.mayFlushUpTo(executeAt)
    }

    eu.uopLock.release()
    srcp.elaborationLock.release()
    ts.trapLock.release()
    sp.elaborationLock.release()


    val exe = new eu.Execute(executeAt){
      flushPort.valid := False
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False

      trapPort.valid := False
      trapPort.exception := True
      trapPort.code.assignDontCare()
      trapPort.tval := B(PC).andMask(OP === EnvPluginOp.EBREAK) //That's what spike do

      val privilege = ps.getPrivilege(HART_ID)
      val xretPriv = Decode.UOP(29 downto 28).asUInt
      val forceCommit = True
      switch(this(OP)) {
        is(EnvPluginOp.EBREAK) {
          trapPort.code := CSR.MCAUSE_ENUM.BREAKPOINT
        }
        is(EnvPluginOp.ECALL) {
          trapPort.code := B(privilege.resize(Global.CODE_WIDTH) | CSR.MCAUSE_ENUM.ECALL_USER)
        }
        is(EnvPluginOp.XRET) {
//          trapPort.code := CAUSE_XRET //the reschedule cause isn't the final value which will end up into XCAUSE csr
//          setup.reschedule.tval(1 downto 0) := xretPriv.asBits
//          when(xretPriv < priv.getPrivilege()) {
//            setup.reschedule.cause := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
//            setup.reschedule.reason := ScheduleReason.TRAP
//            setup.reschedule.skipCommit := True
//          }
        }
      }

      when(isValid && SEL) {
        flushPort.valid := True
        trapPort.valid := True
        bypass(Global.TRAP) := True
        when(forceCommit) {
          bypass(TRAP_COMMIT) := True
        }
      }
    }
  }
}
