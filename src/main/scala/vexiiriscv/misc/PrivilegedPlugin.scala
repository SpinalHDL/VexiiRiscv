package vexiiriscv.misc

import spinal.core.{Bool, _}
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrRamPlugin, ExecuteLanePlugin}
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._
import vexiiriscv._
import vexiiriscv.fetch.PcService
import vexiiriscv.schedule.Ages

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object PrivilegedParam{
  def full = PrivilegedParam(
    withSupervisor = false,
    withUser       = false,
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

case class PrivilegedParam(withSupervisor : Boolean,
                           withUser: Boolean,
                           withUserTrap: Boolean,
                           withRdTime : Boolean,
                           withDebug: Boolean,
                           debugTriggers : Int,
                           vendorId: Int,
                           archId: Int,
                           impId: Int,
                           var hartId: Int) {
  def setHartId(v : Int) : this.type = {
    hartId = v
    this
  }
}


case class TrapSpec(bus : Flow[Trap], age : Int)
case class Trap(laneAgeWidth : Int, full : Boolean) extends Bundle{
  val exception = Bool()
  val tval = TVAL()
  val code = CODE()
  val laneAge = full generate UInt(laneAgeWidth bits)
  val hartId = full generate HART_ID()

  def toRaw(): Trap = {
    val r = new Trap(laneAgeWidth, false)
    r.assignSomeByName(this)
    r
  }
}

trait CauseUser{
  def getCauseWidthMin() : Int
}

/**
 * fetch (page fault, access fault)
 * decode (illegal)
 * execute
 * - miss aligned load/store/branch
 * - page fault, access fault
 * - ecall, ebreak
 *
 * To do a trap request :
 * - Flush all youngers instructions
 * - Store cause / tval
 * - Rise trap flag in the pipe (this will disable side-effects)
 */
trait TrapService extends Area{
  val trapLock = Retainer()
  val traps = ArrayBuffer[TrapSpec]()
  def newTrap(age: Int, laneAgeWidth: Int): Flow[Trap] = {
    traps.addRet(TrapSpec(Flow(Trap(laneAgeWidth, true)), age)).bus
  }

  def trapHandelingAt : Int
  val trapPendings = ArrayBuffer[Bits]()
  def newTrapPending() = trapPendings.addRet(Bits(Global.HART_COUNT bits))
}

case class TrapPending() extends Bundle{
  val hartId = Global.HART_ID()
}


object TrapReason{
  val INTERRUPT = 0
  val PRIV_RET = 1
}

class PrivilegedPlugin(params : Seq[PrivilegedParam], trapAt : Int) extends FiberPlugin with TrapService with CommitService{
  override def trapHandelingAt: Int = trapAt

  def implementSupervisor = params.exists(_.withSupervisor)
  def implementUser = params.exists(_.withUser)
  def implementUserTrap = params.exists(_.withUserTrap)

  def getPrivilege(hartId : UInt) : UInt = logic.csrs.map(_.privilege).read(hartId)

  case class Delegator(var enable: Bool, privilege: Int)
  case class InterruptSpec(var cond: Bool, id: Int, privilege: Int, delegators: List[Delegator])
  case class ExceptionSpec(id: Int, delegators: List[Delegator])
  override def getCommitMask(hartId: Int): Bits = io.harts(hartId).commitMask
  override def hasInflight(hartId: Int): Bool = io.harts(hartId).hasInflight

  val io = during build new Area{
    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val commitMask = Bits(host.list[ExecuteLanePlugin].size bits)
      val hasInflight = Bool()
      val p = params(hartId)
      val rdtime = in UInt (64 bits)
      val int = new Area {
        val pending = False
        val m = new Area {
          val timer = Verilator.public(in Bool())
          val software = Verilator.public(in Bool())
          val external = Verilator.public(in Bool())
        }
        val s = p.withSupervisor generate new Area {
          val external = Verilator.public(in Bool())
        }
        val u = p.withUserTrap generate new Area {
          val external = Verilator.public(in Bool())
        }
      }
      val spec = new Area{
        var interrupt = ArrayBuffer[InterruptSpec]()
        var exception = ArrayBuffer[ExceptionSpec]()

        def addInterrupt(cond: Bool, id: Int, privilege: Int, delegators: List[Delegator]): Unit = {
          interrupt += InterruptSpec(cond, id, privilege, delegators)
        }
      }
    }
  }



  val logic = during setup new Area{
    val cap = host[CsrAccessPlugin]
    val pp = host[PipelineBuilderPlugin]
    val pcs = host[PcService]
    val buildBefore = retains(List(pp.elaborationLock, pcs.elaborationLock, cap.csrLock) ++ host.get[CsrRamPlugin].map(_.allocationLock))
    awaitBuild()

    val causesWidthMins = host.list[CauseUser].map(_.getCauseWidthMin())
    CODE_WIDTH.set((4 +: causesWidthMins).max)

    assert(HART_COUNT.get == 1)

    val csrs = for(hartId <- 0 until HART_COUNT) yield new Area{
      val p = params(hartId)
      val hartIo = io.harts(hartId)
      val api = cap.hart(hartId)
      val withFs = RVF || p.withSupervisor
      val privilege = RegInit(U"11")
      val withMachinePrivilege = privilege >= U"11"
      val withSupervisorPrivilege = privilege >= U"01"
      val xretAwayFromMachine = False

      val m = new api.Csr(CSR.MSTATUS) {
        val status = new Area {
          val mie, mpie = RegInit(False)
          val mpp = RegInit(U"00")
          val fs = withFs generate RegInit(U"00")
          val sd = False
          if (RVF) ??? //setup.isFpuEnabled setWhen (fs =/= 0)
          if (withFs) sd setWhen (fs === 3)

          readWrite(11 -> mpp, 7 -> mpie, 3 -> mie)
          read(XLEN - 1 -> sd)
          if (withFs) readWrite(13 -> fs)
          if (p.withSupervisor && XLEN.get == 64) read(34 -> U"10")
        }

        val cause = new api.Csr(CSR.MCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)

          readWrite(XLEN-1 -> interrupt, 0 -> code)
        }

        val ip =  new api.Csr(CSR.MIP) {
          val meip = RegNext(hartIo.int.m.external) init (False)
          val mtip = RegNext(hartIo.int.m.timer) init (False)
          val msip = RegNext(hartIo.int.m.software) init (False)
          read(11 -> meip, 7 -> mtip, 3 -> msip)
        }

        val ie = new api.Csr(CSR.MIE) {
          val meie, mtie, msie = RegInit(False)
          readWrite(11 -> meie, 7 -> mtie, 3 -> msie)
        }

        val medeleg = p.withSupervisor generate new api.Csr(CSR.MEDELEG) {
          val iam, bp, eu, es, ipf, lpf, spf = RegInit(False)
          val mapping = mutable.LinkedHashMap(0 -> iam, 3 -> bp, 8 -> eu, 9 -> es, 12 -> ipf, 13 -> lpf, 15 -> spf)
          for ((id, enable) <- mapping) readWrite(id -> enable)
        }
        val mideleg = p.withSupervisor generate new api.Csr(CSR.MIDELEG) {
          val st, se, ss = RegInit(False)
          readWrite(9 -> se, 5 -> st, 1 -> ss)
        }

        val tval = Reg(TVAL) init (0)
        val epc = Reg(PC) init (0)
        val tvec = Reg(PC) init (0)

        api.onCsr(CSR.MTVAL).readWrite(tval)
        api.onCsr(CSR.MEPC).readWrite(epc)
        api.onCsr(CSR.MTVEC).readWrite(tvec)

//        val tvec = cap.readWriteRam(CSR.MTVEC)
//        val tval = cap.readWriteRam(CSR.MTVAL)
//        val epc = cap.readWriteRam(CSR.MEPC)
        val scratch = cap.readWriteRam(CSR.MSCRATCH)
//        for(i <- 0 until 16) cap.readWriteRam(CSR.MHPMCOUNTER3+i)


//        for (i <- 0 until 4) cap.readWrite(CSR.MHPMCOUNTER3+i, 0 -> Reg(Bits(32 bits)).init(0))


        hartIo.spec.addInterrupt(ip.mtip && ie.mtie, id = 7, privilege = 3, delegators = Nil)
        hartIo.spec.addInterrupt(ip.msip && ie.msie, id = 3, privilege = 3, delegators = Nil)
        hartIo.spec.addInterrupt(ip.meip && ie.meie, id = 11, privilege = 3, delegators = Nil)
      }
    }




    trapLock.await()
    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      val p = params(hartId)
      val hartIo = io.harts(hartId)
      val csr = csrs(hartId)

      def privilegeMux[T <: Data](priv: UInt)(machine: => T, supervisor: => T): T = {
        val ret = CombInit(machine)
        switch(priv) {
          if (p.withSupervisor) is(1) {
            ret := supervisor
          }
        }
        ret
      }

      //Process interrupt request, code and privilege
      val interrupt = new Area {
        val valid = False
        val code = Global.CODE().assignDontCare()
        val targetPrivilege = UInt(2 bits).assignDontCare()

        val privilegeAllowInterrupts = mutable.LinkedHashMap[Int, Bool]()
        var privilegs: List[Int] = Nil
        privilegs :+= 3
        privilegeAllowInterrupts += 3 -> (csr.m.status.mie || !csr.withMachinePrivilege)

        if (p.withSupervisor) {
          ???
//          privilegs = 1 :: privilegs
//          privilegeAllowInterrupts += 1 -> ((s.status.sie && !csr.withMachinePrivilege) || !csr.withSupervisorPrivilege)
        }

        if (p.withUserTrap) {
          privilegs = 0 :: privilegs
          ??? // privilegeAllowInterrupts += 1 -> ((ustatus.UIE && !setup.supervisorPrivilege))
        }

        while (privilegs.nonEmpty) {
          val p = privilegs.head
          when(privilegeAllowInterrupts(p)) {
            for (i <- hartIo.spec.interrupt
                 if i.privilege <= p //EX : Machine timer interrupt can't go into supervisor mode
                 if privilegs.tail.forall(e => i.delegators.exists(_.privilege == e))) { // EX : Supervisor timer need to have machine mode delegator
              val delegUpOn = i.delegators.filter(_.privilege > p).map(_.enable).fold(True)(_ && _)
              val delegDownOff = !i.delegators.filter(_.privilege <= p).map(_.enable).orR
              when(i.cond && delegUpOn && delegDownOff) {
                valid := True
                code := i.id
                targetPrivilege := p
              }
            }
          }
          privilegs = privilegs.tail
        }

        if (p.withDebug) {
          ???
//          when(debug.dcsr.step && debug.dcsr.stepie && !setup.debugMode) {
//            valid := False
//          }
//
//          when(debug.doHalt) {
//            valid := True
//          }
        }

        val pendingInterrupt = RegNext(valid) init (False)
        hartIo.int.pending setWhen(pendingInterrupt)
      }



      val trap = new Area {
        val pending = new Area {
          val requests = traps.map(e => new AgedArbiterUp(e.bus.valid && e.bus.hartId === hartId, e.bus.payload.toRaw(), e.age, e.age))
          val arbiter = new AgedArbiter(requests)
          val state = arbiter.down.toReg
          val pc = Reg(PC)

          val xret = new Area {
            val sourcePrivilege = state.tval(1 downto 0).asUInt
            val targetPrivilege = privilegeMux(sourcePrivilege)(
              csr.m.status.mpp,
              ???//U"0" @@ supervisor.sstatus.spp
            )
          }
        }

        val exception = new Area {
          val exceptionTargetPrivilegeUncapped = U"11"
          val code = CombInit(pending.state.code)
          switch(code) {
            for (s <- hartIo.spec.exception) {
              is(s.id) {
                var exceptionPrivilegs = if (p.withSupervisor) List(1, 3) else List(3)
                while (exceptionPrivilegs.length != 1) {
                  val p = exceptionPrivilegs.head
                  if (exceptionPrivilegs.tail.forall(e => s.delegators.exists(_.privilege == e))) {
                    val delegUpOn = s.delegators.filter(_.privilege > p).map(_.enable).fold(True)(_ && _)
                    val delegDownOff = !s.delegators.filter(_.privilege <= p).map(_.enable).orR
                    when(delegUpOn && delegDownOff) {
                      exceptionTargetPrivilegeUncapped := p
                    }
                  }
                  exceptionPrivilegs = exceptionPrivilegs.tail
                }
              }
            }
          }
          val targetPrivilege = csr.privilege.max(exceptionTargetPrivilegeUncapped)
        }


        val trigger = new Area {
          val lanes = host.list[ExecuteLanePlugin] //TODO AREA filter the ones which may trap
          hartIo.commitMask := B(for (self <- lanes; sn = self.execute(trapAt).down) yield sn.isFiring && sn(COMMIT))
          hartIo.hasInflight := (for (self <- lanes; ctrlId <- 1 to self.executeAt + trapAt; sn = self.ctrl(ctrlId).up) yield sn.isValid && sn(HART_ID) === hartId).orR
          val oh = B(for (self <- lanes; sn = self.execute(trapAt).down) yield sn.isFiring && sn(TRAP))
          val valid = oh.orR
          val pc = OHMux.or(oh, lanes.map(_.execute(trapAt).down(PC)), true)
          when(valid) {
            pending.pc := pc
          }
        }

        val whitebox = new Area {
          val trap = False
          val interrupt = Bool().assignDontCare()
          val code = Global.CODE().assignDontCare()
        }

        val pcPort = pcs.createJumpInterface(Ages.TRAP, 0, 0)
        pcPort.valid := False
        pcPort.hartId := hartId
        pcPort.pc.assignDontCare()
        val fsm = new StateMachine {
          val RUNNING = makeInstantEntry()
          val COMPLETION = new State()

          val inflightTrap = trapPendings.map(_(hartId)).orR
          val holdPort = pcs.newHoldPort(hartId)
          holdPort := inflightTrap || !isActive(RUNNING)

          val buffer = new Area {
            val sampleIt = False
            def sample[T <: Data](that : T) : T = RegNextWhen(that, sampleIt)
            val i = new Area {
              val valid = sample(interrupt.valid)
              val code = sample(interrupt.code)
              val targetPrivilege = sample(interrupt.targetPrivilege)
            }
          }

          RUNNING.whenIsActive {
            when(trigger.valid) {
              buffer.sampleIt := True
              goto(COMPLETION)
            }
          }

          val trapCode = pending.state.exception.mux(pending.state.code, buffer.i.code)
          COMPLETION.whenIsActive {
            when(pending.state.exception || pending.state.code === TrapReason.INTERRUPT && buffer.i.valid) {
              pcPort.valid := True
              pcPort.pc := csrs(hartId).m.tvec

              csr.m.epc := pending.pc
              csr.m.tval := pending.state.tval.andMask(pending.state.exception)
              csr.m.status.mie := False
              csr.m.status.mpie := csr.m.status.mie
              csr.m.status.mpp := csr.privilege
              csr.m.cause.code := trapCode
              csr.m.cause.interrupt := !pending.state.exception
              csr.privilege := exception.targetPrivilege

              whitebox.trap := True
              whitebox.interrupt := !pending.state.exception
              whitebox.code := trapCode
            } otherwise {
              switch(pending.state.code) {
                is(TrapReason.INTERRUPT) {
                  assert(!buffer.i.valid)
                  pcPort.valid := True
                  pcPort.pc := pending.pc
                }
                is(TrapReason.PRIV_RET){
                  pcPort.valid := True
                  pcPort.pc := csr.m.epc

                  csr.privilege := pending.xret.targetPrivilege
                  csr.xretAwayFromMachine setWhen (pending.xret.targetPrivilege < 3)
                  switch(pending.state.tval(1 downto 0)) {
                    is(3) {
                      csr.m.status.mpp := 0
                      csr.m.status.mie := csr.m.status.mpie
                      csr.m.status.mpie := True
                    }
                    p.withSupervisor generate is(1) {
                      ???
//                      s.sstatus.spp := U"0"
//                      s.sstatus.sie := supervisor.sstatus.spie
//                      s.sstatus.spie := True
                    }
                  }
                }
                default {
                  assert(False, "Unexpected trap reason")
                }
              }
            }
            goto(RUNNING)
          }
        }
      }

    }
    buildBefore.release()
  }
}
