package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamPlugin, CsrRamService, ExecuteLanePlugin}
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._
import vexiiriscv._
import vexiiriscv.decode.Decode
import vexiiriscv.decode.Decode.{INSTRUCTION_SLICE_COUNT, INSTRUCTION_SLICE_COUNT_WIDTH, INSTRUCTION_WIDTH}
import vexiiriscv.fetch.{Fetch, FetchL1Service, InitService, LsuL1Service, LsuService, PcService}
import vexiiriscv.memory.AddressTranslationService
import vexiiriscv.prediction.{HistoryPlugin, Prediction}
import vexiiriscv.schedule.Ages

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class TrapSpec(bus : Flow[Trap], age : Int, subAge : Int)
case class Trap(laneAgeWidth : Int, full : Boolean) extends Bundle{
  val exception = Bool()
  val tval = TVAL()
  val code = CODE()
  val arg = TRAP_ARG()
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

  //age => main priority, bigger win, meaning it a stage futher down the pipeline
  //laneAge => On the same pipeline stage, specify at run time local age (in the stage over multiple lanes). smaller => win
  //subAge => Static priority between trap ports with same age and same laneAge, bigger win
  def newTrap(age: Int, laneAgeWidth: Int, subAge : Int = 0): Flow[Trap] = {
    traps.addRet(TrapSpec(Flow(Trap(laneAgeWidth, true)), age, subAge)).bus
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
  val REDO = 2
  val NEXT = 3
  val FENCE_I = 4
  val SFENCE_VMA = 5
  val MMU_REFILL = 6
  val WFI = 7
  val DEBUG_TRIGGER = 8
}

object TrapArg{
  val LOAD = 0
  val STORE = 1
  val FETCH = 2
}


//TODO ensure that CSR stored in ram are properly masked on read (mtval ... )
class TrapPlugin(trapAt : Int) extends FiberPlugin with TrapService {
  override def trapHandelingAt: Int = trapAt

  def askWake(hartId : Int) = api.harts(hartId).askWake := True

  val api = during build new Area{
    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      val redo = False
      val askWake = False
      val rvTrap = False
      val fsmBusy = Bool()
    }
  }

  val logic = during setup new Area{
    val priv = host[PrivilegedPlugin]
    val cap = host[CsrAccessPlugin]
    val pp = host[PipelineBuilderPlugin]
    val fl1p = host.get[FetchL1Service]
    val lsu = host.get[LsuService]
    val lsul1 = host.get[LsuL1Service]
    val pcs = host[PcService]
    val hp = host.get[HistoryPlugin]
    val ats = host[AddressTranslationService]
    val withRam = host.get[CsrRamService].nonEmpty
    val crs = withRam generate host[CsrRamService]
    val invalidationLocks = retains(fl1p.map(_.invalidationRetainer).toList ++ lsu.map(_.invalidationRetainer))
    val buildBefore = retains(List(pp.elaborationLock, pcs.elaborationLock, cap.csrLock, ats.portsLock) ++ hp.map(_.elaborationLock))
    val ramPortRetainers = withRam generate crs.portLock()
    awaitBuild()

    val fetchL1Invalidate = fl1p.nonEmpty generate (0 until HART_COUNT).map(hartId => fl1p.get.newInvalidationPort())
    val lsuL1Invalidate = lsu.nonEmpty generate (0 until HART_COUNT).map(hartId => lsu.get.newInvalidationPort())

    invalidationLocks.release()

    val trapArgWidths = ArrayBuffer[Int](2)
    if(ats.mayNeedRedo) trapArgWidths += 2+ats.getStorageIdWidth()
    TRAP_ARG_WIDTH.set(trapArgWidths.max)

    trapLock.await()

    val initHold = Bool()

    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      val csr = priv.logic.harts(hartId)

      val crsPorts = withRam generate new Area{
        val read = crs.ramReadPort(CsrRamService.priority.TRAP)
        read.valid := False
        read.address.assignDontCare()

        val write = crs.ramWritePort(CsrRamService.priority.TRAP)
        write.valid := False
        write.address.assignDontCare()
        write.data.assignDontCare()
      }

      def privilegeMux[T <: Data](privValue: UInt)(machine: => T, supervisor: => T): T = {
        val ret = CombInit(machine)
        switch(privValue) {
          if (priv.p.withSupervisor) is(1) {
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

        if (priv.p.withSupervisor) {
          privilegs = 1 :: privilegs
          privilegeAllowInterrupts += 1 -> ((csr.s.status.sie && !csr.withMachinePrivilege) || !csr.withSupervisorPrivilege)
        }

        if (priv.p.withUserTrap) {
          privilegs = 0 :: privilegs
          ??? // privilegeAllowInterrupts += 1 -> ((ustatus.UIE && !setup.supervisorPrivilege))
        }

        while (privilegs.nonEmpty) {
          val p = privilegs.head
          when(privilegeAllowInterrupts(p)) {
            for (i <- csr.spec.interrupt
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

        if (priv.p.withDebug) {
          valid clearWhen (csr.debug.dcsr.step && !csr.debug.dcsr.stepie)
          valid setWhen (csr.debug.doHalt)
        }

        val validBuffer = RegNext(valid) init(False)
        val pendingInterrupt = validBuffer && priv.api.harts(hartId).allowInterrupts
        csr.int.pending setWhen(pendingInterrupt)

        when(csr.spec.interrupt.map(_.cond).orR){
          askWake(hartId)
        }
      }



      val trap = new Area {
        val pending = new Area {
          val requests = traps.map(e => new AgedArbiterUp(e.bus.valid && e.bus.hartId === hartId, e.bus.payload.toRaw(), e.age, e.bus.laneAge, e.subAge))
          val arbiter = new AgedArbiter(requests)
          val state = arbiter.down.toReg
          val pc = Reg(PC)
          val history = hp.nonEmpty generate Reg(Prediction.BRANCH_HISTORY)
          val slices = Reg(UInt(INSTRUCTION_SLICE_COUNT_WIDTH+1 bits))

          val xret = new Area {
            val sourcePrivilege = state.arg(1 downto 0).asUInt
            val targetPrivilege = privilegeMux(sourcePrivilege)(
              csr.m.status.mpp,
              U"0" @@ csr.s.status.spp
            )
          }
        }

        val exception = new Area {
          val exceptionTargetPrivilegeUncapped = U"11"
          val code = CombInit(pending.state.code)
          switch(code) {
            for (s <- csr.spec.exception) {
              is(s.id) {
                var exceptionPrivilegs = if (priv.p.withSupervisor) List(1, 3) else List(3)
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
          csr.commitMask := B(for (self <- lanes; sn = self.execute(trapAt+1).down) yield sn.isFiring && sn(COMMIT))
          val oh = B(for (self <- lanes; sn = self.execute(trapAt).down) yield sn.isFiring && sn(TRAP))
          val valid = oh.orR
          val reader = lanes.map(_.execute(trapAt).down).reader(oh, true)
          when(valid) {
            pending.pc := reader(_(PC))
            if(hp.nonEmpty) pending.history := reader(_(Prediction.BRANCH_HISTORY))
            pending.slices := reader(_(INSTRUCTION_SLICE_COUNT)).resize(INSTRUCTION_SLICE_COUNT_WIDTH+1)+1
          }
        }

        val whitebox = new Area {
          val trap = False
          val interrupt = Bool().assignDontCare()
          val code = Global.CODE().assignDontCare()
        }

        val historyPort = hp.nonEmpty generate hp.get.newPort(Integer.MAX_VALUE, 0)
        if(hp.nonEmpty) {
          historyPort.valid := False
          historyPort.history := pending.history
        }

        val pcPort = pcs.newJumpInterface(Ages.TRAP, 0, 0)
        pcPort.valid := False
        pcPort.hartId := hartId
        pcPort.pc.assignDontCare()
        val fsm = new StateMachine {
          val RESET = makeInstantEntry()
          val RUNNING, PROCESS = new State()
          val TRAP_EPC, TRAP_TVAL, TRAP_TVEC, TRAP_APPLY = new State()
          val XRET_EPC, XRET_APPLY = new State()
          val ATS_RSP = ats.mayNeedRedo generate new State()
          val JUMP = new State()
          val LSU_FLUSH = lsu.nonEmpty generate new State()
          val FETCH_FLUSH = fl1p.nonEmpty generate new State()
          val ENTER_DEBUG, DPC_READ, RESUME = (priv.p.withDebug) generate new State()

          val inflightTrap = trapPendings.map(_(hartId)).orR
          val holdPort = pcs.newHoldPort(hartId)
          holdPort := inflightTrap || !isActive(RUNNING)
          api.harts(hartId).fsmBusy := !isActive(RUNNING)

          val wfi = False //Whitebox

          val buffer = new Area {
            val sampleIt = False
            def sample[T <: Data](that : T) : T = RegNextWhen(that, sampleIt)
            val i = new Area {
              val valid = sample(interrupt.valid)
              val code = sample(interrupt.code)
              val targetPrivilege = sample(interrupt.targetPrivilege)
            }
            val trap = new Area{
              val interrupt = pending.state.code === TrapReason.INTERRUPT && i.valid
              val targetPrivilege = interrupt.mux(i.targetPrivilege, exception.targetPrivilege)
              val tval = pending.state.tval.andMask(!interrupt)
              val code = interrupt.mux(i.code, pending.state.code)
            }
          }

          val resetToRunConditions = ArrayBuffer[Bool](!initHold)
          val atsPorts = ats.mayNeedRedo generate new Area{
            val refill = ats.newRefillPort()
            refill.cmd.valid := False
            refill.cmd.address := pending.state.tval.asUInt
            refill.cmd.storageId := pending.state.arg(2, ats.getStorageIdWidth() bits).asUInt

            val invalidate = ats.newInvalidationPort()
            invalidate.cmd.valid := False
            invalidate.cmd.hartId := hartId

            val invalidated = RegInit(False) setWhen(invalidate.cmd.fire)
            invalidate.cmd.valid setWhen(!invalidated)
            resetToRunConditions += invalidated
          }


          // Used to wait until everybody is ready after reset

          RESET.whenIsActive{
            when(resetToRunConditions.andR){
              goto(RUNNING)
            }
          }

          RUNNING.whenIsActive {
            when(trigger.valid) {
              buffer.sampleIt := True
              goto(PROCESS)
            }
            if (priv.p.withDebug) when(!csr.debug.running && csr.debug.doResume) {
              goto(DPC_READ)
            }
          }

          val jumpTarget = Reg(PC)
          val jumpOffset = UInt(INSTRUCTION_SLICE_COUNT_WIDTH+1 bits)
          jumpTarget := pending.pc + (jumpOffset << Fetch.SLICE_RANGE_LOW)
          jumpOffset := pending.slices.andMask(
            List(
              TrapReason.NEXT,
              TrapReason.FENCE_I,
              TrapReason.SFENCE_VMA,
              TrapReason.WFI
            ).map(pending.state.code === _).orR
          )

          if (fl1p.nonEmpty) fetchL1Invalidate(hartId).cmd.valid := False
          if (lsu.nonEmpty) lsuL1Invalidate(hartId).cmd.valid := False
          val trapEnterDebug = RegInit(False)
          PROCESS.whenIsActive{
            trapEnterDebug := False
            if(hp.nonEmpty) historyPort.valid := True
            when(pending.state.exception || buffer.trap.interrupt) {
              goto(TRAP_TVAL)
              if(priv.p.withDebug){
                when(!csr.debug.debugMode) {
                  val doIt = False
                  when(pending.state.exception && exception.code === CSR.MCAUSE_ENUM.BREAKPOINT) {
                    doIt setWhen(csr.privilege === 3 && csr.debug.dcsr.ebreakm)
                    if (priv.p.withUser) doIt setWhen (csr.privilege === 0 && csr.debug.dcsr.ebreaku)
                    if (priv.p.withSupervisor) doIt setWhen (csr.privilege === 1 && csr.debug.dcsr.ebreaks)
                  }
                  doIt setWhen(buffer.trap.interrupt && csr.debug.doHalt)
                  when(doIt){
                    trapEnterDebug := True
                    goto(TRAP_EPC)
                  }
                } otherwise {
                  goto(ENTER_DEBUG)
                }
              }
            } otherwise {
              switch(pending.state.code) {
                is(TrapReason.INTERRUPT) { //Got a sporadic interrupt => resume
                  assert(!buffer.i.valid)
                  pcPort.valid := True
                  pcPort.pc := pending.pc
                  goto(RUNNING)
                }
                is(TrapReason.PRIV_RET) {
                  goto(XRET_EPC)
                }
                is(TrapReason.FENCE_I) {
                  (lsul1.nonEmpty) match {
                    case true => goto(LSU_FLUSH)
                    case false => fl1p.nonEmpty match {
                      case true => goto(FETCH_FLUSH)
                      case false => goto(JUMP)
                    }
                  }
                }
                is(TrapReason.REDO) {
                  api.harts(hartId).redo := True
                  goto(JUMP)
                }
                is(TrapReason.NEXT) {
                  goto(JUMP)
                }
                is(TrapReason.WFI) {
                  wfi := True
                  when(api.harts(hartId).askWake) {
                    goto(JUMP)
                  }
                }
                is(TrapReason.SFENCE_VMA) {
                  if(ats.mayNeedRedo) {
                    atsPorts.invalidate.cmd.valid := True
                    when(atsPorts.invalidate.cmd.ready) {
                      goto(JUMP)
                    }
                  } else {
                    goto(JUMP)
                  }
                }
                if(ats.mayNeedRedo) is(TrapReason.MMU_REFILL) {
                  atsPorts.refill.cmd.valid := True
                  when(atsPorts.refill.cmd.ready){
                    goto(ATS_RSP)
                  }
                }
                if(priv.p.debugTriggers > 0 ) is(TrapReason.DEBUG_TRIGGER) {
                  csr.debug.trigger.slots.onSel(U(pending.state.tval).resized){slot =>
                    slot.tdata1.hit := True
                  }
                  trapEnterDebug := True
                  goto(TRAP_EPC)
                }
                default {
                  assert(False, "Unexpected trap reason")
                }
              }
            }
          }

          if(lsu.nonEmpty) LSU_FLUSH.whenIsActive{
            lsuL1Invalidate(hartId).cmd.valid := True
            when(lsuL1Invalidate(hartId).cmd.ready) {
              fl1p.nonEmpty match {
                case true => goto(FETCH_FLUSH)
                case false => goto(JUMP)
              }
            }
          }

          if (fl1p.nonEmpty) FETCH_FLUSH.whenIsActive {
            fetchL1Invalidate(hartId).cmd.valid := True
            when(fetchL1Invalidate(hartId).cmd.ready) {
              goto(JUMP)
            }
          }

          if(ats.mayNeedRedo) ATS_RSP.whenIsActive{
            when(atsPorts.refill.rsp.valid){
              goto(JUMP) //TODO shave one cycle
              when(atsPorts.refill.rsp.pageFault || atsPorts.refill.rsp.accessFault){
                pending.state.exception := True
                switch(atsPorts.refill.rsp.pageFault ## pending.state.arg(1 downto 0)){
                  def add(k : Int, v : Int) = is(k){pending.state.code := v}
                  add(TrapArg.FETCH    , CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT)
                  add(TrapArg.LOAD     , CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT)
                  add(TrapArg.STORE    , CSR.MCAUSE_ENUM.STORE_ACCESS_FAULT)
                  add(TrapArg.FETCH | 4, CSR.MCAUSE_ENUM.INSTRUCTION_PAGE_FAULT)
                  add(TrapArg.LOAD  | 4, CSR.MCAUSE_ENUM.LOAD_PAGE_FAULT)
                  add(TrapArg.STORE | 4, CSR.MCAUSE_ENUM.STORE_PAGE_FAULT)
                }
                goto(TRAP_TVAL)
              }
            }
          }

          TRAP_TVAL.whenIsActive {
            crsPorts.write.valid := True
            crsPorts.write.address := privilegeMux(buffer.trap.targetPrivilege)(
              csr.m.tval.getAddress(),
              csr.s.tval.getAddress()
            )
            crsPorts.write.data := Global.expendPc(buffer.trap.tval.asUInt, XLEN).asBits
            when(crsPorts.write.ready) {
              goto(TRAP_EPC)
            }
          }

          TRAP_EPC.whenIsActive {
            crsPorts.write.valid := True
            crsPorts.write.address := privilegeMux(buffer.trap.targetPrivilege)(
              csr.m.epc.getAddress(),
              csr.s.epc.getAddress()
            )
            if (priv.p.withDebug) when(trapEnterDebug) {  crsPorts.write.address := csr.debug.dpc.getAddress() }
            crsPorts.write.data := Global.expendPc(pending.pc, XLEN).asBits
            when(crsPorts.write.ready) {
              goto(TRAP_TVEC)
              if (priv.p.withDebug) when(trapEnterDebug) {
                goto(ENTER_DEBUG)
              }
            }
          }

          val readed = RegNextWhen(crsPorts.read.data, crsPorts.read.valid && crsPorts.read.ready)
          TRAP_TVEC.whenIsActive {
            crsPorts.read.valid := True
            crsPorts.read.address := privilegeMux(buffer.trap.targetPrivilege)(
              csr.m.tvec.getAddress(),
              csr.s.tvec.getAddress()
            )
            when(crsPorts.read.ready) {
              goto(TRAP_APPLY)
            }
          }

          TRAP_APPLY.whenIsActive{
            api.harts(hartId).rvTrap := True
            pcPort.valid := True
            pcPort.pc := U(readed).resized //PC RESIZED

            csr.privilege := buffer.trap.targetPrivilege
            switch(buffer.trap.targetPrivilege) {
              is(3) {
                csr.m.status.mie := False
                csr.m.status.mpie := csr.m.status.mie
                if (priv.p.withUser) csr.m.status.mpp := csr.privilege

                csr.m.cause.code := buffer.trap.code
                csr.m.cause.interrupt := buffer.trap.interrupt
              }
              priv.p.withSupervisor generate is(1) {
                csr.s.status.sie := False
                csr.s.status.spie := csr.s.status.sie
                if (priv.p.withUser) csr.s.status.spp := csr.privilege(0, 1 bits)

                csr.s.cause.code := buffer.trap.code
                csr.s.cause.interrupt := buffer.trap.interrupt
              }
            }

            whitebox.trap := True
            whitebox.interrupt := buffer.trap.interrupt
            whitebox.code :=  buffer.trap.code

            goto(RUNNING)
          }

          if(priv.p.withDebug) {
            csr.debug.bus.exception := False
            csr.debug.bus.ebreak := False
            ENTER_DEBUG.whenIsActive{
              csr.debug.running := False
              when(!csr.debug.debugMode) {
                csr.debug.dcsr.cause := 0
                when(csr.debug.dcsr.step){ csr.debug.dcsr.cause := 4 }
                when(csr.debug.bus.haltReq) { csr.debug.dcsr.cause := 3 }
                when(pending.state.exception && exception.code === CSR.MCAUSE_ENUM.BREAKPOINT) { csr.debug.dcsr.cause := 1 }
                when(!pending.state.exception && exception.code === TrapReason.DEBUG_TRIGGER) { csr.debug.dcsr.cause := 2 }
                csr.debug.dcsr.prv := csr.privilege
              } otherwise {
                csr.debug.bus.exception := pending.state.exception && exception.code =/= CSR.MCAUSE_ENUM.BREAKPOINT
                csr.debug.bus.ebreak    := pending.state.exception && exception.code === CSR.MCAUSE_ENUM.BREAKPOINT
              }
              csr.privilege := 3
              goto(RUNNING)
            }

            DPC_READ.whenIsActive {
              crsPorts.read.valid := True
              crsPorts.read.address := csr.debug.dpc.getAddress
              when(crsPorts.read.ready) {
                goto(RESUME)
              }
            }
            RESUME.whenIsActive {
              pcPort.valid := True
              pcPort.pc := U(readed).resized //PC RESIZED
              csr.privilege := csr.debug.dcsr.prv
              csr.debug.running := True
              csr.debug.bus.resume.rsp.valid := True
              goto(RUNNING)
            }
          }

          val xretPrivilege = U(pending.state.arg(1 downto 0))
          XRET_EPC.whenIsActive{
            crsPorts.read.valid := True
            crsPorts.read.address := privilegeMux(xretPrivilege)(
              csr.m.epc.getAddress(),
              csr.s.epc.getAddress()
            )
            when(crsPorts.read.ready) {
              goto(XRET_APPLY)
            }
          }

          XRET_APPLY.whenIsActive{
            pcPort.valid := True
            pcPort.pc := U(readed).resized //PC RESIZED

            csr.privilege := pending.xret.targetPrivilege
            csr.xretAwayFromMachine setWhen (pending.xret.targetPrivilege < 3)
            switch(pending.state.arg(1 downto 0)) {
              is(3) {
                if(priv.p.withUser) csr.m.status.mpp := 0
                csr.m.status.mie := csr.m.status.mpie
                csr.m.status.mpie := True
              }
              priv.p.withSupervisor generate is(1) {
                csr.s.status.spp := U"0"
                csr.s.status.sie := csr.s.status.spie
                csr.s.status.spie := True
              }
            }
            goto(RUNNING)
          }

          JUMP.whenIsActive {
            pcPort.valid := True
            pcPort.pc := jumpTarget //PC RESIZED
            goto(RUNNING)
          }
        }
      }
    }

    ramPortRetainers.release()
    buildBefore.release()

    initHold := host.list[InitService].map(_.initHold()).orR
  }
}

