package vexiiriscv.misc

import spinal.core.{Bool, _}
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamPlugin, CsrRamService, ExecuteLanePlugin}
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._
import vexiiriscv._
import vexiiriscv.fetch.{Fetch, PcService}
import vexiiriscv.schedule.Ages

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object PrivilegedParam{
  def base = PrivilegedParam(
    withSupervisor = false,
    withUser       = false,
    withUserTrap   = false,
    withRdTime     = true,
    withDebug      = false,
    vendorId       = 0,
    archId         = 5, //As spike
    impId          = 0,
    debugTriggers  = 0
  )
}

case class PrivilegedParam(var withSupervisor : Boolean,
                           var withUser: Boolean,
                           var withUserTrap: Boolean,
                           var withRdTime : Boolean,
                           var withDebug: Boolean,
                           var debugTriggers : Int,
                           var vendorId: Int,
                           var archId: Int,
                           var impId: Int){
  def check(): Unit = {
    assert(!(withSupervisor && !withUser))
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


//TODO ensure that CSR stored in ram are properly masked on read (mtval ... )
class PrivilegedPlugin(val p : PrivilegedParam, hartIds : Seq[Int], trapAt : Int) extends FiberPlugin with TrapService with CommitService{
  override def trapHandelingAt: Int = trapAt

  def implementSupervisor = p.withSupervisor
  def implementUser = p.withUser
  def implementUserTrap = p.withUserTrap

  def getPrivilege(hartId : UInt) : UInt = logic.csrs.map(_.privilege).read(hartId)

  case class Delegator(var enable: Bool, privilege: Int)
  case class InterruptSpec(var cond: Bool, id: Int, privilege: Int, delegators: List[Delegator])
  case class ExceptionSpec(id: Int, delegators: List[Delegator])
  override def getCommitMask(hartId: Int): Bits = io.harts(hartId).commitMask
  override def hasInflight(hartId: Int): Bool = io.harts(hartId).hasInflight

  val misaIds = mutable.LinkedHashSet[Int]()
  def addMisa(id: Char): Unit = addMisa(id - 'A')
  def addMisa(id: Int) = {
    misaIds += id
  }

  val io = during build new Area{
    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val commitMask = Bits(host.list[ExecuteLanePlugin].size bits)
      val hasInflight = Bool()
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
        val interrupt = ArrayBuffer[InterruptSpec]()
        val exception = ArrayBuffer[ExceptionSpec]()

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
    val withRam = host.get[CsrRamService].nonEmpty
    val crs = withRam generate host[CsrRamService]
    val buildBefore = retains(List(pp.elaborationLock, pcs.elaborationLock, cap.csrLock))
    val ramRetainers = withRam generate new Area{
      val csr = crs.csrLock()
      val port = crs.portLock()
    }

    awaitBuild()
    p.check()

    addMisa('I')
    if (RVC) addMisa('C')
    if (RVF) addMisa('F')
    if (RVD) addMisa('D')
    if (p.withUser) addMisa('U')
    if (p.withSupervisor) addMisa('S')

    val causesWidthMins = host.list[CauseUser].map(_.getCauseWidthMin())
    CODE_WIDTH.set((4 +: causesWidthMins).max)

    assert(HART_COUNT.get == 1)

    val csrs = for(hartId <- 0 until HART_COUNT) yield new Area{
      val hartIo = io.harts(hartId)
      val api = cap.hart(hartId)
      val withFs = RVF || p.withSupervisor
      val privilege = RegInit(U"11")
      val withMachinePrivilege = privilege >= U"11"
      val withSupervisorPrivilege = privilege >= U"01"
      val xretAwayFromMachine = False

      val m = new Area{
        api.read(U(p.vendorId), CSR.MVENDORID) // MRO Vendor ID.
        api.read(U(p.archId), CSR.MARCHID) // MRO Architecture ID.
        api.read(U(p.impId), CSR.MIMPID) // MRO Implementation ID.
        api.read(U(hartId), CSR.MHARTID) // MRO Hardware thread ID.Machine Trap Setup
        val misaExt = misaIds.map(1l << _).reduce(_ | _)
        val misaMxl = XLEN.get match {
          case 32 => BigInt(1) << XLEN.get - 2
          case 64 => BigInt(2) << XLEN.get - 2
        }
        val misa = misaMxl | misaExt
        api.read(U(misa, XLEN bits), CSR.MISA) // MRW ISA and extensions

        val status = new api.Csr(CSR.MSTATUS) {
          val mie, mpie = RegInit(False)
          val mpp = p.withUser.mux(RegInit(U"00"), U"11")
          val fs = withFs generate RegInit(U"00")
          val sd = False
          if (RVF) ??? //setup.isFpuEnabled setWhen (fs =/= 0)
          if (withFs) sd setWhen (fs === 3)

          readWrite(7 -> mpie, 3 -> mie)
          read(11 -> mpp)
          if(p.withUser) {
            onWrite(true){
              switch(cap.onWriteBits(12 downto 11)){
                is(3){ mpp := 3 }
                if(p.withSupervisor) is(1){ mpp := 1 }
                is(0){ mpp := 0 }
              }
            }
          }
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

        val edeleg = p.withSupervisor generate new api.Csr(CSR.MEDELEG) {
          val iam, bp, eu, es, ipf, lpf, spf = RegInit(False)
          val mapping = mutable.LinkedHashMap(0 -> iam, 3 -> bp, 8 -> eu, 9 -> es, 12 -> ipf, 13 -> lpf, 15 -> spf)
          for ((id, enable) <- mapping) readWrite(id -> enable)
        }
        val ideleg = p.withSupervisor generate new api.Csr(CSR.MIDELEG) {
          val st, se, ss = RegInit(False)
          readWrite(9 -> se, 5 -> st, 1 -> ss)
        }

//        val tval = Reg(TVAL) init (0)
//        val epc = Reg(PC) init (0)
//        val tvec = Reg(PC) init (0)

//        api.onCsr(CSR.MTVAL).readWrite(tval)
//        api.onCsr(CSR.MEPC).readWrite(epc)
//        api.onCsr(CSR.MTVEC).readWrite(tvec)

        val tvec = crs.readWriteRam(CSR.MTVEC)
        val tval = crs.readWriteRam(CSR.MTVAL)
        val epc = crs.readWriteRam(CSR.MEPC)
        val scratch = crs.readWriteRam(CSR.MSCRATCH)

        hartIo.spec.addInterrupt(ip.mtip && ie.mtie, id = 7, privilege = 3, delegators = Nil)
        hartIo.spec.addInterrupt(ip.msip && ie.msie, id = 3, privilege = 3, delegators = Nil)
        hartIo.spec.addInterrupt(ip.meip && ie.meie, id = 11, privilege = 3, delegators = Nil)
      }

      val s = p.withSupervisor generate new Area {
        val cause = new api.Csr(CSR.SCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)
          readWrite(XLEN - 1 -> interrupt, 0 -> code)
        }

        val status = new Area{
          val sie, spie = RegInit(False)
          val spp = RegInit(U"0")

          api.read(CSR.SSTATUS, XLEN - 1 -> m.status.sd)
          for (offset <- List(CSR.MSTATUS, CSR.SSTATUS)) {
            api.readWrite(offset, 8 -> spp, 5 -> spie, 1 -> sie)
          }
        }

        val ip = new Area {
          val seipSoft = RegInit(False)
          val seipInput = RegNext(hartIo.int.s.external)
          val seipOr = seipSoft || seipInput
          val stip = RegInit(False)
          val ssip = RegInit(False)

          val seipMasked = seipOr && m.ideleg.se
          val stipMasked = stip && m.ideleg.st
          val ssipMasked = ssip && m.ideleg.ss
        }

        val ie = new Area {
          val seie, stie, ssie = RegInit(False)
        }

        val tvec = crs.readWriteRam(CSR.STVEC)
        val tval = crs.readWriteRam(CSR.STVAL)
        val epc = crs.readWriteRam(CSR.SEPC)
        val scratch = crs.readWriteRam(CSR.SSCRATCH)

        if (withFs) api.readWrite(CSR.SSTATUS, 13 -> m.status.fs)

        def mapMie(machineCsr: Int, supervisorCsr: Int, bitId: Int, reg: Bool, machineDeleg: Bool, sWrite: Boolean = true): Unit = {
          api.read(reg, machineCsr, bitId)
          api.write(reg, machineCsr, bitId)
          api.read(reg && machineDeleg, supervisorCsr, bitId)
          if (sWrite) api.writeWhen(reg, machineDeleg, supervisorCsr, bitId)
        }

        mapMie(CSR.MIE, CSR.SIE, 9, ie.seie, m.ideleg.se)
        mapMie(CSR.MIE, CSR.SIE, 5, ie.stie, m.ideleg.st)
        mapMie(CSR.MIE, CSR.SIE, 1, ie.ssie, m.ideleg.ss)

        api.read(ip.seipOr, CSR.MIP, 9)
        api.write(ip.seipSoft, CSR.MIP, 9)
        api.read(ip.seipOr && m.ideleg.se, CSR.SIP, 9)
        mapMie(CSR.MIP, CSR.SIP, 5, ip.stip, m.ideleg.st, sWrite = false)
        mapMie(CSR.MIP, CSR.SIP, 1, ip.ssip, m.ideleg.ss)
        api.readToWrite(ip.seipSoft, CSR.MIP, 9) //Avoid an external interrupt value to propagate to the soft external interrupt register.


        hartIo.spec.addInterrupt(ip.ssip && ie.ssie, id = 1, privilege = 1, delegators = List(Delegator(m.ideleg.ss, 3)))
        hartIo.spec.addInterrupt(ip.stip && ie.stie, id = 5, privilege = 1, delegators = List(Delegator(m.ideleg.st, 3)))
        hartIo.spec.addInterrupt(ip.seipOr && ie.seie, id = 9, privilege = 1, delegators = List(Delegator(m.ideleg.se, 3)))

        for ((id, enable) <- m.edeleg.mapping) hartIo.spec.exception += ExceptionSpec(id, List(Delegator(enable, 3)))

        if (XLEN.get == 64) {
          api.read(CSR.MSTATUS, 32 -> U"10")
          api.read(CSR.SSTATUS, 32 -> U"10")
        }
      }
    }

    val defaultTrap = new Area {
      val csrPrivilege = cap.onDecodeAddress(8, 2 bits)
      val csrReadOnly = cap.onDecodeAddress(10, 2 bits) === U"11"
      when(csrReadOnly && cap.onDecodeWrite || csrPrivilege > csrs.reader(cap.onDecodeHartId)(_.privilege)) {
        cap.onDecodeTrap()
      }
    }

    val readAnyWriteLegal = new Area {
      val tvecFilter = CsrListFilter(List(CSR.MTVEC) ++ p.withSupervisor.option(CSR.STVEC))
      val epcFilter = CsrListFilter(List(CSR.MEPC) ++ p.withSupervisor.option(CSR.SEPC))
      cap.onWrite(tvecFilter, false) {
        cap.onWriteBits(0, 2 bits) := 0
      }
      cap.onWrite(epcFilter, false) {
        cap.onWriteBits(0, log2Up(Fetch.SLICE_BYTES) bits) := 0
      }
    }


    ramRetainers.csr.release()
    trapLock.await()

    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      val hartIo = io.harts(hartId)
      val csr = csrs(hartId)

      val crsPorts = withRam generate new Area{
        val read = crs.ramReadPort(CsrRamService.priority.TRAP)
        read.valid := False
        read.address.assignDontCare()

        val write = crs.ramWritePort(CsrRamService.priority.TRAP)
        write.valid := False
        write.address.assignDontCare()
        write.data.assignDontCare()
      }

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
          privilegs = 1 :: privilegs
          privilegeAllowInterrupts += 1 -> ((csr.s.status.sie && !csr.withMachinePrivilege) || !csr.withSupervisorPrivilege)
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
              U"0" @@ csr.s.status.spp
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

        val pcPort = pcs.newJumpInterface(Ages.TRAP, 0, 0)
        pcPort.valid := False
        pcPort.hartId := hartId
        pcPort.pc.assignDontCare()
        val fsm = new StateMachine {
          val RUNNING = makeInstantEntry()
          val PROCESS = new State()
          val TRAP_EPC, TRAP_TVAL, TRAP_TVEC, TRAP_APPLY = new State()
          val XRET_EPC, XRET_APPLY = new State()

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
            val trap = new Area{
              val interrupt = pending.state.code === TrapReason.INTERRUPT && i.valid
              val targetPrivilege = interrupt.mux(i.targetPrivilege, exception.targetPrivilege)
              val tval = pending.state.tval.andMask(!interrupt)
              val code = interrupt.mux(i.code, pending.state.code)
            }
          }


          RUNNING.whenIsActive {
            when(trigger.valid) {
              buffer.sampleIt := True
              goto(PROCESS)
            }
          }

          PROCESS.whenIsActive{
            when(pending.state.exception || buffer.trap.interrupt) {
              goto(TRAP_TVAL)
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
                default {
                  assert(False, "Unexpected trap reason")
                }
              }
            }
          }

          TRAP_TVAL.whenIsActive {
            crsPorts.write.valid := True
            crsPorts.write.address := privilegeMux(buffer.trap.targetPrivilege)(
              csr.m.tval.getAddress(),
              csr.s.tval.getAddress()
            )
            crsPorts.write.data := S(buffer.trap.tval).resize(XLEN).asBits
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
            crsPorts.write.data := S(pending.pc, XLEN bits).asBits //TODO PC sign extends ? (DONE)
            when(crsPorts.write.ready) {
              goto(TRAP_TVEC)
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
            pcPort.valid := True
            pcPort.pc := U(readed).resized //PC RESIZED

            csr.privilege := buffer.trap.targetPrivilege
            switch(buffer.trap.targetPrivilege) {
              is(3) {
                csr.m.status.mie := False
                csr.m.status.mpie := csr.m.status.mie
                if (p.withUser) csr.m.status.mpp := csr.privilege

                csr.m.cause.code := buffer.trap.code
                csr.m.cause.interrupt := buffer.trap.interrupt
              }
              p.withSupervisor generate is(1) {
                csr.s.status.sie := False
                csr.s.status.spie := csr.s.status.sie
                if (p.withUser) csr.s.status.spp := csr.privilege(0, 1 bits)

                csr.s.cause.code := buffer.trap.code
                csr.s.cause.interrupt := buffer.trap.interrupt
              }
            }

            whitebox.trap := True
            whitebox.interrupt := buffer.trap.interrupt
            whitebox.code :=  buffer.trap.code

            goto(RUNNING)
          }

          val xretPrivilege = U(pending.state.tval(1 downto 0))
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
            switch(pending.state.tval(1 downto 0)) {
              is(3) {
                if(p.withUser) csr.m.status.mpp := 0
                csr.m.status.mie := csr.m.status.mpie
                csr.m.status.mpie := True
              }
              p.withSupervisor generate is(1) {
                csr.s.status.spp := U"0"
                csr.s.status.sie := csr.s.status.spie
                csr.s.status.spie := True
              }
            }
            goto(RUNNING)
          }
        }
      }
    }

    ramRetainers.port.release()
    buildBefore.release()
  }
}



//          COMPLETION.whenIsActive {
//            when(pending.state.exception || pending.state.code === TrapReason.INTERRUPT && buffer.i.valid) {
//              pcPort.valid := True
//              pcPort.pc := csrs(hartId).m.tvec
//
//              csr.m.epc := pending.pc
//              csr.m.tval := pending.state.tval.andMask(pending.state.exception)
//              csr.m.status.mie := False
//              csr.m.status.mpie := csr.m.status.mie
//              csr.m.status.mpp := csr.privilege
//              csr.m.cause.code := trapCode
//              csr.m.cause.interrupt := !pending.state.exception
//              csr.privilege := exception.targetPrivilege ???
//
//              whitebox.trap := True
//              whitebox.interrupt := !pending.state.exception
//              whitebox.code := trapCode
//            } otherwise {
//              switch(pending.state.code) {
//                is(TrapReason.INTERRUPT) {
//                  assert(!buffer.i.valid)
//                  pcPort.valid := True
//                  pcPort.pc := pending.pc
//                }
//                is(TrapReason.PRIV_RET){
//                  pcPort.valid := True
//                  pcPort.pc := csr.m.epc
//
//                  csr.privilege := pending.xret.targetPrivilege
//                  csr.xretAwayFromMachine setWhen (pending.xret.targetPrivilege < 3)
//                  switch(pending.state.tval(1 downto 0)) {
//                    is(3) {
//                      csr.m.status.mpp := 0
//                      csr.m.status.mie := csr.m.status.mpie
//                      csr.m.status.mpie := True
//                    }
//                    p.withSupervisor generate is(1) {
//                      ???
////                      s.sstatus.spp := U"0"
////                      s.sstatus.sie := supervisor.sstatus.spie
////                      s.sstatus.spie := True
//                    }
//                  }
//                }
//                default {
//                  assert(False, "Unexpected trap reason")
//                }
//              }
//            }
//            goto(RUNNING)
//          }