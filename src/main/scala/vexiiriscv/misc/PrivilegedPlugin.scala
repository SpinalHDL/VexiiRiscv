package vexiiriscv.misc

import spinal.core.{Bool, _}
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.cpu.riscv.debug.{DebugDmToHartOp, DebugHartBus, DebugModule}
import spinal.lib.fsm._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamPlugin, CsrRamService, ExecuteLanePlugin}
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._
import vexiiriscv._
import vexiiriscv.decode.{AlignerPlugin, Decode, DecodePipelinePlugin, InjectorService}
import vexiiriscv.execute.fpu.FpuDirtyService
import vexiiriscv.fetch.{Fetch, PcService}
import vexiiriscv.schedule.{Ages, ScheduleService}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object PrivilegedParam{
  def base = PrivilegedParam(
    withSupervisor = false,
    withUser       = false,
    withUserTrap   = false,
    withRdTime     = false,
    withDebug      = false,
    vendorId       = 0,
    archId         = 5, //As spike
    impId          = 0,
    debugTriggers  = 0,
    debugTriggersLsu = false
  )
}

class LsuTriggerBus(triggers : Int) extends Bundle {
  val hartId = Global.HART_ID()
  val load, store = Bool()
  val virtual = Global.MIXED_ADDRESS()
  val size = UInt(2 bits)
  val hits = Bits(triggers bits)
}

trait LsuTriggerService{
  def getLsuTriggerBus() : LsuTriggerBus
}

case class PrivilegedParam(var withSupervisor : Boolean,
                           var withUser: Boolean,
                           var withUserTrap: Boolean,
                           var withRdTime : Boolean,
                           var withDebug: Boolean,
                           var debugTriggers : Int,
                           var debugTriggersLsu : Boolean,
                           var vendorId: Int,
                           var archId: Int,
                           var impId: Int){
  def check(): Unit = {
    assert(!(withSupervisor && !withUser))
  }
}

case class Delegator(var enable: Bool, privilege: Int)
case class InterruptSpec(var cond: Bool, id: Int, privilege: Int, delegators: List[Delegator])
case class ExceptionSpec(id: Int, delegators: List[Delegator])

class PrivilegedPlugin(val p : PrivilegedParam, val hartIds : Seq[Int]) extends FiberPlugin with CommitService with LsuTriggerService{
  def implementSupervisor = p.withSupervisor
  def implementUser = p.withUser
  def implementUserTrap = p.withUserTrap

  def getPrivilege(hartId : UInt) : UInt = logic.harts.map(_.privilege).read(hartId)


  override def getCommitMask(hartId: Int): Bits = logic.harts(hartId).commitMask

  val misaIds = mutable.LinkedHashSet[Int]()
  def addMisa(id: Char): Unit = addMisa(id - 'A')
  def addMisa(id: Int) = {
    misaIds += id
  }

  def hart(id : Int) = logic.harts(id)

  val PC_TRIGGER_HITS = Payload(Bits(p.debugTriggers bits))


  override def getLsuTriggerBus(): LsuTriggerBus = api.lsuTriggerBus

  val api = during build new Area{
    val lsuTriggerBus = new LsuTriggerBus(p.debugTriggers)
    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      val allowInterrupts      = True
      val allowException       = True
      val allowEbreakException = True
      val fpuEnable = False
    }
  }

  def inhibateInterrupts(hartId : Int): Unit = api.harts(hartId).allowInterrupts := False
  def inhibateException(hartId : Int): Unit = api.harts(hartId).allowException := False
  def inhibateEbreakException(hartId : Int): Unit = api.harts(hartId).allowEbreakException := False
  def fpuEnable(hartId : Int) = api.harts(hartId).fpuEnable

  val logic = during setup new Area {
    val cap = host[CsrAccessPlugin]
    val pp = host[PipelineBuilderPlugin]
    val ap = host[AlignerPlugin]
    val pcs = host[PcService]
    val tp = host[TrapPlugin]
    val ss = host[ScheduleService]
    val dpp = host[DecodePipelinePlugin]
    val withRam = host.get[CsrRamService].nonEmpty
    val crs = withRam generate host[CsrRamService]
    val injs = host[InjectorService]
    val buildBefore = retains(List(cap.csrLock, injs.injectRetainer, pcs.elaborationLock, tp.trapLock, dpp.elaborationLock, ss.elaborationLock) ++ withRam.option(crs.csrLock))

    awaitBuild()
    p.check()

    addMisa('I')
    if (RVC) addMisa('C')
    if (RVF) addMisa('F')
    if (RVD) addMisa('D')
    if (RVA) addMisa('A')
    if (RVM) addMisa('M')
    if (p.withUser) addMisa('U')
    if (p.withSupervisor) addMisa('S')

    val causesWidthMins = host.list[CauseUser].map(_.getCauseWidthMin())
    CODE_WIDTH.set((4 +: causesWidthMins).max)

    assert(HART_COUNT.get == 1)

    val rdtime = in UInt (64 bits)
    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val xretAwayFromMachine = False
      val commitMask = Bits(host.list[ExecuteLanePlugin].size bits)
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
      val spec = new Area {
        val interrupt = ArrayBuffer[InterruptSpec]()
        val exception = ArrayBuffer[ExceptionSpec]()

        def addInterrupt(cond: Bool, id: Int, privilege: Int, delegators: List[Delegator]): Unit = {
          interrupt += InterruptSpec(cond, id, privilege, delegators)
        }
      }

      val api = cap.hart(hartId)
      val withFs = RVF || p.withSupervisor
      val privilege = RegInit(U"11")
      val withMachinePrivilege = privilege >= U"11"
      val withSupervisorPrivilege = privilege >= U"01"


      val debug = p.withDebug generate new Area{
        val injector = p.withDebug generate injs.injectPort()
        val bus = slave(DebugHartBus())
        val running = RegInit(True)
        val debugMode = !running
        val fetchHold = pcs.newHoldPort(hartId)
        fetchHold := !running

        bus.resume.rsp.valid := False

        bus.running := running
        bus.halted := !running
        bus.unavailable := BufferCC(ClockDomain.current.isResetActive)
        when(debugMode) {
          inhibateInterrupts(hartId)
        }

        val reseting = RegNext(False) init (True)
        bus.haveReset := RegInit(False) setWhen (reseting) clearWhen (bus.ackReset)

        val enterHalt = running.getAheadValue().fall(False)
        val doHalt = RegInit(False) setWhen (bus.haltReq && bus.running && !debugMode) clearWhen (enterHalt)
        val forceResume = False
        val doResume = forceResume || bus.resume.isPending(1)

        val dataCsrr = new Area {
          val state = RegInit(U(0, log2Up(XLEN / 32) bits))
          bus.hartToDm.valid := False
          bus.hartToDm.address := state.resized
          bus.hartToDm.data := cap.bus.write.bits.subdivideIn(32 bits).read(state)
          api.onWrite(DebugModule.CSR_DATA, onlyOnFire = false) {
            when(state =/= state.maxValue) {
              cap.bus.write.doHalt()
            }
            bus.hartToDm.valid := True
            state := (state + 1).resized
          }
        }

        val withDebugFpuAccess = false //withPrivilegedDebug && pipeline.config.FLEN == 64 && XLEN == 32
        val dataCsrw = new Area {
          val value = Vec.fill(1 + withDebugFpuAccess.toInt)(Reg(Bits(XLEN bits)))

          val fromDm = new Area {
            when(bus.dmToHart.valid && bus.dmToHart.op === DebugDmToHartOp.DATA) {
              value((bus.dmToHart.address >> (XLEN/32-1)).resized).subdivideIn(32 bits).onSel(bus.dmToHart.address, relaxedWidth = true) { chunk =>
                chunk := bus.dmToHart.data
              }
            }
          }

          val toHart = new Area {
            api.read(value(0), DebugModule.CSR_DATA)
          }
        }

        val inject = new Area {
          val cmd = bus.dmToHart.takeWhen(bus.dmToHart.op === DebugDmToHartOp.EXECUTE || bus.dmToHart.op === DebugDmToHartOp.REG_READ || bus.dmToHart.op === DebugDmToHartOp.REG_WRITE)
          val buffer = cmd.toStream.stage
          injector.valid := buffer.valid && buffer.op === DebugDmToHartOp.EXECUTE
          injector.payload := buffer.data

          buffer.ready := injector.fire
          val fpu = withDebugFpuAccess generate new Area {
            ???
          }

          if (!withDebugFpuAccess) bus.regSuccess := False

          val pending = RegInit(False) setWhen (cmd.valid && bus.dmToHart.op === DebugDmToHartOp.EXECUTE) clearWhen (bus.exception || bus.commit || bus.ebreak || bus.redo)
          bus.redo := pending && tp.api.harts(hartId).redo

          val commited = Reg(Bool()) clearWhen(cmd.fire) setWhen(commitMask.orR)
          bus.commit := pending && commited && !tp.api.harts(hartId).fsmBusy
        }

        val dpc = crs.readWriteRam(CSR.DPC)
        val dcsr = new Area {
          val prv       = RegInit(U"11")
          val step      = RegInit(False) //TODO
          val nmip      = False
          val mprven    = True
          val cause     = RegInit(U"000")
          val stoptime  = RegInit(False)
          val stopcount = RegInit(False)
          val stepie    = RegInit(False) //TODO
          val ebreaku   = p.withUser generate RegInit(False)
          val ebreaks   = p.withSupervisor generate RegInit(False)
          val ebreakm   = RegInit(False)
          val xdebugver = U(4, 4 bits)

          val stepLogic = new StateMachine {
            val IDLE, SINGLE, WAIT = new State()
            setEntry(IDLE)

            val stepped = Reg(Bool()) setWhen(commitMask.orR || tp.api.harts(hartId).rvTrap)
            val counter = Reg(UInt(2 bits)) init(0)
            host[AlignerPlugin].api.singleFetch setWhen(step)

            IDLE whenIsActive {
              when(step && bus.resume.rsp.valid) {
                goto(SINGLE)
              }
            }
            SINGLE whenIsActive {
              when(ap.api.downMoving) {
                stepped := False
                counter := 0
                goto(WAIT)
              }
            }

            WAIT whenIsActive {
              ap.api.haltIt := True
              when(tp.api.harts(hartId).redo) {
                goto(SINGLE)
              }
              when(stepped && !tp.api.harts(hartId).fsmBusy) {
                doHalt := True
                counter := counter + 1
                when(counter.andR) {
                  goto(IDLE)
                }
              }
            }
            always {
              when(enterHalt) {
                goto(IDLE)
              }
            }
            build()
          }


          api.read(CSR.DCSR, 3 -> nmip, 6 -> cause, 28 -> xdebugver, 4 -> mprven)
          api.readWrite(CSR.DCSR, 0 -> prv, 2 -> step, 9 -> stoptime, 10 -> stopcount, 11 -> stepie, 15 -> ebreakm)
          if (p.withSupervisor) api.readWrite(CSR.DCSR, 13 -> ebreaks)
          if (p.withUser) api.readWrite(CSR.DCSR, 12 -> ebreaku)

          when(debugMode || step || bus.haltReq) {
            tp.askWake(hartId)
          }
        }
        val stoptime = out(RegNext(debugMode && dcsr.stoptime) init(False))

        val trigger = (p.debugTriggers > 0) generate new Area {
          val tselect = new Area {
            val index = Reg(UInt(log2Up(p.debugTriggers) bits))
            api.readWrite(index, CSR.TSELECT)

            val outOfRange = if (isPow2(p.debugTriggers)) False else index < p.debugTriggers
          }

          //TODO may remove tinfo, as it is optional
          val tinfo = new Area {
            api.read(CSR.TINFO, 0 -> tselect.outOfRange, 2 -> !tselect.outOfRange)
          }

          val pcBreakMatchAt = 0
          val pcBreakTrapAt = 1

          val pcBreaks = for(laneId <- 0 until Decode.LANES) yield new dpp.LaneArea(pcBreakTrapAt, laneId) {
            val doIt = isValid && PC_TRIGGER_HITS.orR && !(0 until laneId).map(dpp.ctrl(pcBreakTrapAt).lane).map(lane => lane.isValid && lane.up(TRAP)).orR
            when(doIt) {
              bypass(Global.TRAP) := True
            }

            val trapPort = tp.newTrap(dpp.getAge(pcBreakTrapAt), Decode.LANES, subAge = 1)
            trapPort.valid     := doIt
            trapPort.exception := False
            trapPort.tval      := B(OHToUInt(PC_TRIGGER_HITS)).resized
            trapPort.code      := TrapReason.DEBUG_TRIGGER
            trapPort.arg       := 0
            trapPort.laneAge   := laneId
            trapPort.hartId    := HART_ID

            val flushPort = ss.newFlushPort(dpp.getAge(pcBreakTrapAt), log2Up(Decode.LANES), true)
            flushPort.valid   := doIt
            flushPort.hartId  := Global.HART_ID
            flushPort.uopId   := Decode.UOP_ID
            flushPort.laneAge := laneId
            flushPort.self    := False
          }

          val slots = for (slotId <- 0 until p.debugTriggers) yield new Area {
            val selected = tselect.index === slotId

            def csrw(csrId: Int, thats: (Int, Data)*): Unit = {
              api.onWrite(csrId, false) {
                when(selected) {
                  for ((offset, data) <- thats) {
                    data.assignFromBits(cap.bus.write.bits(offset, widthOf(data) bits))
                  }
                }
              }
            }

            def csrr(csrId: Int, read: Bits, thats: (Int, Data)*): Unit = {
              when(selected) {
                for ((offset, data) <- thats) {
                  read(offset, widthOf(data) bits) := data.asBits
                }
              }
            }

            def csrrw(csrId: Int, read: Bits, thats: (Int, Data)*): Unit = {
              csrw(csrId, thats: _*)
              csrr(csrId, read, thats: _*)
            }

            val tdata1 = new Area {
              val read = B(0, XLEN bits)
              val tpe = U(2, 4 bits)
              val dmode = Reg(Bool()) init (False) //TODO

              val execute = RegInit(False)
              val m, s, u = RegInit(False)
              val action = RegInit(U"0000")
              val privilegeHit = !debugMode && privilege.mux(
                0 -> u,
                1 -> s,
                3 -> m,
                default -> False
              )
              val hit = RegInit(False)
              val size = Reg(UInt((XLEN.get == 32).mux(2, 4) bits)) init (0)

              csrrw(CSR.TDATA1, read, 2 -> execute, 3 -> u, 4 -> s, 6 -> m, XLEN - 5 -> dmode, 12 -> action, 20 -> hit, 16 -> size(1 downto 0))
              if(size.getWidth > 2) csrrw(CSR.TDATA1, read, 21 -> size(3 downto 2))
              csrr(CSR.TDATA1, read, XLEN - 4 -> tpe)
              csrr(CSR.TDATA1, read, 21 -> B(12))

              val load, store = RegInit(False)
              val chain = RegInit(False).allowUnsetRegToAvoidLatch
              val select = False
              val matcher = Reg(Bits(4 bits)) init (0)
              if (p.debugTriggersLsu) {
                csrrw(CSR.TDATA1, read, 11 -> chain, 0 -> load, 1 -> store, 7 -> matcher)
                csrr(CSR.TDATA1, read, 18 -> (load && select))
              }
            }

            val chainBroken = Bool()
            val tdata2 = new Area {
              val value = Reg(Bits(Global.MIXED_WIDTH bits))
              csrw(CSR.TDATA2, 0 -> value)
              val enabled = !debugMode && tdata1.action === 1 && tdata1.privilegeHit// && !chainBroken


              val execute = for (laneId <- 0 until Decode.LANES) yield new dpp.LaneArea(pcBreakMatchAt, laneId) {
                PC_TRIGGER_HITS(slotId) := enabled && tdata1.execute && U(value) === Global.PC
              }

              val lsu = p.debugTriggersLsu generate new Area {
                val lsuTrigger = getLsuTriggerBus()
                val sizeSpec = ArrayBuffer[(Any, Bool)](
                  0 -> True,
                  1 -> (lsuTrigger.size === 0),
                  2 -> (lsuTrigger.size === 1),
                  3 -> (lsuTrigger.size === 2)
                )
                if(XLEN.get >= 64) {
                  sizeSpec += 5 -> (lsuTrigger.size === 3)
                  sizeSpec += (default -> False)
                }

                val sizeOk = tdata1.size.muxList(sizeSpec)
                val matcher = new Area {
                  val cpu = B(lsuTrigger.virtual)
                  val cmp = U(cpu) < U(value)

                  val mask = B(Global.MIXED_WIDTH - 12 bits, default -> true) ## Napot(value(12 - 2 downto 0)).orMask(tdata1.matcher =/= 1)
                  val cpuMasked = CombInit(cpu)
//                  when(tdata1.matcher === 4 || tdata1.matcher === 5) {
//                    cpuMasked(15 downto 0) := cpu.subdivideIn(2 slices)(U(tdata1.matcher.lsb)) & value(31 downto 16)
//                  }
                  val maskHits = (~((B(cpuMasked) & mask) ^ (value & mask))).subdivideIn(2 slices).map(_.andR)
                  val maskHit = maskHits.andR

                  val ok = tdata1.matcher.mux(
                    0 -> maskHit,
                    1 -> maskHit,
                    2 -> !cmp,
                    3 -> cmp,
//                    4 -> maskHits(0),
//                    5 -> maskHits(0),
                    default -> False
                  )
                }
                val virtualTrigger = (tdata1.store && lsuTrigger.store || tdata1.load && lsuTrigger.load)
                val hitNoChain = enabled && !chainBroken && sizeOk && matcher.ok && virtualTrigger
                val hit = hitNoChain && !tdata1.chain
              }
            }
          }

          for (slotId <- slots.indices) {
            val slot = slots(slotId)
            slotId match {
              case 0 => slot.chainBroken := False
              case _ => {
                val prev = slots(slotId - 1)
                slot.chainBroken := prev.tdata1.chain && (prev.chainBroken || p.debugTriggersLsu.mux(!prev.tdata2.lsu.hitNoChain, False))
              }
            }

            val lsuTrigger = getLsuTriggerBus()
            lsuTrigger.hits(slotId) := p.debugTriggersLsu.mux(slot.tdata2.lsu.hit, False)
          }

          api.read(CSR.TDATA1, 0 -> slots.map(_.tdata1.read).read(tselect.index))
          api.read(CSR.TDATA2, 0 -> S(slots.map(_.tdata2.value).read(tselect.index)).resize(XLEN))
        }
      }
      val m = new Area {
        api.read(U(p.vendorId), CSR.MVENDORID) // MRO Vendor ID.
        api.read(U(p.archId), CSR.MARCHID) // MRO Architecture ID.
        api.read(U(p.impId), CSR.MIMPID) // MRO Implementation ID.
        api.read(U(hartIds(hartId)), CSR.MHARTID) // MRO Hardware thread ID.Machine Trap Setup
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
          if (RVF) {
            fpuEnable(hartId) setWhen (fs =/= 0)
            when(host.list[FpuDirtyService].map(_.gotDirty()).orR){
              fs := 3
            }
          }
          if (withFs) sd setWhen (fs === 3)


          readWrite(7 -> mpie, 3 -> mie)
          read(11 -> mpp)
          if (p.withUser) {
            onWrite(true) {
              switch(cap.bus.write.bits(12 downto 11)) {
                is(3) { mpp := 3 }
                if (p.withSupervisor) is(1) { mpp := 1 }
                is(0) { mpp := 0 }
              }
            }
          }
          read(XLEN - 1 -> sd)
          if (withFs) readWrite(13 -> fs)
          if (p.withUser && XLEN.get == 64) read(32 -> U"10")
          if (p.withSupervisor && XLEN.get == 64) read(34 -> U"10")
        }

        val cause = new api.Csr(CSR.MCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)
          readWrite(XLEN - 1 -> interrupt, 0 -> code)
        }

        val ip = new api.Csr(CSR.MIP) {
          val meip = RegNext(int.m.external) init (False)
          val mtip = RegNext(int.m.timer) init (False)
          val msip = RegNext(int.m.software) init (False)
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

        val tvec = crs.readWriteRam(CSR.MTVEC)
        val tval = crs.readWriteRam(CSR.MTVAL)
        val epc  = crs.readWriteRam(CSR.MEPC)
        val scratch = crs.readWriteRam(CSR.MSCRATCH)

        spec.addInterrupt(ip.mtip && ie.mtie, id = 7, privilege = 3, delegators = Nil)
        spec.addInterrupt(ip.msip && ie.msie, id = 3, privilege = 3, delegators = Nil)
        spec.addInterrupt(ip.meip && ie.meie, id = 11, privilege = 3, delegators = Nil)
      }

      val s = p.withSupervisor generate new Area {
        val cause = new api.Csr(CSR.SCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)
          readWrite(XLEN - 1 -> interrupt, 0 -> code)
        }

        val status = new Area {
          val sie, spie = RegInit(False)
          val spp = RegInit(U"0")

          api.read(CSR.SSTATUS, XLEN - 1 -> m.status.sd)
          for (offset <- List(CSR.MSTATUS, CSR.SSTATUS)) {
            api.readWrite(offset, 8 -> spp, 5 -> spie, 1 -> sie)
          }
          if (XLEN.get == 64) api.read(CSR.SSTATUS, 32 -> U"10")
        }

        val ip = new Area {
          val seipSoft = RegInit(False)
          val seipInput = RegNext(int.s.external)
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


        spec.addInterrupt(ip.ssip && ie.ssie, id = 1, privilege = 1, delegators = List(Delegator(m.ideleg.ss, 3)))
        spec.addInterrupt(ip.stip && ie.stie, id = 5, privilege = 1, delegators = List(Delegator(m.ideleg.st, 3)))
        spec.addInterrupt(ip.seipOr && ie.seie, id = 9, privilege = 1, delegators = List(Delegator(m.ideleg.se, 3)))

        for ((id, enable) <- m.edeleg.mapping) spec.exception += ExceptionSpec(id, List(Delegator(enable, 3)))
      }

      if (p.withRdTime) {
        XLEN.get match {
          case 32 => {
            api.read(rdtime(31 downto 0), CSR.UTIME)
            api.read(rdtime(63 downto 32), CSR.UTIMEH)
          }
          case 64 => {
            api.read(rdtime, CSR.UTIME)
          }
        }
      }
    }

    val defaultTrap = new Area {
      val csrPrivilege = cap.bus.decode.address(8, 2 bits)
      val csrReadOnly = cap.bus.decode.address(10, 2 bits) === U"11"
      when(csrReadOnly && cap.bus.decode.write || csrPrivilege > harts.reader(cap.bus.decode.hartId)(_.privilege)) {
        cap.bus.decode.doException()
      }
    }

    val readAnyWriteLegal = new Area {
      val tvecFilter = CsrListFilter(List(CSR.MTVEC) ++ p.withSupervisor.option(CSR.STVEC))
      val epcFilter = CsrListFilter(List(CSR.MEPC) ++ p.withSupervisor.option(CSR.SEPC))
      cap.onWrite(tvecFilter, false) {
        cap.bus.write.bits(0, 2 bits) := 0
      }
      cap.onWrite(epcFilter, false) {
        cap.bus.write.bits(0, log2Up(Fetch.SLICE_BYTES) bits) := 0
      }
    }

    buildBefore.release()
  }
}





//        val tval = Reg(TVAL) init (0)
//        val epc = Reg(PC) init (0)
//        val tvec = Reg(PC) init (0)

//        api.onCsr(CSR.MTVAL).readWrite(tval)
//        api.onCsr(CSR.MEPC).readWrite(epc)
//        api.onCsr(CSR.MTVEC).readWrite(tvec)
