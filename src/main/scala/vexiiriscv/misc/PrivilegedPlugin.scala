package vexiiriscv.misc

import spinal.core.{Bool, _}
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.cpu.riscv.debug.{DebugDmToHartOp, DebugHartBus, DebugModule}
import spinal.lib.fsm._
import spinal.lib.misc.aia._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrCondFilter, CsrListFilter, CsrRamPlugin, CsrRamService, ExecuteLanePlugin}
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
    withHypervisor = false,
    withRdTime     = false,
    withSSTC       = false,
    withDebug      = false,
    withXs         = false,
    mstatusFsInit  = 0,
    vendorId       = 0,
    archId         = 46, //As spike
    impId          = 0,
    imsicInterrupts = 0,
    debugTriggers  = 0,
    debugTriggersLsu = false,
    withHartIdInputDefaulted = false,
    withInterrutpFilter = false
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
                           var withHypervisor : Boolean,
                           var withRdTime : Boolean,
                           var withSSTC : Boolean,
                           var withDebug: Boolean,
                           var withXs : Boolean,
                           var withInterrutpFilter : Boolean,
                           var mstatusFsInit : Int,
                           var debugTriggers : Int,
                           var debugTriggersLsu : Boolean,
                           var vendorId: Int,
                           var archId: Int,
                           var impId: Int,
                           var imsicInterrupts: Int,
                           var withHartIdInputDefaulted : Boolean){
  def withImsic = imsicInterrupts > 0

  def check(): Unit = {
    assert(!(withSupervisor && !withUser))
    assert((withSSTC && withSupervisor && withRdTime) || !withSSTC)
    assert((imsicInterrupts == 0) || (isPow2(imsicInterrupts) && imsicInterrupts >= 64 && imsicInterrupts <= 2048))
    assert(!withHypervisor || (withSupervisor && withRdTime))
  }
}

case class Delegator(var enable: Bool, privilege: Int)
case class InterruptSpec(var cond: Bool, id: Int, privilege: Int, priority: Int, delegators: List[Delegator])
case class ExceptionSpec(id: Int, delegators: List[Delegator])

/**
 * This implements the large majority of the RISC-V privileged spec :
 * - Most CSR
 * - Interrupts
 * - Debug interface (RISC-V debug spec)
 */
class PrivilegedPlugin(val p : PrivilegedParam, val hartIds : Seq[Int]) extends FiberPlugin with CommitService with LsuTriggerService{
  def implementSupervisor = p.withSupervisor
  def implementUser = p.withUser
  def implementUserTrap = p.withUserTrap

  def getPrivilege(hartId : UInt) : SInt = logic.harts.map(_.privilege).read(hartId)
  def isMachine(hartId : UInt) : Bool = getPrivilege(hartId) === PrivilegeMode.M
  def isSupervisor(hartId : UInt) : Bool = getPrivilege(hartId) === PrivilegeMode.S
  def isUSer(hartId : UInt) : Bool = getPrivilege(hartId) === PrivilegeMode.U


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
    val harts = for(_ <- 0 until HART_COUNT) yield new Area{
      val allowInterrupts      = True
      val allowException       = True
      val allowEbreakException = True
      val fpuEnable = False
      val hartId = (hartIds == null) generate in(UInt(32 bits))
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
    val withIndirectCsr = host.get[IndirectCsrPlugin].nonEmpty
    val indirect = withIndirectCsr generate host[IndirectCsrPlugin]
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
    if (p.withHypervisor) addMisa('H')

    val causesWidthMins = host.list[CauseUser].map(_.getCauseWidthMin())
    CODE_WIDTH.set((5 +: causesWidthMins).max)

    assert(HART_COUNT.get == 1)
    api.get

    val rdtime = in UInt (64 bits)
    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val indirectHart = withIndirectCsr generate indirect.logic.harts(hartId)
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
          val iprio = InterruptInfo.defaultOrder.indexOf(id)
          require(iprio != -1, "New registered interrupt must be added to InterruptPrio.defaultOrder")

          interrupt += InterruptSpec(cond, id, privilege, iprio + 1, delegators)
        }
      }

      val api = cap.hart(hartId)
      val withFs = RVF || p.withSupervisor
      val privilege = Reg(PrivilegeMode.TYPE()) init(PrivilegeMode.M)
      val withMachinePrivilege = privilege >= PrivilegeMode.M
      val withSupervisorPrivilege = privilege >= PrivilegeMode.S
      val withVirtualSupervisorPrivilege = privilege >= PrivilegeMode.VS
      val withGuestPrivilege = PrivilegeMode.isGuest(privilege)
      val withHostPrivilege = !withGuestPrivilege

      val hartRunning = RegInit(True).allowUnsetRegToAvoidLatch()
      val debugMode = !hartRunning

      val debug = p.withDebug generate new Area{
        val injector = p.withDebug generate injs.injectPort()
        val bus = slave(DebugHartBus())

        val fetchHold = pcs.newHoldPort(hartId)
        fetchHold := !hartRunning

        bus.resume.rsp.valid := False

        val reseting = RegNext(False) init (True)
        bus.haveReset := RegInit(False) setWhen (reseting) clearWhen (bus.ackReset)
        bus.running := hartRunning
        bus.halted := !hartRunning
        bus.unavailable := reseting

        when(debugMode) {
          inhibateInterrupts(hartId)
        }



        val enterHalt = hartRunning.getAheadValue().fall(False)
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
          val prv       = Reg(PrivilegeMode.TYPE()) init(PrivilegeMode.M)
          val step      = RegInit(False) //TODO
          val nmip      = False
          val mprven    = True
          val cause     = RegInit(U"000")
          val stoptime  = RegInit(True)
          val stopcount = RegInit(False)
          val stepie    = RegInit(False) //TODO
          val ebreaku   = p.withUser generate RegInit(False)
          val ebreaks   = p.withSupervisor generate RegInit(False)
          val ebreakm   = RegInit(False)
          val xdebugver = U(4, 4 bits)

          val stepLogic = new StateMachine {
            val IDLE, SINGLE, WAIT_IT = new State()
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
                goto(WAIT_IT)
              }
            }

            WAIT_IT whenIsActive {
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
      }

      val noTrigger = (p.debugTriggers == 0) generate new Area {
        cap.allowCsr(CSR.TSELECT)
        cap.allowCsr(CSR.TDATA1)
        cap.allowCsr(CSR.TDATA2)
      }
      val trigger = (p.debugTriggers > 0) generate new Area {
        val tselect = new Area {
          val index = Reg(UInt(log2Up(p.debugTriggers) bits)) init(0)
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
            val action = RegInit(U(0, p.withDebug.toInt bits))
            val privilegeHit = !debugMode && privilege.mux(
              PrivilegeMode.U -> u,
              PrivilegeMode.S -> s,
              PrivilegeMode.M -> m,
              default -> False
            )
            val hit = RegInit(False)
            val size = Reg(UInt((XLEN.get == 32).mux(2, 4) bits)) init (0)
            val doEbreak = action === 0

            csrrw(CSR.TDATA1, read, 2 -> execute, 3 -> u, 4 -> s, 6 -> m, XLEN - 5 -> dmode, 12 -> action, 20 -> hit, 16 -> size(1 downto 0))
            if(size.getWidth > 2) csrrw(CSR.TDATA1, read, 21 -> size(3 downto 2))
            csrr(CSR.TDATA1, read, XLEN - 4 -> tpe)

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
            val enabled = !debugMode && tdata1.privilegeHit// && !chainBroken


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
                val maskHit = (~((B(cpuMasked) & mask) ^ (value & mask))).andR

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

      val m = new Area {
        api.read(U(p.vendorId), CSR.MVENDORID) // MRO Vendor ID.
        api.read(U(p.archId), CSR.MARCHID) // MRO Architecture ID.
        api.read(U(p.impId), CSR.MIMPID) // MRO Implementation ID.
        hartIds match {
          case null => {
            assert(HART_COUNT.get == 1)
            api.read(PrivilegedPlugin.this.api.harts(hartId).hartId, CSR.MHARTID)
          }
          case _ => {
            if(p.withHartIdInputDefaulted){
              val hartIdDefaulted = in UInt(32 bits)
              hartIdDefaulted.default(hartIds(hartId))
              hartIdDefaulted.setName("hartId_" + hartId)
              api.read(hartIdDefaulted, CSR.MHARTID)
            } else {
              api.read(U(hartIds(hartId)), CSR.MHARTID)
            }

          } // MRO Hardware thread ID.Machine Trap Setup
        }

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
          val mpv = p.withHypervisor.mux(RegInit(False), False)
          val fs = withFs generate RegInit(U(p.mstatusFsInit, 2 bits))
          val sd = False
          val tsr, tvm = p.withSupervisor generate RegInit(False)
          val tw = p.withUser.mux(RegInit(False), False)
          val mprv = RegInit(False) clearWhen(xretAwayFromMachine)
          val xs = p.withXs generate RegInit(U(p.mstatusFsInit, 2 bits))
          val gva = p.withHypervisor generate RegInit(False)

          if (RVF) {
            fpuEnable(hartId) setWhen (fs =/= 0 && p.withHypervisor.mux(withHostPrivilege, True))
            when(withHostPrivilege && host.list[FpuDirtyService].map(_.gotDirty()).orR){
              fs := 3
            }
          }
          if (withFs) sd setWhen (fs === 3)
          if (p.withXs) sd setWhen (xs === 3)


          readWrite(7 -> mpie, 3 -> mie)
          read(11 -> mpp)
          if (p.withUser) {
            onWrite(true) {
              switch(cap.bus.write.bits(12 downto 11)) {
                is(PrivilegeMode.M) { mpp := PrivilegeMode.M }
                if (p.withSupervisor) is(PrivilegeMode.S) { mpp := PrivilegeMode.S }
                is(PrivilegeMode.U) { mpp := PrivilegeMode.U }
              }
            }
          }
          read(XLEN - 1 -> sd)
          readWrite(17 -> mprv)
          if (withFs) readWrite(13 -> fs)
          if (p.withXs) readWrite(15 -> xs)
          if (p.withUser && XLEN.get == 64) read(32 -> U"10")
          if (p.withSupervisor && XLEN.get == 64) read(34 -> U"10")
          if (p.withSupervisor) readWrite(22 -> tsr, 20 -> tvm)
          if (p.withUser) readWrite(21 -> tw)
          if (p.withHypervisor && XLEN.get == 64) readWrite(38 -> gva, 39 -> mpv)

          cap.trapNextOnWrite += CsrListFilter(List(CSR.MSTATUS)) // Status can have various side effect on the MMU and FPU
        }

        val statush = new api.Csr(CSR.MSTATUSH) {
          import status._

          if (p.withHypervisor && XLEN.get == 32) readWrite(6 -> gva, 7 -> mpv)

          cap.trapNextOnWrite += CsrListFilter(List(CSR.MSTATUSH)) // Status can have various side effect on the MMU and FPU
        }

        val cause = new api.Csr(CSR.MCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)
          readWrite(XLEN - 1 -> interrupt, 0 -> code)
        }

        val imsic = p.withImsic generate genImsicArea(CSR.MIREG, CSR.MTOPEI, indirectHart.m.csrFilter(_, _))

        val ip = new api.Csr(CSR.MIP) {
          val mext = if (p.withImsic) imsic.deliveryArbiter(int.m.external) else int.m.external

          val meip = RegNext(mext) init (False)
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
          val eh, vi, igpf, lgpf, sgpf = p.withHypervisor generate RegInit(False)
          val mapping = mutable.LinkedHashMap(0 -> iam, 3 -> bp, 8 -> eu, 9 -> es, 12 -> ipf, 13 -> lpf, 15 -> spf) ++ p.withHypervisor.mux(
            mutable.LinkedHashMap(10 -> eh, 20 -> igpf, 21 -> lgpf, 22 -> vi, 23 -> sgpf),
            mutable.LinkedHashMap()
          )
          for ((id, enable) <- mapping) readWrite(id -> enable)
        }

        val ideleg = p.withSupervisor generate new api.Csr(CSR.MIDELEG) {
          val st, se, ss = RegInit(False)
          readWrite(9 -> se, 5 -> st, 1 -> ss)

          if (p.withHypervisor) readWrite(10 -> True, 6 -> True, 2 -> True)
        }

        val tvec = crs.readWriteRam(CSR.MTVEC)
        val tval = crs.readWriteRam(CSR.MTVAL)
        val epc  = crs.readWriteRam(CSR.MEPC)
        val scratch = crs.readWriteRam(CSR.MSCRATCH)

        val tval2 = p.withHypervisor generate crs.readWriteRam(CSR.MTVAL2)
        val tinst = p.withHypervisor generate crs.readWriteRam(CSR.MTINST)

        spec.addInterrupt(ip.mtip && ie.mtie, id = 7, privilege = PrivilegeMode.M, delegators = Nil)
        spec.addInterrupt(ip.msip && ie.msie, id = 3, privilege = PrivilegeMode.M, delegators = Nil)
        spec.addInterrupt(ip.meip && ie.meie, id = 11, privilege = PrivilegeMode.M, delegators = Nil)

        val topi = new Area {
          val interrupt = Global.CODE().assignDontCare()
          val priority = Mux(interrupt === B(0), B(0), B(1))
          api.read(CSR.MTOPI, 0 -> priority, 16 -> interrupt)
        }
      }

      val mcounteren = p.withRdTime generate new Area {
        val tm = Reg(True)
        api.readWrite(tm, CSR.MCOUNTEREN, 1)
      }

      val h = p.withHypervisor generate new Area {
        val status = new api.Csr(CSR.HSTATUS) {
          val vtsr, vtvm = RegInit(False)
          val vtw = RegInit(False)
          val gva = RegInit(False)
          val spv, spvp = RegInit(False)

          readWrite(6 -> gva, 7 -> spv, 8 -> spvp, 20 -> vtvm, 21 -> vtw, 22 -> vtsr)
          if (XLEN.get == 64) read(32 -> U"10")
        }

        val counteren = p.withRdTime generate new Area {
          val tm = RegInit(False)
          api.readWrite(tm, CSR.HCOUNTEREN, 1)
        }

        val timedelta = p.withRdTime generate new Area {
          val delta = RegInit(U(0, 64 bits))
          val calibrated = rdtime + delta
          val accessable = mcounteren.tm && counteren.tm

          XLEN.get match {
            case 32 => {
              api.readWrite(delta(31 downto 0), CSR.HTIMEDELTA)
              api.readWrite(delta(63 downto 32), CSR.HTIMEDELTAH)

              api.read(calibrated(31 downto 0), GuestCsrFilter(CSR.UTIME))
              api.read(calibrated(63 downto 32), GuestCsrFilter(CSR.UTIMEH))
              api.allowCsr(GuestCsrFilter(CSR.UTIME), accessable)
              api.allowCsr(GuestCsrFilter(CSR.UTIMEH), accessable)
            }
            case 64 => {
              api.readWrite(delta, CSR.HTIMEDELTA)

              api.read(calibrated, GuestCsrFilter(CSR.UTIME))
              api.allowCsr(GuestCsrFilter(CSR.UTIME), accessable)
            }
          }
        }

        val edeleg = new api.Csr(CSR.HEDELEG) {
          val bp, eu, ipf, lpf, spf = RegInit(False)
          val mapping = mutable.LinkedHashMap(3 -> bp, 8 -> eu, 12 -> ipf, 13 -> lpf, 15 -> spf)
          for ((id, enable) <- mapping) readWrite(id -> enable)
        }

        val ideleg = new api.Csr(CSR.HIDELEG) {
          val vst, vse, vss = RegInit(False)
          readWrite(10 -> vse, 6 -> vst, 2 -> vss)
          api.readWrite(CSR.MIDELEG, 10 -> vse, 6 -> vst, 2 -> vss)
        }

        val ie = new api.Csr(CSR.HIE) {
          val vseie, vstie, vssie = RegInit(False)
          readWrite(10 -> vseie, 6 -> vstie, 2 -> vssie)
          api.readWrite(CSR.MIE, 10 -> vseie, 6 -> vstie, 2 -> vssie)
        }

        val ip = new Area {
          val vseip, vstip, vssip = RegInit(False)
        }

        // vseip
        api.readWrite(ip.vseip, CSR.HVIP, 10)
        api.read(ip.vseip, CsrListFilter(List(CSR.MIP, CSR.HIP)), 10)

        // vstip
        api.readWrite(ip.vstip, CSR.HVIP, 6)
        api.read(ip.vstip, CsrListFilter(List(CSR.MIP, CSR.HIP)), 6)

        // vssip
        api.readWrite(ip.vssip, CsrListFilter(List(CSR.MIP, CSR.HVIP, CSR.HIP)), 2)

        val tval = crs.readWriteRam(CSR.HTVAL)
        val tinst = crs.readWriteRam(CSR.HTINST)

        spec.addInterrupt(ie.vseie && ip.vseip && !ideleg.vse, id = 10, privilege = PrivilegeMode.S, delegators = List(Delegator(True, PrivilegeMode.M)))
        spec.addInterrupt(ie.vstie && ip.vstip && !ideleg.vst, id = 6, privilege = PrivilegeMode.S, delegators = List(Delegator(True, PrivilegeMode.M)))
        spec.addInterrupt(ie.vssie && ip.vssip && !ideleg.vss, id = 2, privilege = PrivilegeMode.S, delegators = List(Delegator(True, PrivilegeMode.M)))
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
          cap.trapNextOnWrite += CsrListFilter(List(CSR.SSTATUS))
        }

        val sstc = new Area {
          val envcfg = new Area {
            val enable = RegInit(False)
            val allowUpdate = Bool(p.withSSTC)

            if (XLEN.get == 32) {
              api.read(enable, CSR.MENVCFGH, 31)
              api.writeWhen(enable, allowUpdate, CSR.MENVCFGH, 31)
            } else {
              api.read(enable, CSR.MENVCFG, 63)
              api.writeWhen(enable, allowUpdate, CSR.MENVCFG, 63)
            }
          }

          val logic = p.withSSTC generate new Area {
            val cmp = RegInit(U(64 bits, default -> true))
            val ip = RegNext(rdtime >= cmp)

            val accessable =  withMachinePrivilege || (mcounteren.tm && envcfg.enable)

            if (XLEN.get == 32) {
              api.readWrite(cmp(31 downto 0), CSR.STIMECMP)
              api.readWrite(cmp(63 downto 32), CSR.STIMECMPH)
              api.allowCsr(CSR.STIMECMP, accessable)
              api.allowCsr(CSR.STIMECMPH, accessable)
            } else {
              api.readWrite(cmp, CSR.STIMECMP)
              api.allowCsr(CSR.STIMECMP, accessable)
            }
          }

          val interrupt = if (p.withSSTC) logic.ip else False
        }

        val imsic = p.withImsic generate genImsicArea(CSR.SIREG, CSR.STOPEI, indirectHart.s.csrFilter(_, _))

        val ip = new Area {
          val sext = if (p.withImsic) imsic.deliveryArbiter(int.s.external) else int.s.external

          val seipSoft = RegInit(False)
          val seipInput = RegNext(sext)
          val seipOr = seipSoft || seipInput
          val stipSoft = RegInit(False)
          val stipOr = Mux(sstc.envcfg.enable, sstc.interrupt, stipSoft)
          val ssip = RegInit(False)

          val seipMasked = seipOr && m.ideleg.se
          val stipMasked = stipOr && m.ideleg.st
          val ssipMasked = ssip && m.ideleg.ss
        }

        val ie = new Area {
          val seie, stie, ssie = RegInit(False)
        }

        // IE bits when mideleg = 0 and mvien = 1
        val ieShadow = p.withInterrutpFilter generate new Area {
          val seie, ssie = RegInit(False)
        }

        val vie = p.withInterrutpFilter generate new api.Csr(CSR.MVIEN) {
          val seie, ssie = RegInit(False)
          readWrite(9 -> seie, 1 -> ssie)
        }

        val vip = p.withInterrutpFilter generate new Area {
          val seip, ssip = RegInit(False)

          api.readWrite(seip, CsrCondFilter(CSR.MVIP, vie.seie), 9)
          api.read(ip.seipOr, CsrCondFilter(CSR.MVIP, !vie.seie), 9)
          api.write(ip.seipSoft, CsrCondFilter(CSR.MVIP, !vie.seie), 9)
          api.readWrite(ssip, CsrCondFilter(CSR.MVIP, vie.ssie), 1)
          api.readWrite(ip.ssip, CsrCondFilter(CSR.MVIP, !vie.ssie), 1)
          api.read(ip.stipOr, CSR.MVIP, 5)
          api.writeWhen(ip.stipSoft, !sstc.envcfg.enable, CSR.MVIP, 5)
        }

        val tvec = crs.readWriteRam(CSR.STVEC)
        val tval = crs.readWriteRam(CSR.STVAL)
        val epc = crs.readWriteRam(CSR.SEPC)
        val scratch = crs.readWriteRam(CSR.SSCRATCH)

        if (withFs) api.readWrite(CSR.SSTATUS, 13 -> m.status.fs)
        if (p.withXs) api.readWrite(CSR.SSTATUS, 15 -> m.status.xs)

        val iepNoFilter = !p.withInterrutpFilter generate new Area {
          def mapSie(supervisorCsr: Int, bitId: Int, reg: Bool, machineDeleg: Bool, sWrite: Boolean = true): Unit = {
            api.read(reg && machineDeleg, supervisorCsr, bitId)
            if (sWrite) api.writeWhen(reg, machineDeleg, supervisorCsr, bitId)
          }

          mapSie(CSR.SIE, 9, ie.seie, m.ideleg.se)
          mapSie(CSR.SIE, 5, ie.stie, m.ideleg.st)
          mapSie(CSR.SIE, 1, ie.ssie, m.ideleg.ss)

          api.read(ip.seipOr && m.ideleg.se, CSR.SIP, 9)
          mapSie(CSR.SIP, 1, ip.ssip, m.ideleg.ss)
        }

        val iepFilter = p.withInterrutpFilter generate new Area {
          def mapSie(supervisorCsr: Int, bitId: Int, reg: Bool, machineDeleg: Bool, sWrite: Boolean = true): Unit = {
            api.read(reg, CsrCondFilter(supervisorCsr, machineDeleg), bitId)
            if (sWrite) api.write(reg, CsrCondFilter(supervisorCsr, machineDeleg), bitId)
          }

          def mapVie(supervisorCsr: Int, bitId: Int, reg: Bool, machineDeleg: Bool, virtualEnable: Bool, sWrite: Boolean = true): Unit = {
            api.read(reg && virtualEnable, CsrCondFilter(supervisorCsr, !machineDeleg), bitId)
            if (sWrite) api.writeWhen(reg, virtualEnable, CsrCondFilter(supervisorCsr, !machineDeleg), bitId)
          }

          mapSie(CSR.SIE, 9, ie.seie, m.ideleg.se)
          mapVie(CSR.SIE, 9, ieShadow.seie, m.ideleg.se, vie.seie)
          // mapMie(CSR.MIE, CSR.SIE, 5, ie.stie, m.ideleg.st)
          api.read(ie.stie && m.ideleg.st, CSR.SIE, 9)
          api.writeWhen(ie.stie, m.ideleg.st, CSR.SIE, 9)
          mapSie(CSR.SIE, 1, ie.ssie, m.ideleg.ss)
          mapVie(CSR.SIE, 1, ieShadow.ssie, m.ideleg.ss, vie.ssie)

          // seip
          api.read(ip.seipOr, CsrCondFilter(CSR.SIP, m.ideleg.se), 9)
          api.read(vip.seip && vie.seie, CsrCondFilter(CSR.SIP, !m.ideleg.se), 9)
          api.writeWhen(vip.seip, vie.seie, CsrCondFilter(CSR.SIP, !m.ideleg.se), 9)

          // ssip
          mapSie(CSR.SIP, 1, ip.ssip, m.ideleg.ss)
          api.read(vip.ssip && vie.ssie, CsrCondFilter(CSR.SIP, !m.ideleg.ss), 9)
          api.writeWhen(vip.ssip, vie.ssie, CsrCondFilter(CSR.SIP, !m.ideleg.ss), 9)
          mapVie(CSR.SIP, 1, vip.ssip, m.ideleg.ss, vie.ssie)
        }

        api.readWrite(CSR.MIE, 9 -> ie.seie, 5 -> ie.stie, 1 -> ie.ssie)
        api.read(ip.seipOr, CSR.MIP, 9)
        api.write(ip.seipSoft, CSR.MIP, 9)
        api.read(ip.stipOr, CSR.MIP, 5)
        api.writeWhen(ip.stipSoft, !sstc.envcfg.enable, CSR.MIP, 5)
        api.read(ip.stipOr && m.ideleg.st, CSR.SIP, 5)
        api.readWrite(ip.ssip, CSR.MIP, 1)
        api.readToWrite(ip.seipSoft, CSR.MIP, 9) //Avoid an external interrupt value to propagate to the soft external interrupt register.

        spec.addInterrupt(ip.ssip && ie.ssie, id = 1, privilege = PrivilegeMode.S, delegators = List(Delegator(m.ideleg.ss, PrivilegeMode.M)))
        spec.addInterrupt(ip.stipOr && ie.stie, id = 5, privilege = PrivilegeMode.S, delegators = List(Delegator(m.ideleg.st, PrivilegeMode.M)))
        spec.addInterrupt(ip.seipOr && ie.seie, id = 9, privilege = PrivilegeMode.S, delegators = List(Delegator(m.ideleg.se, PrivilegeMode.M)))
        if (p.withInterrutpFilter) {
          spec.addInterrupt(!m.ideleg.se && vip.seip && vie.seie && ieShadow.seie, id = 9, privilege = PrivilegeMode.S, delegators = List(Delegator(True, PrivilegeMode.M)))
          spec.addInterrupt(!m.ideleg.ss && vip.ssip && vie.ssie && ieShadow.ssie, id = 1, privilege = PrivilegeMode.S, delegators = List(Delegator(True, PrivilegeMode.M)))
        }

        val topi = new Area {
          val interrupt = Global.CODE().assignDontCare()
          val priority = Mux(interrupt === B(0), B(0), B(1))
          api.read(CSR.STOPI, 0 -> priority, 16 -> interrupt)
        }
      }

      val vs = p.withHypervisor generate new Area {
        val status = new api.Csr(CSR.VSSTATUS) {
          val sie, spie = RegInit(False)
          val spp = RegInit(U"0")
          val fs = withFs generate RegInit(U(p.mstatusFsInit, 2 bits))
          val xs = p.withXs generate RegInit(U(p.mstatusFsInit, 2 bits))
          val sd = False

          if (RVF) {
            fpuEnable(hartId) setWhen (withGuestPrivilege && fs =/= 0)
            when(withGuestPrivilege && host.list[FpuDirtyService].map(_.gotDirty()).orR){
              fs := 3
            }
          }

          if (withFs) sd setWhen (fs === 3)
          if (p.withXs) sd setWhen (xs === 3)

          readWrite(8 -> spp, 5 -> spie, 1 -> sie)
          read(XLEN - 1 -> sd)
          if (withFs) readWrite(13 -> fs)
          if (p.withXs) readWrite(15 -> xs)
          if (XLEN.get == 64) read(32 -> U"10")
          cap.trapNextOnWrite += CsrListFilter(List(CSR.VSSTATUS))
        }
        api.remapWhen(CSR.SSTATUS, CSR.VSSTATUS, withGuestPrivilege)

        val cause = new api.Csr(CSR.VSCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)
          readWrite(XLEN - 1 -> interrupt, 0 -> code)
        }
        api.remapWhen(CSR.SCAUSE, CSR.VSCAUSE, withGuestPrivilege)

        def mapVSie(guestCsr: Int, bitId: Int, reg: Bool, hypervisorDeleg: Bool, sWrite: Boolean = true): Unit = {
          api.read(reg && hypervisorDeleg, guestCsr, bitId)
          if (sWrite) api.writeWhen(reg, hypervisorDeleg, guestCsr, bitId)
        }

        mapVSie(CSR.VSIE, 9, h.ie.vseie, h.ideleg.vse)
        mapVSie(CSR.VSIE, 5, h.ie.vstie, h.ideleg.vst)
        mapVSie(CSR.VSIE, 1, h.ie.vssie, h.ideleg.vss)
        api.remapWhen(CSR.SIE, CSR.VSIE, withGuestPrivilege)

        mapVSie(CSR.VSIP, 9, h.ip.vseip, h.ideleg.vse)
        mapVSie(CSR.VSIP, 5, h.ip.vstip, h.ideleg.vst)
        mapVSie(CSR.VSIP, 1, h.ip.vssip, h.ideleg.vss)
        api.remapWhen(CSR.SIP, CSR.VSIP, withGuestPrivilege)

        spec.addInterrupt(h.ie.vseie && h.ip.vseip && h.ideleg.vse, id = 9, privilege = PrivilegeMode.VS, delegators = List(Delegator(True, PrivilegeMode.M), Delegator(True, PrivilegeMode.S)))
        spec.addInterrupt(h.ie.vstie && h.ip.vstip && h.ideleg.vst, id = 5, privilege = PrivilegeMode.VS, delegators = List(Delegator(True, PrivilegeMode.M), Delegator(True, PrivilegeMode.S)))
        spec.addInterrupt(h.ie.vssie && h.ip.vssip && h.ideleg.vss, id = 1, privilege = PrivilegeMode.VS, delegators = List(Delegator(True, PrivilegeMode.M), Delegator(True, PrivilegeMode.S)))

        val topi = new Area {
          val interrupt = Global.CODE().assignDontCare()
          val priority = Mux(interrupt === B(0), B(0), B(1))
          api.read(CSR.VSTOPI, 0 -> priority, 16 -> interrupt)
          api.remapWhen(CSR.STOPI, CSR.VSTOPI, withGuestPrivilege)
        }

        val tval = crs.readWriteRam(CSR.VSTVAL)
        api.remapWhen(CSR.STVAL, CSR.VSTVAL, withGuestPrivilege)

        val epc = crs.readWriteRam(CSR.VSEPC)
        api.remapWhen(CSR.SEPC, CSR.VSEPC, withGuestPrivilege)

        val tvec = crs.readWriteRam(CSR.VSTVEC)
        api.remapWhen(CSR.STVEC, CSR.VSTVEC, withGuestPrivilege)
      }

      val time = p.withRdTime generate new Area {
        val accessable =  withMachinePrivilege || mcounteren.tm

        XLEN.get match {
          case 32 => {
            api.read(rdtime(31 downto 0), HostCsrFilter(CSR.UTIME))
            api.read(rdtime(63 downto 32), HostCsrFilter(CSR.UTIMEH))
            api.allowCsr(HostCsrFilter(CSR.UTIME), accessable)
            api.allowCsr(HostCsrFilter(CSR.UTIMEH), accessable)
          }
          case 64 => {
            api.read(rdtime, HostCsrFilter(CSR.UTIME))
            api.allowCsr(HostCsrFilter(CSR.UTIME), accessable)
          }
        }
      }

      val exception = p.withSupervisor generate new Area {
        for ((id, enable) <- m.edeleg.mapping) {
          var delegator = List(Delegator(enable, PrivilegeMode.M))

          if (p.withHypervisor && h.edeleg.mapping.contains(id)) delegator = delegator ++ List(Delegator(h.edeleg.mapping(id), PrivilegeMode.S))

          spec.exception += ExceptionSpec(id, delegator)
        }
      }

      def genImsicArea(ireg: Int, topei: Int, provider: (Int, Int) => CsrCondFilter) = new Area {
        val file = ImsicFile(hartIds(hartId), 1 until p.imsicInterrupts)
        val identity = file.identity
        val triggers = in(file.triggers)

        api.readWrite(file.threshold, provider(IndirectCSR.eithreshold, ireg))

        val sources = for (interrupt <- file.interrupts) yield new Area {
          val id = interrupt.id
          val offset = id / XLEN * (1 + (XLEN == 64).toInt)

          api.readWrite(interrupt.ie, provider(IndirectCSR.eie0 + offset, ireg), id % XLEN)
          api.readWrite(interrupt.ip, provider(IndirectCSR.eip0 + offset, ireg), id % XLEN)
        }

        api.read(topei, 0 -> identity, 16 -> identity)
        val claim = new Area {
          val toClaim = RegInit(U(0, file.idWidth bits))
          api.onRead(topei, false){
            toClaim := identity
          }
          api.onWrite(topei, true) {
            file.claim(toClaim)
          }
        }

        val eidelivery = RegInit(U(0x40000000, XLEN bits))
        api.readWrite(eidelivery, provider(IndirectCSR.eidelivery, ireg))

        def deliveryArbiter(aplicTarget: Bool): Bool = {
          eidelivery.mux(
            1 -> (identity > 0),
            0x40000000 -> aplicTarget,
            default -> False
          )
        }
      }

      def HostCsrFilter(id: Int): Any = p.withHypervisor.mux(HostCsrFilter(id, True), id)
      def HostCsrFilter(id: Int, cond: Bool) = CsrCondFilter(id, withHostPrivilege && cond)
      def GuestCsrFilter(id: Int, cond: Bool = True) = CsrCondFilter(id, withGuestPrivilege && cond)
    }

    val defaultTrap = new Area {
      val csrPrivilege = cap.bus.decode.privilege.asUInt
      val csrReadOnly = cap.bus.decode.address(10, 2 bits) === U"11"
      // todo
      val hartPrivilege = harts.reader(cap.bus.decode.hartId)(_.privilege)
      val adjustPrivilege = UInt(2 bits)

      p.withHypervisor generate {
        adjustPrivilege := Mux(hartPrivilege === S(PrivilegeMode.S), U"10", hartPrivilege(1 downto 0).asUInt)
      }

      !p.withHypervisor generate {
        adjustPrivilege := hartPrivilege(1 downto 0).asUInt
      }

      when(csrReadOnly && cap.bus.decode.write || csrPrivilege > adjustPrivilege) {
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
