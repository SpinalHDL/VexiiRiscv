package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import spinal.lib.pipeline.Stageable
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.decode.Decode.{INSTRUCTION_SLICE_COUNT, UOP, rfaKeys}
import vexiiriscv.misc.{TrapReason, TrapService}
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{CSR, Const, IMM, IntRegFile, RD, RS1, Riscv, Rvi}

import scala.collection.mutable.ArrayBuffer
import vexiiriscv.riscv.Riscv._
import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}

object CsrFsm{
  val CSR_VALUE = Payload(Bits(XLEN bits))
}

/**
 * Implements the RISC-V CSR read/write instructions, aswell as provide an API for other plugin to populate the CSR space.
 * In other words, this plugin do not define any CSR, but provide an API to define them (that API is defined in CsrService).
 *
 * To help with the FMax, CSR accesses are implemented by using a state-machine. Accesses are done over 4 cycles :
 * idle -> read -> write -> completion
 *
 * This maybe a bit overkilled, but as the CSR access isn't critical for the IPC, better to much cycles than not enough.
 */
class CsrAccessPlugin(val layer : LaneLayer,
                      writeBackKey : Any,
                      injectAt : Int = 0,
                      wbAt : Int = 1) extends FiberPlugin with CsrService {

  val SEL = Payload(Bool())
  val TO_RF = Payload(Bits(Riscv.XLEN bits))
  val CSR_IMM = Payload(Bool())
  val CSR_MASK = Payload(Bool())
  val CSR_CLEAR = Payload(Bool())
  val CSR_WRITE = Payload(Bool())
  val CSR_READ = Payload(Bool())
  val CSR_ADDRESS = Payload(UInt(12 bits))

  import CsrFsm._

  override val bus = during build CsrBus().setup()
  override def waitElaborationDone(): Unit = logic.get

  val logic = during setup new Area {
    val elp = host.find[ExecuteLanePlugin](_.laneName == layer.laneName)
    val irf = host.find[RegfileService](_.rfSpec == IntRegFile)
    val iwb = host.find[IntFormatPlugin](_.lane == layer.lane)
    val dp = host[DispatchPlugin]
    val ram = host.get[CsrRamService]
    val sp = host[ReschedulePlugin]
    val ts = host[TrapService]

    val ramPortRetainer = ram.map(_.portLock())
    val ioRetainer = retains(dp.elaborationLock, iwb.elaborationLock, elp.uopLock, sp.elaborationLock, ts.trapLock)
    val buildBefore = retains(elp.pipelineLock, irf.elaborationLock)
    awaitBuild()

    elp.setDecodingDefault(SEL, False)

    val add = new ExecuteUnitElementSimple.Api(layer, null, SEL).add(_)
    add(Rvi.CSRRW).decode(CSR_IMM -> False, CSR_MASK -> False)
    add(Rvi.CSRRS).decode(CSR_IMM -> False, CSR_MASK -> True, CSR_CLEAR -> False)
    add(Rvi.CSRRC).decode(CSR_IMM -> False, CSR_MASK -> True, CSR_CLEAR -> True)
    add(Rvi.CSRRWI).decode(CSR_IMM -> True, CSR_MASK -> False)
    add(Rvi.CSRRSI).decode(CSR_IMM -> True, CSR_MASK -> True, CSR_CLEAR -> False)
    add(Rvi.CSRRCI).decode(CSR_IMM -> True, CSR_MASK -> True, CSR_CLEAR -> True)

    val wbWi = iwb.access(wbAt)
    for(op <- List(Rvi.CSRRW, Rvi.CSRRS, Rvi.CSRRC, Rvi.CSRRWI, Rvi.CSRRSI, Rvi.CSRRCI).map(layer(_))){
      op.dontFlushFrom(injectAt)
      op.mayFlushUpTo(injectAt)
      iwb.addMicroOp(wbWi, op)
      dp.fenceOlder(op.uop)
    }

    for (op <- List(Rvi.CSRRW, Rvi.CSRRS, Rvi.CSRRC).map(layer(_))) {
      op.addRsSpec(RS1, injectAt)
    }

    val age = layer.lane.getExecuteAge(injectAt)
    val flushPort = sp.newFlushPort(age, laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
    val trapPort = ts.newTrap(age, Execute.LANE_AGE_WIDTH)

    ioRetainer.release()

    csrLock.await()

    val trapNextOnWriteFilter = CsrListFilter(trapNextOnWrite.flatMap{
      case e : CsrListFilter => e.mapping
    }.toList)

    onDecode(trapNextOnWriteFilter) {
      when(bus.decode.write) {
        bus.decode.doTrap(TrapReason.NEXT)
      }
    }

    ramPortRetainer.foreach(_.release())

    var unamedId = -1
    def filterToName(filter: Any) = filter match {
      case f: Int => f.toString
      case f: Nameable => f.isNamed.mux(f.getName(), s"UNAMED_${unamedId += 1; unamedId}")//assert(f.isNamed, "CSR addresses need to be named. If you do csrApi.access(y, CsrCondFilter(..)), move the CsrCondFilter in a val : val myFilter = CsrCondFilter(..))"); f.getName()
    }

    val grouped = spec.groupByLinked(_.csrFilter)

    val fsm = new StateMachine{
      val IDLE = makeInstantEntry()
      val READ, WRITE, COMPLETION = new State()

      val rd = rfaKeys.get(RD)

      val interface = new Area {
        val sels = grouped.map(e => e._1 -> Reg(Bool()).setName("REG_CSR_" + filterToName(e._1)))
        val read, write = Reg(Bool())
        val rs1 = CSR_VALUE()
        val aluInput, csrValue, onWriteBits = Reg(CSR_VALUE) //onWriteBits is only for whiteboxing
        val hartId = Global.HART_ID()
        val uopId = Decode.UOP_ID()
        val uop = Decode.UOP()
        val doImm, doMask, doClear = Bool()
        val rdPhys = rd.PHYS()
        val rdEnable = Bool()
        val fire = False
      }

      val inject = new elp.Execute(injectAt){
        assert(!(up(LANE_SEL) && SEL && isCancel), "CsrAccessPlugin saw forbidden select && cancel request")
        val imm = IMM(UOP)
        val csrAddress = UOP(Const.csrRange)
        val immZero = imm.z === 0
        val srcZero = CSR_IMM ? immZero otherwise UOP(Const.rs1Range) === 0
        val csrWrite = !(CSR_MASK && srcZero)
        val csrRead = !(!CSR_MASK && !up(rd.ENABLE))
        val sels = grouped.map(e => e._1 -> Bool().setName("COMB_CSR_" + filterToName(e._1)))
        for ((filter, sel) <- sels) sel := (filter match {
          case filter: Int => csrAddress === filter
          case filter: CsrListFilter => filter.mapping.map(csrAddress === _).orR //TODO
          case filter: CsrCondFilter => csrAddress === filter.csrId && filter.cond
        })
        val implemented = sels.values.orR

        val onDecodeDo = isValid && SEL && isActive(IDLE)
        val priorities = spec.collect { case e: CsrOnDecode => e.priority }.distinct.sorted
        for (priority <- priorities) {
          for ((csrFilter, elements) <- grouped) {
            val onDecodes = elements.collect { case e: CsrOnDecode if e.priority == priority => e }
            if (onDecodes.nonEmpty) when(onDecodeDo && sels(csrFilter)) {
              onDecodes.foreach(_.body())
            }
          }
        }

        val trap = !implemented || bus.decode.exception

        bus.decode.read := csrRead
        bus.decode.write := csrWrite
        bus.decode.hartId := Global.HART_ID
        bus.decode.address := csrAddress.asUInt

        val unfreeze = RegNext(False) init(False)
        interface.hartId := Global.HART_ID
        interface.uopId := Decode.UOP_ID
        interface.read := SEL && !trap && csrRead
        interface.write := SEL && !trap && csrWrite
        interface.rs1 := up(elp(IntRegFile, RS1))
        interface.uop := UOP
        interface.doImm := CSR_IMM
        interface.doMask := CSR_MASK
        interface.doClear := CSR_CLEAR
        interface.rdEnable := up(rd.ENABLE)
        interface.rdPhys := rd.PHYS

        val freeze = isValid && SEL && !unfreeze
        elp.freezeWhen(freeze)

        flushPort.valid := False
        flushPort.hartId := Global.HART_ID
        flushPort.uopId := Decode.UOP_ID
        flushPort.laneAge := Execute.LANE_AGE
        flushPort.self := False

        trapPort.valid := False
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
        trapPort.tval := UOP.resized
        trapPort.arg := 0
        trapPort.laneAge := Execute.LANE_AGE

        val flushReg = RegInit(False) setWhen(flushPort.valid) clearWhen(!elp.isFreezed())
        when(flushReg) {
          flushPort.valid := True
          bypass(Global.TRAP) := True
        }

        // For timing reasons, avoinding trap to get long combinatorial path
        val sampled = RegNext(elp.isFreezed()) init(False)
        val trapReg = RegNext(trap)
        val busTrapReg = RegNext(bus.decode.trap)
        val busTrapCodeReg = RegNext(bus.decode.trapCode)

        IDLE whenIsActive {
          (interface.sels.values, sels.values).zipped.foreach(_ := _)
          when(onDecodeDo) {
            when(!trap && !bus.decode.trap) {
              goto(READ)
            }
            when(sampled) {
              when(trapReg) {
                bypass(Global.TRAP) := True
                bypass(Global.COMMIT) := False
                flushPort.valid := True
                trapPort.valid := True
                unfreeze := elp.isFreezed()
              } otherwise {
                goto(READ)
                when(busTrapReg) {
                  bypass(Global.TRAP) := True
                  flushPort.valid := True
                  trapPort.valid := True
                  trapPort.exception := False
                  trapPort.code := busTrapCodeReg
                }
              }
            }
          }
        }
      }

      val readLogic = new Area {
        val onReadsDo = False
        val onReadsFireDo = False
        bus.read.valid := onReadsDo
        bus.read.address := U(interface.uop(Const.csrRange))

        bus.read.moving := !bus.read.halt //TODO || eu.getExecute(0).isFlushed

        val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area {
          setPartialName(filterToName(csrFilter))

          val onReads = elements.collect { case e: CsrOnRead => e }
          val onReadsAlways = onReads.filter(!_.onlyOnFire)
          val onReadsFire = onReads.filter(_.onlyOnFire)

          if (onReadsAlways.nonEmpty) when(onReadsDo && interface.sels(csrFilter)) {
            onReadsAlways.foreach(_.body())
          }
          if (onReadsFire.nonEmpty) when(onReadsFireDo && interface.sels(csrFilter)) {
            onReadsFire.foreach(_.body())
          }
        }

        val csrValue = CSR_VALUE()
        if (reads.nonEmpty) {
          val shifteds = reads.map(e => e.bitOffset match {
            case 0 => e.value
            case _ => e.value << e.bitOffset
          })
          val resized = shifteds.map(e => widthOf(e) match {
            case w if w == XLEN.get => e
            case w if w <  XLEN.get => e.resize(XLEN)
          })
          csrValue := resized.reduceBalancedTree(_ | _)
        } else {
          csrValue := 0
        }

        bus.read.data := csrValue
        bus.read.toWriteBits := csrValue
        for ((csrFilter, elements) <- grouped) {
          val onReadToWrite = elements.collect { case e: CsrOnReadToWrite => e }
          if (onReadToWrite.nonEmpty) when(onReadsDo && interface.sels(csrFilter)) {
            onReadToWrite.foreach(_.body())
          }
        }
        spec.foreach{
          case e : CsrIsReadingCsr => {
            e.value := interface.sels(e.csrFilter)
          }
          case _ =>
        }

        for((id, value) <- onReadingHartIdMap) value := bus.read.hartId === id

        READ.whenIsActive {
          onReadsDo := interface.read
          interface.aluInput := bus.read.toWriteBits
          interface.csrValue := csrValue
          when(!bus.read.halt) {
            onReadsFireDo := interface.read
            goto(WRITE)
          }
        }
      }


      val writeLogic = new Area {
        val imm = IMM(interface.uop)
        bus.write.moving := !bus.write.halt //TODO || eu.getExecute(0).isFlushed

        val alu = new Area {
          val mask = interface.doImm ? imm.z.resized | interface.rs1
          val masked = interface.doClear ? (interface.aluInput & ~mask) otherwise (interface.aluInput | mask)
          val result = interface.doMask ? masked otherwise mask
        }

        interface.onWriteBits := alu.result
        bus.write.bits := alu.result
        bus.write.address := U(interface.uop(Const.csrRange))

        val onWritesDo = False
        val onWritesFireDo = False

        bus.write.valid := onWritesDo

        WRITE.whenIsActive {
          onWritesDo := interface.write
          when(!bus.write.halt) {
            onWritesFireDo := interface.write
            goto(COMPLETION)
          }
        }


        for ((id, value) <- onWritingHartIdMap) value := bus.write.hartId === id

        val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area {
          setPartialName(filterToName(csrFilter))

          val cancels = elements.collect { case e: CsrWriteCancel => e }.toList
          val onWrites = elements.collect { case e: CsrOnWrite => e }
          val onWritesAlways = onWrites.filter(!_.onlyOnFire)
          val onWritesFire = onWrites.filter(_.onlyOnFire)

          def doIt() {
            if (onWritesAlways.nonEmpty) when(onWritesDo && interface.sels(csrFilter)) {
              onWritesAlways.foreach(_.body())
            }
            if (onWritesFire.nonEmpty) when(onWritesFireDo && interface.sels(csrFilter)) {
              onWritesFire.foreach(_.body())
            }
          }

          cancels match {
            case Nil => doIt()
            case l => when(cancels.map(_.cond).orR === False) {
              doIt()
            }
          }
        }
      }

      wbWi.valid := elp.execute(wbAt)(SEL)
      wbWi.payload := interface.csrValue

      COMPLETION.whenIsNext(inject.unfreeze := True)
      COMPLETION.whenIsActive {
        when(inject.isReady) {
          interface.fire := True
          goto(IDLE)
        }
      }
    }
    buildBefore.release()
  }
}
