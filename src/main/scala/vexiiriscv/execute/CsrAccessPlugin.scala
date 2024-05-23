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
import vexiiriscv.riscv.{CSR, Const, IMM, IntRegFile, RD, RS1, Rvi}

import scala.collection.mutable.ArrayBuffer
import vexiiriscv.riscv.Riscv._
import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}

object CsrFsm{
  val CSR_VALUE = Payload(Bits(XLEN bits))
}

class CsrAccessPlugin(val layer : LaneLayer,
                      writeBackKey : Any,
                      integrated : Boolean = true,
                      injectAt : Int = 0) extends FiberPlugin with CsrService with CompletionService {
  override def getCompletions(): Seq[Flow[CompletionPayload]] = if(!integrated) Seq(logic.fsm.completion) else Nil

  val SEL = Payload(Bool())
  val CSR_IMM = Payload(Bool())
  val CSR_MASK = Payload(Bool())
  val CSR_CLEAR = Payload(Bool())
  val CSR_WRITE = Payload(Bool())
  val CSR_READ = Payload(Bool())
  val CSR_ADDRESS = Payload(UInt(12 bits))

  import CsrFsm._
  
  override val bus = during build CsrBus().setup()

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

    val wbWi = integrated generate iwb.access(injectAt)
    for(op <- List(Rvi.CSRRW, Rvi.CSRRS, Rvi.CSRRC, Rvi.CSRRWI, Rvi.CSRRSI, Rvi.CSRRCI).map(layer(_))){
      op.dontFlushFrom(injectAt)
      op.mayFlushUpTo(injectAt)
      if (!integrated) ??? //elp.setRdOutOfPip(op)
      if (integrated) iwb.addMicroOp(wbWi, op)
      //      dp.fenceYounger(op)
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

    val wbNi = !integrated generate irf.newWrite(withReady = true, sharingKey = writeBackKey)


    def filterToName(filter: Any) = filter match {
      case f: Int => f.toString
      case f: Nameable => f.getName()
    }

    val grouped = spec.groupByLinked(_.csrFilter)

    val fsm = new StateMachine{
      val IDLE = makeInstantEntry()
      val READ, WRITE, COMPLETION = new State()

      val rd = rfaKeys.get(RD)

      //TODO this is a bit fat
      val regs = new Area {
        def doReg[T <: Data](that : HardType[T]) : T = if(integrated) that() else Reg(that)
        val sels = grouped.map(e => e._1 -> Reg(Bool()).setName("REG_CSR_" + filterToName(e._1)))
        val read, write = Reg(Bool())
        val rs1 = doReg(CSR_VALUE)
        val aluInput, csrValue, onWriteBits = Reg(CSR_VALUE) //onWriteBits is only for whiteboxing
        val hartId = doReg(Global.HART_ID)
        val uopId = doReg(Decode.UOP_ID)
        val uop = doReg(Decode.UOP)
        val doImm, doMask, doClear = doReg(Bool())
        val rdPhys = doReg(rd.PHYS)
        val rdEnable = doReg(Bool())
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

        def connectRegs(): Unit = {
          regs.hartId := Global.HART_ID
          regs.uopId := Decode.UOP_ID
          regs.read := SEL && !trap && csrRead
          regs.write := SEL && !trap && csrWrite
          regs.rs1 := up(elp(IntRegFile, RS1))
          regs.uop := UOP
          regs.doImm := CSR_IMM
          regs.doMask := CSR_MASK
          regs.doClear := CSR_CLEAR
          regs.rdEnable := up(rd.ENABLE)
          regs.rdPhys := rd.PHYS
        }

        bus.decode.read := csrRead
        bus.decode.write := csrWrite
        bus.decode.hartId := Global.HART_ID
        bus.decode.address := csrAddress.asUInt

        val unfreeze = RegNext(False) init(False)
        val iLogic = integrated generate new Area{
          connectRegs()
          val freeze = isValid && SEL && !unfreeze
          elp.freezeWhen(freeze)
        }
        val niLogic = !integrated generate new Area{
          when(isActive(IDLE)) {
            connectRegs()
          }
        }

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
          (regs.sels.values, sels.values).zipped.foreach(_ := _)
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
        bus.read.address := U(regs.uop(Const.csrRange))

        bus.read.moving := !bus.read.halt //TODO || eu.getExecute(0).isFlushed

        val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area {
          setPartialName(filterToName(csrFilter))

          val onReads = elements.collect { case e: CsrOnRead => e }
          val onReadsAlways = onReads.filter(!_.onlyOnFire)
          val onReadsFire = onReads.filter(_.onlyOnFire)

          if (onReadsAlways.nonEmpty) when(onReadsDo && regs.sels(csrFilter)) {
            onReadsAlways.foreach(_.body())
          }
          if (onReadsFire.nonEmpty) when(onReadsFireDo && regs.sels(csrFilter)) {
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
          if (onReadToWrite.nonEmpty) when(onReadsDo && regs.sels(csrFilter)) {
            onReadToWrite.foreach(_.body())
          }
        }
        spec.foreach{
          case e : CsrIsReadingCsr => {
            e.value := regs.sels(e.csrFilter)
          }
          case _ =>
        }

        for((id, value) <- onReadingHartIdMap) value := bus.read.hartId === id

        READ.whenIsActive {
          onReadsDo := regs.read
          regs.aluInput := bus.read.toWriteBits
          regs.csrValue := csrValue
          when(!bus.read.halt) {
            onReadsFireDo := regs.read
            goto(WRITE)
          }
        }
      }


      val writeLogic = new Area {
        val imm = IMM(regs.uop)
        bus.write.moving := !bus.write.halt //TODO || eu.getExecute(0).isFlushed

        val alu = new Area {
          val mask = regs.doImm ? imm.z.resized | regs.rs1
          val masked = regs.doClear ? (regs.aluInput & ~mask) otherwise (regs.aluInput | mask)
          val result = regs.doMask ? masked otherwise mask
        }

        regs.onWriteBits := alu.result
        bus.write.bits := alu.result
        bus.write.address := U(regs.uop(Const.csrRange))

        val onWritesDo = False
        val onWritesFireDo = False

        bus.write.valid := onWritesDo

        WRITE.whenIsActive {
//          regs.flushPipeline setWhen (io.onWriteFlushPipeline)
          onWritesDo := regs.write
          when(!bus.write.halt) {
            onWritesFireDo := regs.write
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
            if (onWritesAlways.nonEmpty) when(onWritesDo && regs.sels(csrFilter)) {
              onWritesAlways.foreach(_.body())
            }
            if (onWritesFire.nonEmpty) when(onWritesFireDo && regs.sels(csrFilter)) {
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

      val completion = Flow(CompletionPayload()) //Only used when !integrated
      completion.valid := False
      completion.uopId := regs.uopId
      completion.hartId := regs.hartId
      completion.trap := inject(Global.TRAP)
      completion.commit := inject(Global.COMMIT)

      integrated match {
        case true => {
          wbWi.valid := inject(SEL)
          wbWi.payload := regs.csrValue
        }
        case false =>{
          wbNi.valid := False
          wbNi.data := regs.csrValue
          wbNi.uopId := regs.uopId
          wbNi.hartId := regs.hartId
          wbNi.address := regs.rdPhys
        }
      }

      COMPLETION.whenIsNext(inject.unfreeze := True)
      COMPLETION.whenIsActive {
        when(regs.rdEnable){
          if(!integrated) wbNi.valid := True
        }
        when(inject.isReady) {
          regs.fire := True
          completion.valid := True
          goto(IDLE)
        }
      }
    }
    buildBefore.release()
  }
}
