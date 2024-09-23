// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{CSR, Const, IMM, IntRegFile, RD, Riscv, Rvi}
import vexiiriscv._
import decode.Decode._
import Global._
import spinal.core.fiber.Handle
import spinal.lib.{Flow, KeepAttribute}
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.{Fetch, PcPlugin}
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService}
import vexiiriscv.misc.{PerformanceCounterService, TrapService}
import vexiiriscv.prediction.Prediction.BRANCH_HISTORY_WIDTH
import vexiiriscv.prediction.{FetchWordPrediction, HistoryPlugin, HistoryUser, LearnCmd, LearnService, LearnSource, Prediction}
import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}

import scala.collection.mutable

object BranchPlugin extends AreaObject {
  val BranchCtrlEnum = new SpinalEnum(binarySequential) {
    val B, JAL, JALR = newElement()
  }
  val BRANCH_CTRL =  Payload(BranchCtrlEnum())
}


class PcCalc(bp: BranchPlugin, at: Int) extends Area {
  import bp._
  import BranchPlugin._
  val ctrl = bp.layer.lane.execute(at)
  import ctrl._

  val srcp = host.find[SrcPlugin](_.layer == layer)
  val imm = IMM(UOP)
  val target_a = BRANCH_CTRL.mux(
    default -> S(PC),
    BranchCtrlEnum.JALR -> srcp.SRC1.resize(PC_WIDTH)
  )

  val target_b = BRANCH_CTRL.mux(
    default -> imm.b_sext,
    BranchCtrlEnum.JAL -> imm.j_sext,
    BranchCtrlEnum.JALR -> imm.i_sext
  )

  val slices = Decode.INSTRUCTION_SLICE_COUNT +^ 1
  val sliceShift = Fetch.SLICE_RANGE_LOW.get
  val PC_TRUE = insert(U(target_a + target_b).resize(PC_WIDTH));
  PC_TRUE(0) := False //PC RESIZED
  val PC_FALSE = insert(PC + (slices << sliceShift))
  val PC_LAST_SLICE = insert(PC + (Decode.INSTRUCTION_SLICE_COUNT << sliceShift))


  // Without those keepattribute, Vivado will transform the logic in a way which will serialize the 32 bits of the COND comparator,
  // with the 32 bits of the TRUE/FALSE adders, ending up in a quite long combinatorial path (21 lut XD)
  KeepAttribute(apply(PC_TRUE))
  KeepAttribute(apply(PC_FALSE))
  KeepAttribute(apply(PC_LAST_SLICE))
}

class BranchPlugin(val layer : LaneLayer,
                   var aluAt : Int = 0,
                   var jumpAt: Int = 1,
                   var wbAt: Int = 0,
                   var withJalr : Boolean = true) extends ExecutionUnitElementSimple(layer) with LearnSource {
  import BranchPlugin._

  def catchMissaligned = !Riscv.RVC
  override def getLearnPort(): Option[Stream[LearnCmd]] = logic.jumpLogic.learn

  def pluginsOnLane = host.list[BranchPlugin].filter(_.layer.lane == layer.lane)
  val pcCalc : Handle[PcCalc] = during build {
    // So, here we look for other branchplugins on the same lane to try reusing their calculation (better for fmax / LUT, cost some FF)
    val firstOfLane = pluginsOnLane.sortBy(_.jumpAt).head
    if(firstOfLane == BranchPlugin.this || withJalr){
      new PcCalc(this, Math.max(0, aluAt-(!withJalr).toInt))
    } else {
      firstOfLane.pcCalc.get
    }
  }

  val logic = during setup new Logic{
    val wbp = host.find[WriteBackPlugin](p => p.lane == layer.lane && p.rf == IntRegFile)
    val sp = host[ReschedulePlugin]
    val pcp = host[PcPlugin]
    val hp = host.get[HistoryPlugin]
    val ls = host[LearnService]
    val ts = host[TrapService]
    val pcs = host.get[PerformanceCounterService]
    val ioRetainer = retains(wbp.elaborationLock, sp.elaborationLock, pcp.elaborationLock, ts.trapLock)
    hp.foreach(ioRetainer += _.elaborationLock)

    val lastOfLane = pluginsOnLane.sortBy(_.jumpAt).last
    val isLastOfLane = BranchPlugin.this == lastOfLane
    val events = (isLastOfLane && pcs.nonEmpty).option(new Area{
      val branchMiss = pcs.get.createEventPort(PerformanceCounterService.BRANCH_MISS)
      val branchCount = pcs.get.createEventPort(PerformanceCounterService.BRANCH_COUNT)
    })
    awaitBuild()

    import SrcKeys._
    if(hp.nonEmpty) host[DispatchPlugin].hmKeys += Prediction.BRANCH_HISTORY

    add(Rvi.JAL ).decode(BRANCH_CTRL -> BranchCtrlEnum.JAL )
    if(withJalr) add(Rvi.JALR).decode(BRANCH_CTRL -> BranchCtrlEnum.JALR).srcs(SRC1.RF)
    add(Rvi.BEQ ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF)
    add(Rvi.BNE ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF)
    add(Rvi.BLT ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS  )
    add(Rvi.BGE ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS  )
    add(Rvi.BLTU).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS_U)
    add(Rvi.BGEU).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS_U)

    val jList = List(Rvi.JAL) ++ withJalr.option(Rvi.JALR)
    val bList = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU)

    val wb = wbp.createPort(wbAt)
    for(j <- jList; spec = layer(j)) {
      wbp.addMicroOp(wb, spec)
      spec.setCompletion(Math.max(jumpAt, wbAt))
      spec.mayFlushUpTo(jumpAt)
    }
    for (j <- bList; spec = layer(j)) {
      spec.setCompletion(jumpAt)
      spec.mayFlushUpTo(jumpAt)
    }

    val age = el.getExecuteAge(jumpAt)
    val pcPort = pcp.newJumpInterface(age, laneAgeWidth = Execute.LANE_AGE_WIDTH, aggregationPriority = 0)
    val historyPort = hp.map(_.newPort(age, Execute.LANE_AGE_WIDTH))
    val flushPort = sp.newFlushPort(age, laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
    val trapPort = catchMissaligned generate ts.newTrap(age, Execute.LANE_AGE_WIDTH)

    uopRetainer.release()
    ioRetainer.release()

    // Without prediction, the plugin can assume that there is no correction to do if no branch is needed
    // leading to a simpler design.
    val withBtb = host.get[FetchWordPrediction].nonEmpty

    val PC_TRUE = pcCalc.PC_TRUE
    val PC_FALSE = pcCalc.PC_FALSE
    val PC_LAST_SLICE = pcCalc.PC_LAST_SLICE

    val alu = new el.Execute(aluAt) {
      val ss = SrcStageables
      val EQ = insert(srcp.SRC1 === srcp.SRC2)

      val btb = withBtb generate new Area {
        val BAD_TARGET = insert(Prediction.ALIGNED_JUMPED_PC =/= PC_TRUE)
//        val REAL_TARGET = insert(COND.mux[UInt](PC_TRUE, PC_FALSE))
      }

      val expectedMsb = host[AddressTranslationService].getSignExtension(AddressTranslationPortUsage.FETCH, srcp.SRC1.asUInt)
      val MSB_FAILED = insert(BRANCH_CTRL === BranchCtrlEnum.JALR && srcp.SRC1.dropLow(MIXED_WIDTH).asBools.map(_ =/= expectedMsb).orR)
    }

    val jumpLogic = new el.Execute(jumpAt) {
      val COND = insert(BRANCH_CTRL.mux(
        BranchCtrlEnum.JALR -> True,
        BranchCtrlEnum.JAL -> True,
        BranchCtrlEnum.B -> UOP(14 downto 12).mux[Bool](
          B"000" ->  alu.EQ,
          B"001" -> !alu.EQ,
          M"1-1" -> !srcp.LESS,
          default -> srcp.LESS
        )
      ))

      val btb = withBtb generate new Area {
        val REAL_TARGET = insert(COND.mux[UInt](PC_TRUE, PC_FALSE))
      }


      val wrongCond = withBtb.mux[Bool](Prediction.ALIGNED_JUMPED =/= COND     , COND )
      val needFix   = withBtb.mux[Bool](wrongCond || COND && alu.btb.BAD_TARGET, wrongCond) || alu.MSB_FAILED
      val doIt = isValid && SEL && needFix
      val pcTarget = withBtb.mux[UInt](btb.REAL_TARGET, PC_TRUE)


      val history = historyPort.nonEmpty generate new Area{
        val fetched, next = Prediction.BRANCH_HISTORY()
        val withSelfHistory = false // && Global.HART_COUNT.get == 1 && Fetch.SLICE_COUNT.get == 1 && host.list[BranchPlugin].size == 1 && host.get[FetchWordPrediction].get.useAccurateHistory

        val fromSelf = withSelfHistory generate new Area {
          val state = Reg(Prediction.BRANCH_HISTORY) init (0)
          val shifted = (state ## COND).dropHigh(1)
          when(down.isFiring && SEL && BRANCH_CTRL === BranchCtrlEnum.B) {
            state := shifted
          }

          fetched := state
          next := (BRANCH_CTRL === BranchCtrlEnum.B).mux(shifted, state)
        }

        val fromFetch = !withSelfHistory generate new Area {
          val slice = PC(Fetch.SLICE_RANGE.get)
          var shifter = CombInit(apply(Prediction.BRANCH_HISTORY))
          for (sliceId <- 0 until Fetch.SLICE_COUNT - 1) {
            when(slice < sliceId && Prediction.ALIGNED_SLICES_BRANCH(sliceId)) {
              shifter \= (shifter ## Prediction.ALIGNED_SLICES_TAKEN(sliceId)).dropHigh(1)
            }
          }
          when(BRANCH_CTRL === BranchCtrlEnum.B) {
            shifter \= (shifter ## COND).dropHigh(1)
          }
          next := shifter
          fetched := Prediction.BRANCH_HISTORY
        }
      }

      pcPort.valid := doIt
      pcPort.fault := alu.MSB_FAILED
      pcPort.pc := pcTarget
      pcPort.laneAge := Execute.LANE_AGE

      historyPort.foreach{ port =>
        port.valid := doIt
        port.history := history.next
        port.age := Execute.LANE_AGE
      }

      flushPort.valid := doIt
      flushPort.hartId := Global.HART_ID
      flushPort.uopId :=  Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False

      val MISSALIGNED = insert(PC_TRUE(0, Fetch.SLICE_RANGE_LOW bits) =/= 0 && COND)
      if (catchMissaligned) { //Non RVC can trap on missaligned branches
        trapPort.valid := False
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.FETCH_MISSALIGNED
        trapPort.tval := B(PC_TRUE)
        trapPort.arg := 0
        trapPort.laneAge := Execute.LANE_AGE

        when(doIt && MISSALIGNED){
          trapPort.valid := True
          bypass(TRAP) := True
          bypass(COMMIT) := False
        }
      }

      val IS_JAL = insert(BRANCH_CTRL === BranchCtrlEnum.JAL)
      val IS_JALR = insert(BRANCH_CTRL === BranchCtrlEnum.JALR)
      val rdLink  = List[Bits](1,5).map(UOP(Const.rdRange) === _).orR
      val rs1Link = List[Bits](1,5).map(UOP(Const.rs1Range) === _).orR
      val rdEquRs1 = UOP(Const.rdRange) === UOP(Const.rs1Range)

      ls.learnLock.await()

      val learn = isLastOfLane.option(Stream(LearnCmd(ls.learnCtxElements.toSeq)))
      learn.foreach { learn =>
        learn.valid := isValid && isReady && !isCancel && pluginsOnLane.map(p => apply(p.SEL)).orR
        learn.taken := COND
        learn.pcTarget := PC_TRUE
        learn.pcOnLastSlice := PC_LAST_SLICE
        learn.isBranch := BRANCH_CTRL === BranchCtrlEnum.B
        learn.isPush := (IS_JAL || IS_JALR) && rdLink
        learn.isPop := IS_JALR && (!rdLink && rs1Link || rdLink && rs1Link && !rdEquRs1)
        learn.wasWrong := needFix
        learn.badPredictedTarget := withBtb.mux(alu.btb.BAD_TARGET, False)
        if(historyPort.nonEmpty) learn.history := history.fetched
        learn.uopId := Decode.UOP_ID
        learn.hartId := Global.HART_ID
        for (e <- ls.learnCtxElements) {
          learn.ctx(e).assignFrom(apply(e))
        }
        events.foreach{e =>
          e.branchMiss := learn.valid && learn.isBranch && learn.wasWrong
          e.branchCount := learn.valid && learn.isBranch
        }
      }

    }

    val wbLogic = new el.Execute(wbAt){
      wb.valid := SEL
      wb.payload := Global.expendPc(PC_FALSE, Riscv.XLEN).asBits
    }
  }
}
