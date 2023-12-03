// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{Const, IMM, RD, Riscv, Rvi}
import vexiiriscv._
import decode.Decode._
import Global._
import spinal.lib.{Flow, KeepAttribute}
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.{Fetch, PcPlugin}
import vexiiriscv.prediction.Prediction.BRANCH_HISTORY_WIDTH
import vexiiriscv.prediction.{FetchWordPrediction, HistoryPlugin, HistoryUser, LearnCmd, Prediction}
import vexiiriscv.schedule.ReschedulePlugin

object BranchPlugin extends AreaObject {
  val BranchCtrlEnum = new SpinalEnum(binarySequential) {
    val B, JAL, JALR = newElement()
  }
  val BRANCH_CTRL =  Payload(BranchCtrlEnum())
}

class BranchPlugin(val laneName : String,
                   var aluAt : Int = 0,
                   var jumpAt: Int = 1,
                   var wbAt: Int = 0) extends ExecutionUnitElementSimple(laneName)  {
  import BranchPlugin._
  lazy val wbp = host.find[WriteBackPlugin](_.laneName == laneName)
  lazy val sp = host[ReschedulePlugin]
  lazy val pcp = host[PcPlugin]
  lazy val hp = host[HistoryPlugin]
  setupRetain(wbp.elaborationLock)
  setupRetain(sp.elaborationLock)
  setupRetain(pcp.elaborationLock)
  setupRetain(hp.elaborationLock)

  val logic = during build new Logic{
    import SrcKeys._

    BRANCH_HISTORY_WIDTH.set((0 +: host.list[HistoryUser].map(_.historyWidthUsed)).max)

    val wb = wbp.createPort(wbAt)
    wbp.addMicroOp(wb, Rvi.JAL, Rvi.JALR)

    add(Rvi.JAL ).decode(BRANCH_CTRL -> BranchCtrlEnum.JAL )
    add(Rvi.JALR).decode(BRANCH_CTRL -> BranchCtrlEnum.JALR).srcs(SRC1.RF)
    add(Rvi.BEQ ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF)
    add(Rvi.BNE ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF)
    add(Rvi.BLT ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS  )
    add(Rvi.BGE ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS  )
    add(Rvi.BLTU).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS_U)
    add(Rvi.BGEU).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS_U)

    val jList = List(Rvi.JAL, Rvi.JALR)
    val bList = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU)

    eu.setCompletion(Math.max(jumpAt, wbAt), jList)
    eu.setCompletion(jumpAt, bList)
    eu.mayFlushUpTo(jumpAt, jList ++ bList)

    val age = eu.getExecuteAge(jumpAt)
    val pcPort = pcp.createJumpInterface(age, laneAgeWidth = Execute.LANE_AGE_WIDTH, aggregationPriority = 0)
    val historyPort = hp.createPort(age)
    val flushPort = sp.newFlushPort(eu.getExecuteAge(jumpAt), laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
    //    val trapPort = if XXX sp.newTrapPort(age)

    eu.uopLock.release()
    wbp.elaborationLock.release()
    sp.elaborationLock.release()
    srcp.elaborationLock.release()
    pcp.elaborationLock.release()
    hp.elaborationLock.release()

    // Without prediction, the plugin can assume that there is no correction to do if no branch is needed
    // leading to a simpler design.
    val withBtb = host.get[FetchWordPrediction].nonEmpty

    val alu = new eu.Execute(aluAt) {
      val ss = SrcStageables
      val EQ = insert(ss.SRC1 === ss.SRC2)

      val COND = insert(BRANCH_CTRL.mux(
        BranchCtrlEnum.JALR -> True,
        BranchCtrlEnum.JAL -> True,
        BranchCtrlEnum.B -> UOP(14 downto 12).mux[Bool](
          B"000" ->  EQ,
          B"001" -> !EQ,
          M"1-1" -> !ss.LESS,
          default -> ss.LESS
        )
      ))

      val imm = IMM(UOP)
      val target_a = BRANCH_CTRL.mux(
        default -> S(PC),
        BranchCtrlEnum.JALR -> ss.SRC1.resize(PC_WIDTH)
      )

      val target_b = BRANCH_CTRL.mux(
        default -> imm.b_sext,
        BranchCtrlEnum.JAL -> imm.j_sext,
        BranchCtrlEnum.JALR -> imm.i_sext
      )

      val slices = Decode.INSTRUCTION_SLICE_COUNT +^ 1
      val sliceShift = Fetch.SLICE_RANGE_LOW.get
      val PC_TRUE = insert(U(target_a + target_b).as(PC)) //TODO overflows ?
      val PC_FALSE = insert(PC + (slices << sliceShift))

      // Without those keepattribute, Vivado will transform the logic in a way which will serialize the 32 bits of the COND comparator,
      // with the 32 bits of the TRUE/FALSE adders, ending up in a quite long combinatorial path (21 lut XD)
      KeepAttribute(this(PC_TRUE ))
      KeepAttribute(this(PC_FALSE))

      val btb = withBtb generate new Area {
        val BAD_TARGET = insert(Prediction.ALIGNED_JUMPED_PC =/= PC_TRUE)
        val REAL_TARGET = insert(COND.mux[UInt](PC_TRUE, PC_FALSE))
      }
    }

    val jumpLogic = new eu.Execute(jumpAt) {
      val wrongCond = withBtb.mux[Bool](Prediction.ALIGNED_JUMPED =/= alu.COND     , alu.COND )
      val needFix   = withBtb.mux[Bool](wrongCond || alu.COND && alu.btb.BAD_TARGET, wrongCond)
      val doIt = isValid && SEL && needFix
      val pcTarget = withBtb.mux[UInt](alu.btb.REAL_TARGET, alu.PC_TRUE)

      val history = new Area{
        assert(Global.HART_COUNT.get == 1)
        assert(host.list[BranchPlugin].size == 1, "Assume the plugin is the only point on which branches are solved, so we can build the history here")
        val state = Reg(Prediction.BRANCH_HISTORY) init(0)
        def fetched = state //Assume it for now, but not always right when fetching multiple instruction per cycles
        val shifted = (state ## alu.COND).dropHigh(1)
        val next = (BRANCH_CTRL === BranchCtrlEnum.B).mux(shifted, state)
        when(down.isFiring && SEL && BRANCH_CTRL === BranchCtrlEnum.B){
          state := shifted
        }
      }


      pcPort.valid := doIt
      pcPort.pc := pcTarget
      pcPort.laneAge := Execute.LANE_AGE

      historyPort.valid := doIt
      historyPort.history := history.next

      flushPort.valid := doIt
      flushPort.hartId := Global.HART_ID
      flushPort.uopId :=  Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False

      val IS_JAL = insert(BRANCH_CTRL === BranchCtrlEnum.JAL)
      val IS_JALR = insert(BRANCH_CTRL === BranchCtrlEnum.JALR)
      val rdLink  = List[Bits](1,5).map(UOP(Const.rdRange) === _).orR
      val rs1Link = List[Bits](1,5).map(UOP(Const.rs1Range) === _).orR
      val rdEquRs1 = UOP(Const.rdRange) === UOP(Const.rs1Range)

      val learn = Flow(LearnCmd())
      learn.valid := up.isFiring && SEL
      learn.taken := alu.COND
      learn.pcTarget := alu.PC_TRUE
      learn.pcOnLastSlice := PC; assert(!Riscv.RVC) //TODO PC + (Fetch.INSTRUCTION_SLICE_COUNT << sliceShift)
      learn.isBranch := BRANCH_CTRL === BranchCtrlEnum.B
      learn.isPush := (IS_JAL || IS_JALR) && rdLink
      learn.isPop := IS_JALR && (!rdLink && rs1Link || rdLink && rs1Link && !rdEquRs1)
      learn.wasWrong := needFix
      learn.history := history.fetched
      learn.uopId := Decode.UOP_ID
      learn.hartId := Global.HART_ID
    }

    val wbLogic = new eu.Execute(wbAt){
      wb.valid := SEL && Decode.rfaKeys.get(RD).ENABLE
      wb.payload := alu.PC_FALSE.asBits
    }
  }
}
