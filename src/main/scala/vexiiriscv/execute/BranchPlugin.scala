// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{IMM, Riscv, Rvi}
import vexiiriscv._
import decode.Decode._
import Global._
import vexiiriscv.fetch.Fetch
import vexiiriscv.schedule.SchedulePlugin

object BranchPlugin extends AreaObject {
  val BranchCtrlEnum = new SpinalEnum(binarySequential) {
    val B, JAL, JALR = newElement()
  }
  val BRANCH_CTRL =  Payload(BranchCtrlEnum())
}

class BranchPlugin(val euId : String,
                   var aluAt : Int = 0,
                   var jumpAt: Int = 1,
                   var wbAt: Int = 0) extends ExecutionUnitElementSimple(euId)  {
  import BranchPlugin._
  lazy val wbp = host.find[WriteBackPlugin](_.euId == euId)
  lazy val sp = host[SchedulePlugin]
  addRetain(wbp)
  addRetain(sp)

  val logic = during build new Logic{
    import SrcKeys._

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

    val pcPort = sp.newPcPort(42)
    val trapPort = sp.newTrapPort(42)
    val flushPort = sp.newFlushPort(42)

    eu.release()
    wbp.release()
    sp.release()

    val aluCtrl = eu.execute(aluAt)
    val alu = new aluCtrl.Area {
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

      val slices = Fetch.INSTRUCTION_SLICE_COUNT +^ 1
      val sliceShift = Fetch.SLICE_RANGE_LOW.get
      val PC_TRUE = insert(U(target_a + target_b).as(PC)) //TODO overflows ?
      val PC_FALSE = insert(PC + (slices << sliceShift))
    }

    val jumpCtrl = eu.execute(jumpAt)
    val jumpLogic = new jumpCtrl.Area {
      val doIt = isValid && SEL && alu.COND

      pcPort.valid := doIt
      pcPort.pc := alu.PC_TRUE

      flushPort.valid := doIt
    }

    val wbCtrl = eu.execute(wbAt)
    val wbLogic = new wbCtrl.Area{
      wb.valid := SEL
      wb.payload := alu.PC_FALSE.asBits
    }
  }
}
