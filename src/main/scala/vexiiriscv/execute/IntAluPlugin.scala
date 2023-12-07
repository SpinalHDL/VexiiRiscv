// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib.Flow
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode._
import vexiiriscv.riscv.{Riscv, Rvi}

object IntAluPlugin extends AreaObject {
  val AluBitwiseCtrlEnum = new SpinalEnum(binarySequential) {
    val XOR, OR, AND = newElement()
  }
  val AluCtrlEnum = new SpinalEnum(binarySequential) {
    val ADD_SUB, SLT_SLTU, BITWISE = newElement()
  }

  val ALU_BITWISE_CTRL = Payload(AluBitwiseCtrlEnum())
  val ALU_CTRL = Payload(AluCtrlEnum())
  val ALU_RESULT = Payload(Bits(Riscv.XLEN bits))
}

class IntAluPlugin(var implName: LaneLayer,
                   var aluAt : Int = 0,
                   var formatAt : Int = 0) extends ExecutionUnitElementSimple(implName)  {
  import IntAluPlugin._
  lazy val ifp = host.find[IntFormatPlugin](_.laneName == implName.el.laneName)
  setupRetain(ifp.elaborationLock)

  val logic = during build new Logic{
    import SrcKeys._

    val ace = AluCtrlEnum
    val abce = AluBitwiseCtrlEnum

    val formatBus = ifp.access(formatAt)
    implicit val _ = ifp -> formatBus

    add(Rvi.ADD ).srcs(Op.ADD   , SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.ADD_SUB )
    add(Rvi.SUB ).srcs(Op.SUB   , SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.ADD_SUB )
    add(Rvi.SLT ).srcs(Op.LESS  , SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.SLT_SLTU)
    add(Rvi.SLTU).srcs(Op.LESS_U, SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.SLT_SLTU)
    add(Rvi.XOR ).srcs(           SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.XOR )
    add(Rvi.OR  ).srcs(           SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.OR  )
    add(Rvi.AND ).srcs(           SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.AND )

    add(Rvi.ADDI ).srcs(Op.ADD   , SRC1.RF, SRC2.I).decode(ALU_CTRL -> ace.ADD_SUB )
    add(Rvi.SLTI ).srcs(Op.LESS  , SRC1.RF, SRC2.I).decode(ALU_CTRL -> ace.SLT_SLTU)
    add(Rvi.SLTIU).srcs(Op.LESS_U, SRC1.RF, SRC2.I).decode(ALU_CTRL -> ace.SLT_SLTU)
    add(Rvi.XORI ).srcs(           SRC1.RF, SRC2.I).decode(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.XOR )
    add(Rvi.ORI  ).srcs(           SRC1.RF, SRC2.I).decode(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.OR  )
    add(Rvi.ANDI ).srcs(           SRC1.RF, SRC2.I).decode(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.AND )

    add(Rvi.LUI  ).srcs(Op.SRC1  , SRC1.U)         .decode(ALU_CTRL -> ace.ADD_SUB)
    add(Rvi.AUIPC).srcs(Op.ADD, SRC1.U, SRC2.PC).decode(ALU_CTRL -> ace.ADD_SUB)

    if(Riscv.XLEN.get == 64){
      add(Rvi.ADDW ).srcs(Op.ADD   , SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.ADD_SUB)
      add(Rvi.SUBW ).srcs(Op.SUB   , SRC1.RF, SRC2.RF).decode(ALU_CTRL -> ace.ADD_SUB)
      add(Rvi.ADDIW).srcs(Op.ADD   , SRC1.RF, SRC2.I ).decode(ALU_CTRL -> ace.ADD_SUB)

      for(op <- List(Rvi.ADDW, Rvi.SUBW, Rvi.ADDIW)){
        ifp.signExtend(formatBus, implName(op), 32)
      }
    }

    eu.uopLock.release()
    srcp.elaborationLock.release()
    ifp.elaborationLock.release()

    val alu = new eu.Execute(aluAt) {
      val ss = SrcStageables

      val bitwise = ALU_BITWISE_CTRL.mux(
        AluBitwiseCtrlEnum.AND  -> (srcp.SRC1 & srcp.SRC2),
        AluBitwiseCtrlEnum.OR   -> (srcp.SRC1 | srcp.SRC2),
        AluBitwiseCtrlEnum.XOR  -> (srcp.SRC1 ^ srcp.SRC2)
      )

      val result = ALU_CTRL.mux(
        AluCtrlEnum.BITWISE  -> bitwise,
        AluCtrlEnum.SLT_SLTU -> S(U(ss.LESS, Riscv.XLEN bits)),
        AluCtrlEnum.ADD_SUB  -> this(ss.ADD_SUB)
      )

      ALU_RESULT := result.asBits
    }

    val format = new eu.Execute(formatAt) {
      formatBus.valid := SEL
      formatBus.payload := ALU_RESULT
    }
  }
}
