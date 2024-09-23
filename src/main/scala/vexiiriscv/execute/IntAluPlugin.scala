// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib.{Flow, KeepAttribute}
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode._
import vexiiriscv.execute.lsu.CmoService
import vexiiriscv.riscv.{Riscv, Rvi}

object IntAluPlugin extends AreaObject {
  val AluBitwiseCtrlEnum = new SpinalEnum(binarySequential) {
    val XOR, OR, AND, ZERO = newElement()
  }
}

class IntAluPlugin(var layer: LaneLayer,
                   var aluAt : Int = 0,
                   var formatAt : Int = 0) extends ExecutionUnitElementSimple(layer)  {
  import IntAluPlugin._

  val ALU_BITWISE_CTRL = Payload(AluBitwiseCtrlEnum())
  val ALU_ADD_SUB, ALU_SLTX = Payload(Bool())
  val ALU_RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic{
    awaitBuild()
    import SrcKeys._

    val abce = AluBitwiseCtrlEnum

    val wb = newWriteback(ifp, formatAt)
    val ORI = Rvi.ORI(host.get[CmoService] match {
      case Some(s) => s.withSoftwarePrefetch
      case None => false
    })

    add(Rvi.ADD ).srcs(Op.ADD   , SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> True , ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.ZERO )
    add(Rvi.SUB ).srcs(Op.SUB   , SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> True , ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.ZERO )
    add(Rvi.SLT ).srcs(Op.LESS  , SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> False, ALU_SLTX -> True , ALU_BITWISE_CTRL -> abce.ZERO )
    add(Rvi.SLTU).srcs(Op.LESS_U, SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> False, ALU_SLTX -> True , ALU_BITWISE_CTRL -> abce.ZERO )
    add(Rvi.XOR ).srcs(           SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> False, ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.XOR )
    add(Rvi.OR  ).srcs(           SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> False, ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.OR  )
    add(Rvi.AND ).srcs(           SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> False, ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.AND )

    add(Rvi.ADDI ).srcs(Op.ADD   , SRC1.RF, SRC2.I).decode(ALU_ADD_SUB -> True , ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.ZERO)
    add(Rvi.SLTI ).srcs(Op.LESS  , SRC1.RF, SRC2.I).decode(ALU_ADD_SUB -> False, ALU_SLTX -> True , ALU_BITWISE_CTRL -> abce.ZERO)
    add(Rvi.SLTIU).srcs(Op.LESS_U, SRC1.RF, SRC2.I).decode(ALU_ADD_SUB -> False, ALU_SLTX -> True , ALU_BITWISE_CTRL -> abce.ZERO)
    add(Rvi.XORI ).srcs(           SRC1.RF, SRC2.I).decode(ALU_ADD_SUB -> False, ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.XOR )
    add(    ORI  ).srcs(           SRC1.RF, SRC2.I).decode(ALU_ADD_SUB -> False, ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.OR  )
    add(Rvi.ANDI ).srcs(           SRC1.RF, SRC2.I).decode(ALU_ADD_SUB -> False, ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.AND )

    add(Rvi.LUI  ).srcs(Op.SRC1, SRC1.U         ).decode(ALU_ADD_SUB -> True, ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.ZERO)
    add(Rvi.AUIPC).srcs(Op.ADD , SRC1.U, SRC2.PC).decode(ALU_ADD_SUB -> True, ALU_SLTX -> False, ALU_BITWISE_CTRL -> abce.ZERO)

    if(Riscv.XLEN.get == 64){
      add(Rvi.ADDW ).srcs(Op.ADD   , SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> True, ALU_SLTX -> False,  ALU_BITWISE_CTRL -> abce.ZERO)
      add(Rvi.SUBW ).srcs(Op.SUB   , SRC1.RF, SRC2.RF).decode(ALU_ADD_SUB -> True, ALU_SLTX -> False,  ALU_BITWISE_CTRL -> abce.ZERO)
      add(Rvi.ADDIW).srcs(Op.ADD   , SRC1.RF, SRC2.I ).decode(ALU_ADD_SUB -> True, ALU_SLTX -> False,  ALU_BITWISE_CTRL -> abce.ZERO)

      for(op <- List(Rvi.ADDW, Rvi.SUBW, Rvi.ADDIW)){
        ifp.signExtend(wb, layer(op), 32)
      }
    }

    uopRetainer.release()

    val alu = new el.Execute(aluAt) {
      val ss = SrcStageables

      val bitwise = ALU_BITWISE_CTRL.mux(
        AluBitwiseCtrlEnum.AND  -> (srcp.SRC1 & srcp.SRC2),
        AluBitwiseCtrlEnum.OR   -> (srcp.SRC1 | srcp.SRC2),
        AluBitwiseCtrlEnum.XOR  -> (srcp.SRC1 ^ srcp.SRC2),
        AluBitwiseCtrlEnum.ZERO -> S(0, Riscv.XLEN bits)
      )
      KeepAttribute(bitwise)

      val result = bitwise | srcp.ADD_SUB.andMask(ALU_ADD_SUB) | S(U(srcp.LESS, Riscv.XLEN bits)).andMask(ALU_SLTX)
      ALU_RESULT := result.asBits
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := ALU_RESULT
    }
  }
}
