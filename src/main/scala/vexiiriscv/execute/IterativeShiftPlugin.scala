// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.riscv._

object IterativeShifterPlugin extends AreaObject {
  val ARITHMETIC = Payload(Bool())
  val LEFT = Payload(Bool())
  val IS_W = Payload(Bool())
  val IS_W_RIGHT = Payload(Bool())
}

class IterativeShifterPlugin(val layer: LaneLayer,
                             val shiftAt: Int = 0,
                             val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  import IterativeShifterPlugin._
  val SHIFT_RESULT = Payload(Bits(Riscv.XLEN bits))
  val SHIFT_DONE = Payload(Bool())

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = ifp.access(formatAt)
    implicit val _ = ifp -> wb

    //TODO why using SRC1 ? why not directly RS1 => less combinatorial path, also not sure about SRC2 is really worth it (for only 5/ 6 bits)
    add(Rvi.SLL).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True, ARITHMETIC -> False)
    add(Rvi.SRL).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, ARITHMETIC -> False)
    add(Rvi.SRA).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, ARITHMETIC -> True)
    add(Rvi.SLLI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> True, ARITHMETIC -> False)
    add(Rvi.SRLI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, ARITHMETIC -> False)
    add(Rvi.SRAI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, ARITHMETIC -> True)

    if (Riscv.XLEN.get == 64) {
      for (op <- List(Rvi.SLL, Rvi.SRL, Rvi.SRA, Rvi.SLLI, Rvi.SRLI, Rvi.SRAI)) {
        layer(op).addDecoding(IS_W -> False, IS_W_RIGHT -> False)
      }
      add(Rvi.SLLW).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True, ARITHMETIC -> False, IS_W -> True, IS_W_RIGHT -> False)
      add(Rvi.SRLW).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, ARITHMETIC -> False, IS_W -> True, IS_W_RIGHT -> True)
      add(Rvi.SRAW).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, ARITHMETIC -> True, IS_W -> True, IS_W_RIGHT -> True)
      add(Rvi.SLLIW).srcs(SRC1.RF, SRC2.I).decode(LEFT -> True, ARITHMETIC -> False, IS_W -> True, IS_W_RIGHT -> False)
      add(Rvi.SRLIW).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, ARITHMETIC -> False, IS_W -> True, IS_W_RIGHT -> True)
      add(Rvi.SRAIW).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, ARITHMETIC -> True, IS_W -> True, IS_W_RIGHT -> True)
      for (op <- List(Rvi.SLLW, Rvi.SRLW, Rvi.SRAW, Rvi.SLLIW, Rvi.SRLIW, Rvi.SRAIW)) {
        ifp.signExtend(wb, layer(op), 32)
      }
    }

    uopRetainer.release()

    // SxxIW with amplitude[5] == 0 is reserved - so we just ignore it
    val shift = new el.Execute(shiftAt) {
      val unscheduleRequest = RegNext(isCancel).clearWhen(isReady).init(False)
      val selected = isValid && SEL

      val amplitudeWidth = if(Riscv.XLEN.get == 64) 6 else 5
      val shamt = srcp.SRC2.resize(amplitudeWidth bit).asUInt
      val zeroShift = shamt === 0

      val busy = RegInit(False)
      val amplitudeReg = Reg(cloneOf(shamt))
      val amplitude = busy ? amplitudeReg | shamt
      val done = amplitude(4 downto 1) === 0

      val signExtended =
        if(Riscv.XLEN.get == 64)
          IS_W_RIGHT ? srcp.SRC1(31).asSInt.resize(32).resize(64).asBits | srcp.SRC1.asBits
        else
          srcp.SRC1.asBits

      val shiftReg = Reg(cloneOf(srcp.SRC1.asBits))
      val shifted = cloneOf(srcp.SRC1.asBits)
      val shiftInput = busy ? shiftReg | signExtended
      val shiftResult = zeroShift ? signExtended | shifted

      shifted := (LEFT & True).mux(
        True -> (shiftInput |<< 1),
        default -> (((ARITHMETIC && srcp.SRC1.msb) ## shiftInput) >> 1)
      )

      when(selected && !zeroShift && !done) {
        busy := True
        shiftReg := shiftResult
        amplitudeReg := amplitude - 1
      }
      when((busy && done) || unscheduleRequest) {
        busy := False
      }

      val freeze = selected && !zeroShift && !done && !unscheduleRequest
      eu.freezeWhen(freeze)

      SHIFT_DONE := selected && done
      SHIFT_RESULT := shiftResult
    }

    val format = new eu.Execute(formatAt) {
      wb.valid := SHIFT_DONE
      wb.payload := SHIFT_RESULT
    }
  }
}
