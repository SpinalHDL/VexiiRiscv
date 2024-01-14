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
                             val formatAt: Int = 0,
                             val leftShifts: Seq[Int] = Seq(),
                             val rightShifts: Seq[Int] = Seq(1, 8),
                             val lateResult: Boolean = false) extends ExecutionUnitElementSimple(layer) {
  assert(leftShifts.isEmpty || leftShifts.contains(1), "If left shifts are used, left shift by 1 must be enabled")
  assert(rightShifts.contains(1), "At least right shift by 1 to the right must be enabled")

  import IterativeShifterPlugin._
  val SHIFT_RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)

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
      val shamt = if(Riscv.XLEN.get==32) {
        srcp.SRC2.resize(amplitudeWidth bit).asUInt
      } else {
        // SxxW instructions only use lower 5 bit of SRC2, not 6
        (srcp.SRC2.resize(amplitudeWidth bit).asBits & B(6 bit, 5 -> !IS_W, default -> True)).asUInt
      }

      val busy = RegInit(False)
      val flipped = RegInit(False)
      val amplitude:UInt = Reg(cloneOf(shamt))

      val done = if(lateResult) {
        amplitude === 0 & !flipped & busy
      } else {
        val rightShiftDone = !LEFT & busy & rightShifts.map(amplitude === _).reduce(_ | _)
        val leftShiftDone = if(leftShifts.isEmpty) {
          // we need to fully shift since we still need to flip back
          busy & amplitude === 0
        } else {
          busy & leftShifts.map(amplitude === _).reduce(_ | _)
        }

        // done comes one cycle "early", in the cycle we do the last action (shift / flip)
        shamt === 0 | leftShiftDone | rightShiftDone
      }

      val signExtended =
        if(Riscv.XLEN.get == 64)
          IS_W_RIGHT ? (B(32 bit, default -> (ARITHMETIC & srcp.SRC1(31))) ## srcp.SRC1.asBits(31 downto 0)) | srcp.SRC1.asBits
        else
          CombInit(srcp.SRC1.asBits)

      val shiftReg:Bits = Reg(cloneOf(srcp.SRC1.asBits))

      val muxed = cloneOf(srcp.SRC1.asBits)
      val cond = when(selected & !busy) {
        flipped := False  
        muxed := signExtended
        amplitude := shamt
        busy := (if(!lateResult) shamt =/= 0 else True)
      }

      val condFlipped = if(leftShifts.nonEmpty) {
        cond
      } else if(Riscv.XLEN.get == 32) {
        val doFlip = LEFT & ((busy & !flipped) | (if(lateResult) (busy & amplitude === 0) else done))
        println("having flip")
        cond.elsewhen(doFlip) {
          flipped := !flipped
          muxed := shiftReg.reversed
        }
      } else {
        val doFlip = LEFT & ((busy & !flipped) | (if(lateResult) (busy & amplitude === 0) else done))
        println("having flip 64")
        cond.elsewhen(doFlip && !IS_W) {
          flipped := !flipped
          muxed := shiftReg.reversed
        }.elsewhen(doFlip & IS_W) {
          flipped := !flipped
          muxed(31 downto 0) := shiftReg(31 downto 0).reversed
          muxed(63 downto 32) := 0
        }
      }

      val leftShifted = leftShifts.sorted.reverse.foldLeft(condFlipped)((f, n) => f.elsewhen(LEFT & amplitude >= n) { // TODO check synthesis for these
        println(s"having left shift by $n")
        amplitude := amplitude - n
        muxed := shiftReg |<< n
      })

      val rightShifted = rightShifts.sorted.reverse.dropRight(1).foldLeft(leftShifted)((f, n) => f.elsewhen(amplitude >= n) {
        println(s"having right shift by $n")
        amplitude := amplitude - n
        muxed := ((((ARITHMETIC && srcp.SRC1.msb) #* n) ## shiftReg) >> n)
      })

      rightShifted.otherwise {
        println(s"having right shift by 1")
        amplitude := amplitude - 1
        muxed := (((ARITHMETIC && srcp.SRC1.msb) ## shiftReg) >> 1)
      }

      shiftReg := muxed

      when((busy && done) || unscheduleRequest) {
        busy := False
      }

      val freeze = selected && !done && !unscheduleRequest
      el.freezeWhen(freeze)

      val result = (if (lateResult) shiftReg else muxed)
      val extendedResult = if(Riscv.XLEN.get == 32) {
        result
      } else {
        // TODO move masking to shifter
        // mask if SLLW, sign extended if SxxW instruction
        (result & ((!IS_W #* 32) ## (True #* 32))) | (((result(31) & ARITHMETIC & IS_W) #* 32) ## (False #* 32))
      }

      SHIFT_RESULT := extendedResult
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := SHIFT_RESULT
    }
  }
}
