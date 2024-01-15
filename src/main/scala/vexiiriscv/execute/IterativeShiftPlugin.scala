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

/** Iterative Shifter
  *
  * The default parameters are optimized for 6-input LUT devices (since it will
  * use 4:1 muxes (load/shift 1/shift 8/flip), but additional shifts can be
  * added to increase performance.
  * Only 2**n shift distances are well supported, other distances will lead
  * to suboptimal shift sequence if needed multiple times.
  *
  * lateResult can be used to make the done logic slighly smaller, which should not be
  * needed for the default configuration. Enabling it costs 1 cycle for all shifts.
  */
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

    // we use RS1 directly, keep SRC1.RF source here for hazard detection
    // TODO not sure about SRC2 is really worth it (for only 5/ 6 bits)
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
      val rs1 = el(IntRegFile, RS1).asBits

      val busy = RegInit(False)
      val flipped = Reg(Bool())
      val amplitude = Reg(cloneOf(shamt))
      val shiftReg = Reg(cloneOf(srcp.SRC1.asBits))

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
          IS_W_RIGHT ? (B(32 bit, default -> (ARITHMETIC & rs1(31))) ## rs1(31 downto 0)) | rs1
        else
          CombInit(rs1)

      val muxed = cloneOf(srcp.SRC1.asBits)

      // right-shift 1 is the fallback, no check needed - drop the 1 on the loop
      amplitude := amplitude - 1
      muxed := (((ARITHMETIC && srcp.SRC1.msb) ## shiftReg) >> 1)
      rightShifts.sorted.drop(1).foreach(n => when(amplitude >= n) {
        println(s"having right shift by $n")
        amplitude := amplitude - n
        muxed := ((((ARITHMETIC && srcp.SRC1.msb) #* n) ## shiftReg) >> n)
      })

      leftShifts.sorted.foreach(n => when(LEFT & amplitude >= n) { // TODO check synthesis for these
        println(s"having left shift by $n")
        // mask away bits shifted into upper 32 bit in case of SLLW instruction
        val mask_w = (True #* (32 - n)) ## (!IS_W #* n) ## (True #* 32)
        amplitude := amplitude - n
        muxed := (shiftReg |<< n) & mask_w
      })

      // we only need flip if there is no left shift
      if (leftShifts.isEmpty && Riscv.XLEN.get == 32) {
        val doFlip = LEFT & ((busy & !flipped) | (if (lateResult) (busy & amplitude === 0) else done))
        println("having flip")
        when(doFlip) {
          amplitude := amplitude // prevent amplitude changes from matches with conditions above, TODO check synthesis
          flipped := !flipped
          muxed := shiftReg.reversed
        }
      } else if(leftShifts.isEmpty) {
        val doFlip = LEFT & ((busy & !flipped) | (if (lateResult) (busy & amplitude === 0) else done))
        println("having flip 64")
        when(doFlip & IS_W) {
          amplitude := amplitude
          flipped := !flipped
          muxed(31 downto 0) := shiftReg(31 downto 0).reversed
          muxed(63 downto 32) := 0
        }
        when(doFlip && !IS_W) {
          amplitude := amplitude
          flipped := !flipped
          muxed := shiftReg.reversed
        }
      }

      when(selected & !busy) {
        flipped := False  
        muxed := signExtended
        amplitude := shamt
        busy := (if(!lateResult) shamt =/= 0 else True)
      }

      shiftReg := muxed

      when((busy && done) || unscheduleRequest) {
        busy := False
      }

      val freeze = selected && !done && !unscheduleRequest
      el.freezeWhen(freeze)

      SHIFT_RESULT := (if (lateResult) shiftReg else muxed)
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := SHIFT_RESULT
      if(Riscv.XLEN.get == 64) {
        when(ARITHMETIC & IS_W) {
          wb.payload(32, 32 bit).setAllTo(SHIFT_RESULT(31))
        }
      }
    }
  }
}
