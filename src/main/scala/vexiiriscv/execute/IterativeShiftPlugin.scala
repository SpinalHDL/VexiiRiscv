// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib.{MuxOH, OHMasking}
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.riscv._

import scala.collection.mutable

object IterativeShifterPlugin extends AreaObject {
  val ARITHMETIC = Payload(Bool())
  val LEFT = Payload(Bool())
  val IS_W = Payload(Bool())
  val IS_W_RIGHT = Payload(Bool())
  val IS_UW = Payload(Bool())
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
                             val with_slli_uw: Boolean = false,
                             val shiftAt: Int = 0,
                             val formatAt: Int = 0,
                             val leftShifts: Seq[Int] = Seq(),
                             val rightShifts: Seq[Int] = Seq(1, 8),
                             val lateResult: Boolean = false) extends ExecutionUnitElementSimple(layer) {
  def isPowerTwo(i: Int) = i > 0 && (i & (i - 1)) == 0
  assert(leftShifts.isEmpty || leftShifts.contains(1), "If left shifts are used, left shift by 1 must be enabled")
  assert(rightShifts.contains(1), "At least right shift by 1 to the right must be enabled")
  assert(leftShifts.forall(isPowerTwo) && rightShifts.forall(isPowerTwo), "shift distances must be power of 2")

  import IterativeShifterPlugin._
  val SHIFT_RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)

    // we use RS1 directly, keep SRC1.RF source here for hazard detection
    add(Rvi.SLL).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True, ARITHMETIC -> False)
    add(Rvi.SRL).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, ARITHMETIC -> False)
    add(Rvi.SRA).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, ARITHMETIC -> True)
    add(Rvi.SLLI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> True, ARITHMETIC -> False)
    add(Rvi.SRLI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, ARITHMETIC -> False)
    add(Rvi.SRAI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, ARITHMETIC -> True)

    if (Riscv.XLEN.get == 64) {
      for (op <- List(Rvi.SLL, Rvi.SRL, Rvi.SRA, Rvi.SLLI, Rvi.SRLI, Rvi.SRAI)) {
        layer(op).addDecoding(IS_W -> False, IS_W_RIGHT -> False)
        if(with_slli_uw)
          layer(op).addDecoding(IS_UW -> False)
      }
      add(Rvi.SLLW ).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True , ARITHMETIC -> False, IS_W_RIGHT -> False)
      add(Rvi.SRLW ).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, ARITHMETIC -> False, IS_W_RIGHT -> True )
      add(Rvi.SRAW ).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, ARITHMETIC -> True , IS_W_RIGHT -> True )
      add(Rvi.SLLIW).srcs(SRC1.RF, SRC2.I ).decode(LEFT -> True , ARITHMETIC -> False, IS_W_RIGHT -> False)
      add(Rvi.SRLIW).srcs(SRC1.RF, SRC2.I ).decode(LEFT -> False, ARITHMETIC -> False, IS_W_RIGHT -> True )
      add(Rvi.SRAIW).srcs(SRC1.RF, SRC2.I ).decode(LEFT -> False, ARITHMETIC -> True , IS_W_RIGHT -> True )
      for (op <- List(Rvi.SLLW, Rvi.SRLW, Rvi.SRAW, Rvi.SLLIW, Rvi.SRLIW, Rvi.SRAIW)) {
        layer(op).addDecoding(IS_W -> True)
        ifp.signExtend(wb, layer(op), 32)

        if(with_slli_uw)
          layer(op).addDecoding(IS_UW -> False)
      }
      if (with_slli_uw) {
        add(RvZbx.SLLI_UW).srcs(SRC1.RF, SRC2.I).decode(LEFT -> True, ARITHMETIC -> False, IS_W -> False, IS_W_RIGHT -> False, IS_UW -> True)
      }
    }

    uopRetainer.release()

    val shift = new el.Execute(shiftAt) {
      val unscheduleRequest = RegNext(isCancel).clearWhen(isReady).init(False)
      val selected = isValid && SEL

      val amplitudeWidth = if(Riscv.XLEN.get == 64) 6 else 5
      val shamt = srcp.SRC2.resize(amplitudeWidth).asUInt
      val rs1 = up(el(IntRegFile, RS1)).asBits

      val busy = RegInit(False)
      val flipped = Reg(Bool())
      val amplitude = Reg(UInt(amplitudeWidth bits))
      val shiftReg = Reg(Bits(Riscv.XLEN bits))

      val dataIn = CombInit(rs1)
      if (Riscv.XLEN.get == 64) {
        when(IS_W) {
          shamt(5) := False
        }
        when(IS_W_RIGHT) {
          dataIn(63 downto 32) := (default -> (ARITHMETIC & rs1(31)))
        }
        if (with_slli_uw) {
          when(IS_UW) {
            dataIn(63 downto 32) := 0
          }
        }
      }

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

      val muxInputs = mutable.ArrayBuffer[(Bool, Bits, UInt)]()
      rightShifts.sorted.foreach(n => {
        val doIt = if(n > 1) amplitude >= n else True
        val input = (((ARITHMETIC && srcp.SRC1.msb) #* n) ## shiftReg) >> n
        val ampl = amplitude - n
        muxInputs.append((doIt, input, ampl))
      })

      leftShifts.sorted.foreach(n => {
        val doIt = LEFT & (if(n > 1) amplitude >= n else True)
        val input = shiftReg |<< n
        val ampl = amplitude - n
        muxInputs.append((doIt, input, ampl))
      })

      // we only need flip if there is no left shift
      // keep track of flip indices so that we can skip them when needed
      val flipIdxs = mutable.ArrayBuffer[Int]()
      if (leftShifts.isEmpty) {
        val doFlip = LEFT & ((busy & !flipped) | (if (lateResult) (busy & amplitude === 0) else done))

        flipIdxs.append(muxInputs.size)
        muxInputs.append((doFlip, shiftReg.reversed, amplitude))
        if (Riscv.XLEN.get == 64) {
          flipIdxs.append(muxInputs.size)
          muxInputs.append((IS_W & doFlip, False #* 32 ## shiftReg(31 downto 0).reversed, amplitude))
        }
      }
      muxInputs.append((selected & !busy, dataIn, shamt))

      val selector = OHMasking.last(Cat(muxInputs.map(_._1)))
      val muxed = MuxOH(selector, muxInputs.map(_._2))
      shiftReg := muxed

      // if we use flip(s), then we don't need them in the amplitude MUX, we just just not
      // EN the amplitude register in that case, saving a few gates
      val updatedAmplitude = MuxOH(
        Cat(selector.asBools.zipWithIndex.flatMap{case (b, n) => if(flipIdxs.contains(n)) None else Some(b)}),
        muxInputs.zipWithIndex.flatMap{case(i, n) => if(flipIdxs.contains(n)) None else Some(i._3)}
      )

      val anyFlip = flipIdxs.foldLeft(False){case (r, n) => r | muxInputs(n)._1}
      if(flipIdxs.nonEmpty) {
        when(anyFlip) {
          flipped := !flipped
        }
      }
      when(!anyFlip) {
        amplitude := updatedAmplitude
      }

      // if we do the load, initialize some other parts of the state as well
      when(selector.msb) {
        flipped := False  
        busy := (if(!lateResult) shamt =/= 0 else True)
      }

      when((busy && done) || unscheduleRequest) {
        busy := False
      }

      val freezeIt = selected && !done && !unscheduleRequest
      el.freezeWhen(freezeIt)

      SHIFT_RESULT := (if (lateResult) shiftReg else muxed)
    }

    val format = new el.Execute(formatAt) {
      // sign extends for 32bit ops on 64bit core are done by the ifp plugin
      wb.valid := SEL
      wb.payload := SHIFT_RESULT
    }
  }
}
