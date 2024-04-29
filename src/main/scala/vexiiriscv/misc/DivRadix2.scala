// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.misc

import spinal.core.{B, U, _}
import spinal.lib._


class DivRadix2(width: Int, val lowArea: Boolean = true) extends DivComp(width, 1){

  val predictShiftInCycle0 = false // Does not work yet.

  // Define the possible shift amounts which can be done in a single cycle.
  // This defines the tradeoff between area and number of cycles used for divisions (especially for small numbers).
  // The list must be sorted in ascending order.
  // The list must contain 1.
  val possibleShiftAmounts = if (lowArea) {
    List(1) // Reduce area at the cost of slower division.
  } else {
    List(1, 2, 8)
  }
  require(possibleShiftAmounts.head == 1, "first shift amount must be 1")
  require(possibleShiftAmounts == possibleShiftAmounts.sorted, "list of possible shift amounts must be sorted")


  val counter = Reg(UInt(log2Up(width - 1) bits))
  val busy = RegInit(False)
  val done = RegInit(False) clearWhen (io.rsp.fire)

  val remainder = Reg(UInt(width * 2 bits))
  val denominator = io.cmd.b
  val denominatorExtended = (denominator @@ U(0, width bits))
  val denomMsbMask = Reg(Bits(width bits))
  val quotient = Reg(UInt(width bits))

  io.rsp.valid := done
  io.rsp.result := quotient
  io.rsp.remain := remainder(width * 2 - 1 downto width)
  io.cmd.ready := !busy

  val shiftAmountOH = Reg(Bits(possibleShiftAmounts.length bits)) // One-hot encoding
  assert(
    shiftAmountOH.asBools.map(U(_)).reduce(_ +^ _) === 1 || !busy,
    "one-hot encoding must have exactly one bit set"
  )

  // Shift amount for remainder in the current cycle.
  val currentShiftAmountInt = OHMux(shiftAmountOH, possibleShiftAmounts.map(U(_)))

  val remainderInput = io.cmd.a.resized

  // Use this value for predicting the next shift amount.
  val remainderForShiftPrediction = if (predictShiftInCycle0) {
    Mux(busy, remainder, remainderInput)
  } else {
    remainder
  }

  when(!busy && !done) {
    // Initialize.
    busy := io.cmd.valid
    remainder := remainderInput
    counter := width - 1
    quotient := 0

    // Mask all bit with zero which are higher than the highest set bit in the denominator.
    val denominatorMsb = OHMasking.firstV2(denominator)
    denomMsbMask := ~(U(BigInt(1) << width) - denominatorMsb).asBits.resize(width bits)
  }

  // Next counter value.
  val counterNext = (counter - currentShiftAmountInt).resized

  // Predict shift amount for next cycle.
  // Predict how many trailing bits will be set to zero in the current cycle.
  {

    // Create one-hot encoding of next shift amount
    shiftAmountOH := B(1) // Default: shift amount = 1


    // Find the bits which will be turned to zero in the next cycle.
    val equalBits = ~(denominatorExtended ^ remainderForShiftPrediction).asBits
    val zeroBits = ~remainderForShiftPrediction.asBits

    // Make sure that all bits on the left of a zero-bit are zero as well.
    val fusedEqualBits = equalBits.asBools.reverse
      .scanLeft(True)(_ & _)
      .drop(1)
      .map(U(_))
      .reverse
      .asBits()
    assert(fusedEqualBits.getWidth == width * 2)

    val fusedZeroBits = zeroBits.asBools.reverse
      .scanLeft(True)(_ & _)
      .drop(1)
      .map(U(_))
      .reverse
      .asBits()
    assert(fusedZeroBits.getWidth == width * 2)

    val fusedOrZero = fusedEqualBits | fusedZeroBits

    // Ignore bits with higher value than the highest bit of the denominator.
    val ones = ~B(0, width bits)
    val masked = (fusedOrZero & (denomMsbMask ## ones)).asUInt

    // Count number of ones.
    val numLeadingOnes = masked.asBools.map(U(_)).reduceBalancedTree(_ +^ _) +^ U(1)


    // Compensate the shift amount which is used in the current cycle.
    val maxShiftAmount = {

      val currentShiftAmount =
        if (predictShiftInCycle0) {
          Mux(
            busy,
            currentShiftAmountInt,
            U(0) // currentShiftAmountInt ist not valid yet
          )
        } else {
          currentShiftAmountInt
        }

      Mux(
        numLeadingOnes > currentShiftAmount,
        numLeadingOnes - currentShiftAmount,
        U(0)
      )
        .min(counterNext)
    }


    // Set shift amount for next cycle:
    when(busy || Bool(predictShiftInCycle0)) {
      for ((shiftAmount, i) <- possibleShiftAmounts.zipWithIndex) {
        when(maxShiftAmount >= U(shiftAmount)) {
          shiftAmountOH := UIntToOh(i, shiftAmountOH.getWidth)
        }
      }
    }

  }

  // Do division:
  when(busy) {

    // Select between different shifted versions based on the one-hot encoded shift amount.
    val shiftedRemainder = OHMux(
      shiftAmountOH,
      possibleShiftAmounts.map(remainder |<< _)
    )

    val shiftedUpper = shiftedRemainder(width * 2 - 1 downto width)
    val shiftedLower = shiftedRemainder(width - 1 downto 0)
    val trySub = shiftedUpper -^ denominator
    val underflow = trySub.msb
    val doSub = !underflow

    // Shift the remainder
    val remainderUpperNext = Mux(doSub, trySub(width - 1 downto 0), shiftedUpper)

    // Do remaining shift.
    val quotientShifted = OHMux(
      shiftAmountOH,
      possibleShiftAmounts.map(_ - 1).map(quotient |<< _)
    )

    quotient := (quotientShifted @@ U(doSub)).resize(width)

    remainder := (remainderUpperNext @@ shiftedLower)

    when(counter === 0) {
      busy := False
      done := True
    }

    // shiftAmount is chosen such that this should not underflow.
    assert(counter >= currentShiftAmountInt || counter === 0, "counter should not underflow except in the last cycle")
    counter := counterNext
  }

  when(io.flush) {
    done := False
    busy := False
  }
}


object DivRadix2TesterExhaustive extends App {

  import spinal.core.sim._

  SimConfig.withWave.compile(new DivRadix2(8)).doSim(seed = 52) { dut =>

    var cycleCount = 0
    fork {
      while (true) {
        dut.clockDomain.waitSampling()
        cycleCount += 1
      }
    }

    dut.clockDomain.forkStimulus(10)
    dut.io.cmd.valid #= false
    dut.io.rsp.ready #= true
    dut.clockDomain.waitSampling()

    for (a <- 0 until 256) {
      for (b <- 0 until 256) {
        dut.io.cmd.valid #= true
        dut.io.cmd.a #= a
        dut.io.cmd.b #= b
        dut.clockDomain.waitSampling()
        dut.io.cmd.valid #= false
        dut.clockDomain.waitSamplingWhere(dut.io.rsp.valid.toBoolean)

        if (b != 0) {
          // TODO: What happens when dividing by zero???
          assert(dut.io.rsp.result.toInt == a / b)
          assert(dut.io.rsp.remain.toInt == a % b)
        }

        dut.clockDomain.waitSampling()
      }
    }
    print("cycle count = ", cycleCount)
    simSuccess()
  }
}

object DivRadix2TesterRandomized extends App {

  import spinal.core.sim._

  SimConfig.withWave.compile(new DivRadix2(16)).doSim(seed = 52) { dut =>

    var cycleCount = 0
    fork {
      while (true) {
        dut.clockDomain.waitSampling()
        cycleCount += 1
      }
    }

    dut.clockDomain.forkStimulus(10)
    dut.io.cmd.valid #= false
    dut.clockDomain.onSamplings(dut.io.rsp.ready.randomize())
    dut.clockDomain.waitSampling()

    for (i <- 0 until 1000) {
      dut.io.cmd.valid #= true
      val a = dut.io.cmd.a.randomizedInt()
      val b = dut.io.cmd.b.randomizedInt()

      dut.io.cmd.a #= a
      dut.io.cmd.b #= b

      dut.clockDomain.waitSampling()
      dut.io.cmd.valid #= false
      dut.clockDomain.waitSamplingWhere(dut.io.rsp.valid.toBoolean && dut.io.rsp.ready.toBoolean)

      if (b != 0) {
        assert(dut.io.rsp.result.toInt == a / b)
        assert(dut.io.rsp.remain.toInt == a % b)
      }

      dut.clockDomain.waitSampling()

    }
    print("cycle count = ", cycleCount)
    simSuccess()
  }
}