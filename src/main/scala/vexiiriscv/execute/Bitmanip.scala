// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.execute.BarrelShifterPlugin.{LEFT, SIGNED}
import vexiiriscv.riscv._

import scala.collection.mutable

object ZbbLogicPlugin {
  val OP = Payload(Bits(2 bit))
}

class ZbbLogicPlugin(val layer: LaneLayer,
                     val executeAt: Int = 0,
                     val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  import ZbbLogicPlugin._
  val RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.ANDN).srcs(SRC1.RF, SRC2.RF).decode(OP -> B"00")
    add(RvZbx.ORN).srcs(SRC1.RF, SRC2.RF).decode(OP -> B"01")
    add(RvZbx.XNOR).srcs(SRC1.RF, SRC2.RF).decode(OP -> B"10")
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val rs1 = el(IntRegFile, RS1).asBits
      val rs2 = el(IntRegFile, RS2).asBits

      RESULT := OP.muxDc(
        0 -> (rs1 & ~rs2),
        1 -> (rs1 | ~rs2),
        2 -> (rs1 ^ ~rs2)
      )
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := RESULT
    }
  }
}

object ZbbCountPlugin extends AreaObject {
  val FLIP = Payload(Bool())
  val INVERT = Payload(Bool())
  val MASK = Payload(Bool())
  val WORD = Payload(Bool())
}

class ZbbCountPlugin(val layer: LaneLayer,
                     val executeAt: Int = 0,
                     val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {

  import ZbbCountPlugin._
  val MASKED = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.CLZ).srcs(SRC1.RF).decode(MASK -> True, INVERT -> True, FLIP -> False, WORD -> False)
    add(RvZbx.CTZ).srcs(SRC1.RF).decode(MASK -> True, INVERT -> True, FLIP -> True, WORD -> False)
    add(RvZbx.CPOP).srcs(SRC1.RF).decode(MASK -> True, INVERT -> False, WORD -> False)
    add(RvZbx.CLZW).srcs(SRC1.RF).decode(MASK -> True, INVERT -> True, FLIP -> False, WORD -> True)
    add(RvZbx.CTZW).srcs(SRC1.RF).decode(MASK -> True, INVERT -> True, FLIP -> True, WORD -> True)
    add(RvZbx.CPOPW).srcs(SRC1.RF).decode(MASK -> True, INVERT -> False, WORD -> True)

    uopRetainer.release()

    val count = new el.Execute(executeAt) {
      val rs1 = el(IntRegFile, RS1).asBits
      val inverted = rs1 ^ (apply(INVERT)#* Riscv.XLEN.get)
      val flipped = FLIP ? inverted.reversed | inverted
      val masked = Bits(32 bit)
      for(i <- 0 until Riscv.XLEN.get - 1) {
        masked(i) := flipped(i+1) & masked(i+1)
      }
      MASKED := masked
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := CountOne(MASKED).resized
    }
  }
}

object ZbbMinMaxPlugin {
  val MIN = Payload(Bool())
}

class ZbbMinMaxPlugin(val layer: LaneLayer,
                      val executeAt: Int = 0,
                      val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  import ZbbMinMaxPlugin._
  val RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.MAX).srcs(SRC1.RF, SRC2.RF, Op.LESS).decode(MIN -> False)
    add(RvZbx.MIN).srcs(SRC1.RF, SRC2.RF, Op.LESS).decode(MIN -> True)
    add(RvZbx.MAXU).srcs(SRC1.RF, SRC2.RF, Op.LESS_U).decode(MIN -> False)
    add(RvZbx.MINU).srcs(SRC1.RF, SRC2.RF, Op.LESS_U).decode(MIN -> True)
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      RESULT := (srcp.LESS ^ MIN).mux(this(srcp.SRC2), this(srcp.SRC1))
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := RESULT
    }
  }
}

object ZbbRotatePlugin {
  val LEFT = Payload(Bool())
  val IS_W = Payload(Bool())
}

class ZbbRotatePlugin(val layer: LaneLayer,
                      val executeAt: Int = 0,
                      val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  import ZbbRotatePlugin._
  val RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.ROL).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True)
    add(RvZbx.ROLW).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True)
    add(RvZbx.ROR).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False)
    add(RvZbx.RORI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False)
    if(Riscv.XLEN.get == 64) {
      add(RvZbx.RORIW).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False)
      add(RvZbx.RORW).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False)
    }
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val src1 = if(Riscv.XLEN.get == 64)
        (el(IntRegFile, RS1) & (!IS_W #* 32 ## True #* 32)).asBits
      else
        el(IntRegFile, RS1).asBits

      val amplitude = srcp.SRC2(log2Up(Riscv.XLEN.get) - 1 downto 0).asUInt
      val reversed = LEFT ? src1.reversed | src1
      val shifted = reversed.rotateRight(amplitude)
      val patched = LEFT ? shifted.reversed | shifted

      RESULT := (if (Riscv.XLEN.get == 64) {
        val wordResult = patched(31 downto 0) | patched(64 downto 32)
        IS_W ? wordResult.asSInt.resize(64).asBits | patched
      } else {
        patched
      })
    }
  }
}

class ZbbOrPlugin(val layer: LaneLayer,
                  val executeAt: Int = 0,
                  val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  val RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.ORC_B).srcs(SRC1.RF)

    uopRetainer.release()

    val combine = new el.Execute(executeAt) {
      val bits = el(IntRegFile, RS1).subdivideIn(8 bit).map(_.orR)
      RESULT := bits
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := Cat(RESULT.asBools.map(_ #* 8))
    }
  }
}

class ZbbByteReversePlugin(val layer: LaneLayer,
                           val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(if(Riscv.XLEN.get == 32) RvZbx.REV8_32 else RvZbx.REV8_64).srcs(SRC1.RF)
    uopRetainer.release()

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := Cat(el(IntRegFile, RS1).subdivideIn(8 bit).reverse)
    }
  }
}

class ZbcPlugin(val layer: LaneLayer,
                val executeAt: Int = 0,
                val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  val RESULT = Payload(Bits(Riscv.XLEN.get bit))
  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.CLMUL).srcs(SRC1.RF, SRC2.RF)
    add(RvZbx.CLMULH).srcs(SRC1.RF, SRC2.RF)
    add(RvZbx.CLMULR).srcs(SRC1.RF, SRC2.RF)

    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val rs1 = el(IntRegFile, RS1)
      val rs2 = el(IntRegFile, RS2)

      // TODO

      RESULT := (0 until Riscv.XLEN.get).map(i => (rs1 << i) & (rs2(i) #* Riscv.XLEN.get)).reduceBalancedTree(_ ^ _)
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := RESULT
    }
  }
}

object ZbsPlugin {
  val INSTRUCTION = Payload(Bits(2 bit))
}

class ZbsPlugin(val layer: LaneLayer,
                val decodeAt: Int = 0,
                val executeAt: Int = 0,
                val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  import ZbsPlugin._

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._
    val MASK = Payload(Bits(Riscv.XLEN bits))
    val RESULT = Payload(Bits(Riscv.XLEN bits))

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.BCLR).srcs(SRC1.RF, SRC2.RF).decode(INSTRUCTION -> B"00")
    add(RvZbx.BEXT).srcs(SRC1.RF, SRC2.RF).decode(INSTRUCTION -> B"01")
    add(RvZbx.BINV).srcs(SRC1.RF, SRC2.RF).decode(INSTRUCTION -> B"10")
    add(RvZbx.BSET).srcs(SRC1.RF, SRC2.RF).decode(INSTRUCTION -> B"11")

    add(RvZbx.BCLRI).srcs(SRC1.RF, SRC2.I).decode(INSTRUCTION -> B"00")
    add(RvZbx.BEXTI).srcs(SRC1.RF, SRC2.I).decode(INSTRUCTION -> B"01")
    add(RvZbx.BINVI).srcs(SRC1.RF, SRC2.I).decode(INSTRUCTION -> B"10")
    add(RvZbx.BSETI).srcs(SRC1.RF, SRC2.I).decode(INSTRUCTION -> B"11")

    uopRetainer.release()

    val decode = new el.Execute(decodeAt) {
      val mask = B(0, Riscv.XLEN bits)
      mask(srcp.SRC2.resize(log2Up(Riscv.XLEN) bit).asUInt) := True
      MASK := mask
    }

    val execute = new el.Execute(executeAt) {
      val rs1 = el(IntRegFile, RS1)
      RESULT := INSTRUCTION.mux(
        0 -> (rs1 & ~MASK),
        1 -> (rs1 & MASK),
        2 -> (rs1 ^ MASK),
        3 -> (rs1 | MASK)
      )
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := RESULT
    }
  }
}