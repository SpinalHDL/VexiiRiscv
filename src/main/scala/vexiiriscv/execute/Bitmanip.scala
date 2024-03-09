// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv

object ZbPlugin {
  def make(layer: LaneLayer,
           executeAt: Int = 0,
           formatAt: Int = 0) = {
    Seq(
        /*new ZbaPlugin(layer, executeAt, formatAt),
        new ZbbLogicPlugin(layer, executeAt, formatAt),
        new ZbbCountPlugin(layer, executeAt, formatAt),
        new ZbbMinMaxPlugin(layer, executeAt, formatAt),
        new ZbbRotatePlugin(layer, executeAt, formatAt),
        new ZbbOrPlugin(layer, executeAt, formatAt),
        new ZbbByteReversePlugin(layer, formatAt),
        new ZbbExtendPlugin(layer, formatAt),
        new ZbcPlugin(layer, executeAt, formatAt),*/
        new ZbsPlugin(layer, executeAt, executeAt, formatAt)
      )
    }
}

object ZbaPlugin {
  val MUX = Payload(Bits(3 bit))
}

class ZbaPlugin(val layer: LaneLayer,
                val executeAt: Int = 0,
                val formatAt: Int = 0)  extends ExecutionUnitElementSimple(layer) {
  val RESULT = Payload(SInt(Riscv.XLEN bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._
    import ZbaPlugin._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.SH1ADD).srcs(SRC1.RF, SRC2.RF).decode(MUX -> B"001")
    add(RvZbx.SH2ADD).srcs(SRC1.RF, SRC2.RF).decode(MUX -> B"010")
    add(RvZbx.SH3ADD).srcs(SRC1.RF, SRC2.RF).decode(MUX -> B"100")
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val sh1 = srcp.SRC1 |<< 1
      val sh2 = srcp.SRC1 |<< 2
      val sh3 = srcp.SRC1 |<< 3
      val sh = MuxOH(MUX, Seq(sh1, sh2, sh3))
      RESULT := el(IntRegFile, RS2).asSInt + sh
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := RESULT.asBits
    }
  }
}

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

    // TODO use ifp for getting word instead of mask
    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.CLZ).srcs(SRC1.RF).decode(MASK -> True, INVERT -> True, FLIP -> False, WORD -> False)
    add(RvZbx.CTZ).srcs(SRC1.RF).decode(MASK -> True, INVERT -> True, FLIP -> True, WORD -> False)
    add(RvZbx.CPOP).srcs(SRC1.RF).decode(MASK -> False, INVERT -> False, WORD -> False)
    add(RvZbx.CLZW).srcs(SRC1.RF).decode(MASK -> True, INVERT -> True, FLIP -> False, WORD -> True)
    add(RvZbx.CTZW).srcs(SRC1.RF).decode(MASK -> True, INVERT -> True, FLIP -> True, WORD -> True)
    add(RvZbx.CPOPW).srcs(SRC1.RF).decode(MASK -> True, INVERT -> False, WORD -> True)

    uopRetainer.release()

    val count = new el.Execute(executeAt) {
      // TODO optimize by merging stuff
      val rs1 = el(IntRegFile, RS1).asBits
      val inverted = rs1 ^ (apply(INVERT) #* Riscv.XLEN.get)
      val flipped = FLIP ? inverted.reversed | inverted
      val masked = Vec(Bool(), Riscv.XLEN.get)
      masked(masked.size - 1) := flipped.msb
      for(i <- 0 until Riscv.XLEN.get - 1) {
        masked(i) := flipped(i) & (masked(i+1) | !MASK)
      }
      MASKED := Cat(masked)
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := CountOne(MASKED).asBits.resized
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
  val RESULT = Payload(SInt(Riscv.XLEN bits))

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
      wb.payload := RESULT.asBits
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
    add(RvZbx.ROL).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True, IS_W -> False)
    add(RvZbx.ROLW).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True, IS_W -> False)
    add(RvZbx.ROR).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, IS_W -> False)
    add(RvZbx.RORI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, IS_W -> False)
    if(Riscv.XLEN.get == 64) {
      add(RvZbx.RORIW).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, IS_W -> True)
      add(RvZbx.RORW).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, IS_W -> True)
    }
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val src1 = if(Riscv.XLEN.get == 64)
        el(IntRegFile, RS1) & B(64 bits, (63 downto 32) -> !IS_W, default -> True)
      else
        el(IntRegFile, RS1).asBits

      val amplitude = srcp.SRC2(log2Up(Riscv.XLEN.get) - 1 downto 0).asUInt
      val reversed = LEFT ? src1.reversed | src1
      val shifted = reversed.rotateRight(amplitude)
      val patched = LEFT ? shifted.reversed | shifted

      RESULT := (if (Riscv.XLEN.get == 64) {
        val wordResult = patched(31 downto 0) | patched(63 downto 32)
        IS_W ? wordResult.asSInt.resize(64).asBits | patched
      } else {
        patched
      })
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := RESULT
    }
  }
}

class ZbbOrPlugin(val layer: LaneLayer,
                  val executeAt: Int = 0,
                  val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  val RESULT = Payload(Bits(Riscv.XLEN/8 bits))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.ORC_B).srcs(SRC1.RF)

    uopRetainer.release()

    val combine = new el.Execute(executeAt) {
      val bits = Cat(el(IntRegFile, RS1).subdivideIn(8 bit).map(_.orR))
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

class ZbbExtendPlugin(val layer: LaneLayer,
                      val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.SEXTB).srcs(SRC1.RF)
    ifp.signExtend(wb, layer(RvZbx.SEXTB), 8)
    add(RvZbx.SEXTH).srcs(SRC1.RF)
    ifp.signExtend(wb, layer(RvZbx.SEXTH), 16)

    val zeroExtend = if(Riscv.XLEN.get == 32) { RvZbx.ZEXTH_32 } else { RvZbx.ZEXTH_64 }
    add(zeroExtend).srcs(SRC1.RF)
    ifp.zeroExtend(wb, layer(zeroExtend), 16)
    uopRetainer.release()

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := srcp.SRC1.asBits
    }
  }
}

object ZbcPlugin {
  val FLIP = Payload(Bool())
  val SHIFT_RS2 = Payload(Bool())
  val MASK_RS2 = Payload(Bool())
}

// Use common CLMUL HW by modifying inputs:
// CLMUL as spec
// CLMULH reversed rs1, rs2, output, rs2 shifted << 1
// CLMULR reversed rs1, rs2, output, rs2 LSB masked
class ZbcPlugin(val layer: LaneLayer,
                val executeAt: Int = 0,
                val formatAt: Int = 0) extends ExecutionUnitElementSimple(layer) {
  import ZbcPlugin._
  val RESULT = Payload(Bits(Riscv.XLEN.get bit))

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, formatAt)
    add(RvZbx.CLMUL).srcs(SRC1.RF, SRC2.RF).decode(FLIP -> False, SHIFT_RS2 -> False, MASK_RS2 -> False)
    add(RvZbx.CLMULH).srcs(SRC1.RF, SRC2.RF).decode(FLIP -> True, SHIFT_RS2 -> True, MASK_RS2 -> False)
    add(RvZbx.CLMULR).srcs(SRC1.RF, SRC2.RF).decode(FLIP -> True, SHIFT_RS2 -> False, MASK_RS2 -> True)

    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val rs1 = this(el(IntRegFile, RS1))
      val rs2 = this(el(IntRegFile, RS2))
      val multiplicand = FLIP ? rs1.reversed | rs1
      val rs2_flipped = el(IntRegFile, RS2).reversed
      val multiplier = (this(SHIFT_RS2) ## this(MASK_RS2)).muxDc(
        0 -> rs2,
        1 -> rs2_flipped,
        2 -> (rs2_flipped |<< 1),
      )

      val masked = Vec.tabulate(Riscv.XLEN.get) { i =>
        multiplier(i) ? (multiplicand |<< i) | (B"0").resized
      }
      RESULT := masked.reduceBalancedTree(_ ^ _)
    }

    val format = new el.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := FLIP ? RESULT.reversed | RESULT
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
  val MASK = Payload(Bits(Riscv.XLEN bits))
  val RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during setup new Logic {
    Riscv.RVZb.set(true)
    awaitBuild()
    import SrcKeys._

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
      val bit = srcp.SRC2.resize(log2Up(Riscv.XLEN) bit).asUInt
      mask(bit) := True
      MASK := mask
    }

    val execute = new el.Execute(executeAt) {
      val rs1 = CombInit(this(el(IntRegFile, RS1)))
      RESULT := INSTRUCTION.mux(
        0 -> (rs1 & ~MASK),
        1 -> B(0, Riscv.XLEN-1 bit) ## (rs1 & MASK).orR,
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