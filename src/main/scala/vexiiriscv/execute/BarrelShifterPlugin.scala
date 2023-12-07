// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.riscv._

object BarrelShifterPlugin extends AreaObject {
  val SIGNED = Payload(Bool())
  val LEFT = Payload(Bool())
  val IS_W = Payload(Bool())
  val IS_W_RIGHT = Payload(Bool())
}

class BarrelShifterPlugin(val layer : LaneLayer,
                          var shiftAt : Int = 0,
                          var formatAt : Int = 0) extends ExecutionUnitElementSimple(layer)  {
  import BarrelShifterPlugin._
  lazy val ifp = host.find[IntFormatPlugin](_.laneName == layer.el.laneName)
  buildBefore(ifp.elaborationLock)

  val SHIFT_RESULT = Payload(Bits(Riscv.XLEN bits))

  val logic = during build new Logic{
    import SrcKeys._

    val wb = ifp.access(formatAt)
    implicit val _ = ifp -> wb

    //TODO why using SRC1 ? why not directly RS1 => less combinatorial path, also not sure about SRC2 is realy wort it (for only 5/ 6 bits)
    add(Rvi.SLL).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True, SIGNED -> False)
    add(Rvi.SRL).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, SIGNED -> False)
    add(Rvi.SRA).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, SIGNED -> True)
    add(Rvi.SLLI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> True, SIGNED -> False)
    add(Rvi.SRLI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, SIGNED -> False)
    add(Rvi.SRAI).srcs(SRC1.RF, SRC2.I).decode(LEFT -> False, SIGNED -> True)

    if (Riscv.XLEN.get == 64) {
      for (op <- List(Rvi.SLL, Rvi.SRL, Rvi.SRA, Rvi.SLLI, Rvi.SRLI, Rvi.SRAI)) {
        layer(op).addDecoding(IS_W -> False, IS_W_RIGHT -> False)
      }
      add(Rvi.SLLW ).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> True , SIGNED -> False, IS_W -> True, IS_W_RIGHT -> False )
      add(Rvi.SRLW ).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, SIGNED -> False, IS_W -> True, IS_W_RIGHT -> True )
      add(Rvi.SRAW ).srcs(SRC1.RF, SRC2.RF).decode(LEFT -> False, SIGNED -> True , IS_W -> True, IS_W_RIGHT -> True )
      add(Rvi.SLLIW).srcs(SRC1.RF, SRC2.I ).decode(LEFT -> True , SIGNED -> False, IS_W -> True, IS_W_RIGHT -> False)
      add(Rvi.SRLIW).srcs(SRC1.RF, SRC2.I ).decode(LEFT -> False, SIGNED -> False, IS_W -> True, IS_W_RIGHT -> True )
      add(Rvi.SRAIW).srcs(SRC1.RF, SRC2.I ).decode(LEFT -> False, SIGNED -> True , IS_W -> True, IS_W_RIGHT -> True )
      for (op <- List(Rvi.SLLW, Rvi.SRLW, Rvi.SRAW, Rvi.SLLIW, Rvi.SRLIW, Rvi.SRAIW)) {
        ifp.signExtend(wb, layer(op), 32)
      }
    }

    eu.uopLock.release()
    srcp.elaborationLock.release()

    val shift = new eu.Execute(shiftAt) {
      val ss = SrcStageables
      val amplitude = srcp.SRC2(log2Up(Riscv.XLEN.get) - 1 downto 0).asUInt
      val reversed = Mux[SInt](LEFT, srcp.SRC1.reversed, srcp.SRC1)
      val shifted = (S((SIGNED & srcp.SRC1.msb) ## reversed) >> amplitude).resize(Riscv.XLEN bits)
      val patched = LEFT ? shifted.reversed | shifted

      if (Riscv.XLEN.get == 64) {
        when(IS_W_RIGHT) {
          reversed(63 downto 32) := (default -> (SIGNED & srcp.SRC1(31)))
        }
        when(IS_W) {
          amplitude(5) := False
        }
      }

      SHIFT_RESULT := B(patched)
    }

    val format = new eu.Execute(formatAt) {
      wb.valid := SEL
      wb.payload := SHIFT_RESULT
    }
  }
}
