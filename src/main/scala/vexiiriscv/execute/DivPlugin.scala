package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.execute.RsUnsignedPlugin._
import vexiiriscv.misc.{AdderAggregator, DivRadix4, MulSpliter}
import vexiiriscv.riscv.Riscv._
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DivPlugin extends AreaObject {
  val REM = Payload(Bool())
  val DIV_RESULT = Payload(Bits(XLEN bits))
  val DIV_REVERT_RESULT = Payload(Bool())
}

class DivPlugin(val layer : LaneLayer,
                var divAt: Int = 0,
                val writebackAt : Int = 0) extends ExecutionUnitElementSimple(layer){
  import DivPlugin._

  val logic = during build new Logic {
    val formatBus = ifp.access(writebackAt)
    implicit val _ = ifp -> formatBus

    add(Rvi.DIV ).decode(REM -> False).rsUnsigned(true  , true )
    add(Rvi.DIVU).decode(REM -> False).rsUnsigned(false , false)
    add(Rvi.REM ).decode(REM -> True ).rsUnsigned(true  , true )
    add(Rvi.REMU).decode(REM -> True ).rsUnsigned(false , false)

    if (XLEN.get == 64) {
      add(Rvi.DIVW ).decode(REM -> False).rsUnsigned(true  , true )
      add(Rvi.DIVUW).decode(REM -> False).rsUnsigned(false , false)
      add(Rvi.REMW ).decode(REM -> True ).rsUnsigned(true  , true )
      add(Rvi.REMUW).decode(REM -> True ).rsUnsigned(false , false)

      for (op <- List(Rvi.DIVW, Rvi.DIVUW, Rvi.REMW, Rvi.REMUW)) {
        ifp.signExtend(formatBus, layer(op), 32)
        layer(op).addDecoding(IS_W -> True)
      }

      for (op <- List(Rvi.DIV, Rvi.DIVU, Rvi.REM, Rvi.REMU)) {
        layer(op).addDecoding(IS_W -> True)
      }
    }

    uopRetainer.release()

    val processing = new eu.Execute(divAt) {
      val div = DivRadix4(width = XLEN.get)

      DIV_REVERT_RESULT := (RS1_REVERT ^ (RS2_REVERT && !REM)) && !(RS2_FORMATED === 0 && RS2_SIGNED && !REM) //RS2_SIGNED == RS1_SIGNED anyway

      val cmdSent = RegInit(False) setWhen (div.io.cmd.fire) clearWhen (isReady)
      div.io.cmd.valid := isValid && SEL && !cmdSent
      div.io.cmd.a := RS1_UNSIGNED.resized
      div.io.cmd.b := RS2_UNSIGNED.resized
      div.io.flush := isReady
      div.io.rsp.ready := False

      val unscheduleRequest = RegNext(hasCancelRequest) clearWhen (isReady) init (False)
      val freeze = isValid && SEL && !div.io.rsp.valid & !unscheduleRequest
      eu.freezeWhen(freeze)

      val selected = REM ? div.io.rsp.remain otherwise div.io.rsp.result

      def twoComplement(that: Bits, enable: Bool): UInt = (Mux(enable, ~that, that).asUInt + enable.asUInt)
      DIV_RESULT := twoComplement(B(selected), DIV_REVERT_RESULT).asBits.resized
    }

    val writeback = new eu.Execute(writebackAt){
      formatBus.valid := SEL
      formatBus.payload := DIV_RESULT
    }
  }
}
