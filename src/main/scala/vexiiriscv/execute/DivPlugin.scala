package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.execute.RsUnsignedPlugin._
import vexiiriscv.misc.{AdderAggregator, DivComp, DivRadix, DivRadix2, DivRsp, MulSpliter}
import vexiiriscv.riscv.Riscv._
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DivPlugin extends AreaObject {
  val REM = Payload(Bool())
  val DIV_RESULT = Payload(Bits(XLEN bits))
}

trait DivReuse{
  def divInject(layer : LaneLayer, at : Int, a : UInt, b : UInt, iterations : UInt) : Unit
  def divRsp : DivRsp
  def divRadix : Int
}

class DivPlugin(val layer : LaneLayer,
                var impl : (Int, Int, Boolean) => DivComp,
                var divAt: Int = 0,
                var writebackAt : Int = 0,
                var radix: Int = 2,
                var area: Boolean = true) extends ExecutionUnitElementSimple(layer) with DivReuse {
  import DivPlugin._


  override def divInject(layer: LaneLayer, at: Int, a: UInt, b: UInt, interations : UInt): Unit = {
    assert(layer == this.layer && at == this.divAt)
    logic.processing.request := True
    val divWidth = logic.processing.div.width
    assert(divWidth >= widthOf(a))
    assert(divWidth >= widthOf(b))
    logic.processing.div.io.cmd.a := a.resized // << (divWidth-widthOf(a))
    logic.processing.div.io.cmd.b := b.resized // << (divWidth-widthOf(b))
    logic.processing.div.io.cmd.normalized := True
    logic.processing.div.io.cmd.iterations := interations
  }

  override def divRsp: DivRsp = logic.processing.div.io.rsp
  override def divRadix: Int = radix

  val logic = during setup new Logic {
    awaitBuild()

    val formatBus = newWriteback(ifp, writebackAt)

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
        layer(op).addDecoding(IS_W -> False)
      }
    }

    uopRetainer.release()

    val processing = new el.Execute(divAt) {
      val div = impl(Riscv.XLEN, radix, area)

      val divRevertResult = RegNext((RS1_REVERT ^ (RS2_REVERT && !REM)) && !(RS2_FORMATED === 0 && RS2_SIGNED && !REM)) //RS2_SIGNED == RS1_SIGNED anyway

      val cmdSent = RegInit(False) setWhen (div.io.cmd.fire) clearWhen (isReady)
      val request = isValid && SEL
      div.io.cmd.valid := request && !cmdSent
      div.io.cmd.a := RS1_UNSIGNED.resized
      div.io.cmd.b := RS2_UNSIGNED.resized
      div.io.cmd.normalized := False
      div.io.cmd.iterations.assignDontCare()
      div.io.flush := isReady
      div.io.rsp.ready := False


      val unscheduleRequest = RegNext(isCancel) clearWhen (isReady) init (False)
      val freeze = request && !div.io.rsp.valid & !unscheduleRequest
      el.freezeWhen(freeze)

      val selected = REM ? div.io.rsp.remain otherwise div.io.rsp.result

      def twoComplement(that: Bits, enable: Bool): UInt = (Mux(enable, ~that, that).asUInt + enable.asUInt)
      DIV_RESULT := twoComplement(B(selected), divRevertResult).asBits.resized
    }

    val writeback = new el.Execute(writebackAt){
      formatBus.valid := SEL
      formatBus.payload := DIV_RESULT
    }
  }
}
