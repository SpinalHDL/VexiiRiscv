package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.Shift
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.{FORMAT, ROUNDING, muxDouble, muxRv64}
import vexiiriscv.riscv._




class FpuF2iPlugin(val layer : LaneLayer,
                   var shiftAt : Int = 0,
                   var resultAt : Int = 1,
                   var intWbAt: Int = 1) extends FiberPlugin{
  val p = FpuUtils

  val SEL = Payload(Bool())


  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val iwbp = host.find[IntFormatPlugin](p => p.laneName == layer.laneName)
    val ffwbp = host.find[FpuFlagsWritebackPlugin](p => p.lane == layer.el)
    val buildBefore = retains(layer.el.pipelineLock)
    val uopLock = retains(layer.el.uopLock, fup.elaborationLock, iwbp.elaborationLock, ffwbp.elaborationLock)
    awaitBuild()

    val ffwb = ffwbp.createPort(List(intWbAt))
    val iwb = iwbp.access(intWbAt)

    layer.el.setDecodingDefault(SEL, False)
    def f2i(uop: MicroOp, size : Int, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(decodings)
      spec.addDecoding(SEL -> True)
      fup.unpack(uop, RS1)
      iwbp.addMicroOp(iwb, spec)
      size match {
        case 32 => iwbp.signExtend(iwb, spec, 32)
      }
    }

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    f2i(Rvfd.FCVT_WU_S, 32, f32)
    f2i(Rvfd.FCVT_W_S , 32, f32)
    if (Riscv.XLEN.get == 64) {
      f2i(Rvfd.FCVT_LU_S, 64, f32)
      f2i(Rvfd.FCVT_L_S , 64, f32)
    }
    if (Riscv.RVD) {
      f2i(Rvfd.FCVT_WU_D, 32, f64)
      f2i(Rvfd.FCVT_W_D , 32, f64)
      if (Riscv.XLEN.get == 64) {
        f2i(Rvfd.FCVT_LU_D,64 , f64)
        f2i(Rvfd.FCVT_L_D ,64 , f64)
      }
    }



    uopLock.release()

    val RS1_FP = fup(RS1)

    val shifterWidth = (p.rsIntWidth + 2) max (p.mantissaWidth + 2)
    val onShift = new layer.Execute(shiftAt) {
      val f2iShiftFull = insert(AFix(p.rsIntWidth - 1) - RS1_FP.exponent)
      val f2iShift = insert(U(f2iShiftFull.raw).sat(widthOf(f2iShiftFull.raw) - log2Up(p.rsIntWidth) - 1))

      val SHIFTED = insert(Shift.rightWithScrap(True ## RS1_FP.mantissa.raw ## B(0, shifterWidth - 1 - p.mantissaWidth bits), f2iShift))
    }

    val onResult = new layer.Execute(resultAt){
      val signed = !Decode.UOP(20)
      val i64 = Decode.UOP(21)

      val (high, low) = onShift.SHIFTED.splitAt(shifterWidth - p.rsIntWidth)
      val unsigned = U(high)
      val round = low.msb ## low.dropHigh(1).orR
      val resign = signed && RS1_FP.sign
      val increment = ROUNDING.mux(
        FpuRoundMode.RNE -> (round(1) && (round(0) || unsigned(0))),
        FpuRoundMode.RTZ -> False,
        FpuRoundMode.RDN -> (round =/= 0 && RS1_FP.sign),
        FpuRoundMode.RUP -> (round =/= 0 && !RS1_FP.sign),
        FpuRoundMode.RMM -> (round(1))
      )
      val resultRaw = (Mux(resign, ~unsigned, unsigned) + (resign ^ increment).asUInt)
      val expMax = (i64 ? AFix(62) | AFix(30)) + AFix(!signed)
      val expMin = (i64 ? AFix(63) | AFix(31))
      val unsignedMin = muxRv64[UInt](i64)(BigInt(1) << 63)(BigInt(1) << 31)
      val overflow = (RS1_FP.exponent > expMax || RS1_FP.isInfinity) && !RS1_FP.sign || RS1_FP.isNan
      val underflow = (RS1_FP.exponent > expMin || signed && RS1_FP.exponent === expMin && (unsigned =/= unsignedMin || increment) || !signed && (unsigned =/= 0 || increment) || RS1_FP.isInfinity) && RS1_FP.sign
      val isZero = RS1_FP.isZero
      if (p.rvd) {
        overflow setWhen (!i64 && !RS1_FP.sign && increment && unsigned(30 downto 0).andR && (signed || unsigned(31)))
      }
      val NV = insert(RS1_FP.isNan && !RS1_FP.quiet)
      val NX = insert(False)
      when(isZero) {
        resultRaw := 0
      } elsewhen (underflow || overflow) {
        val low = overflow
        val high = signed ^ overflow
        resultRaw := (31 -> high, default -> low)
        if (p.rsIntWidth == 64) when(i64) {
          resultRaw := (63 -> high, default -> low)
        }
        NV setWhen !isZero
      } otherwise {
        NX setWhen round =/= 0
      }
      val RESULT = insert(resultRaw)
      if (p.rsIntWidth == 64) when(!i64) {
        RESULT(63 downto 32) := (default -> resultRaw(31))
      }
    }

    val onIntWb = new layer.Execute(intWbAt) {
      iwb.valid   := SEL
      iwb.payload := onResult.RESULT.asBits

      ffwb.ats(0) := SEL
      ffwb.flags.assign(NX = onResult.NX, NV = onResult.NV)
    }

    buildBefore.release()
  }
}
