package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.Shift
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.{FORMAT, ROUNDING}
import vexiiriscv.riscv._

/*
 * Implement the RISC-V floating point rounding conversions
 * For Fmax and Area, this plugin is implemented as three different cases for a normal value:
 * 1. exponent < 0
 * This case only means the FP is between -1 and 1. So only need to check whether it is +0.5/-0.5
 *
 * 2. 0 <= exponent < mantissaWidth
 * This case is calculated as following:
 *   a. calculate the zero point position
 *   b. do a shift accumulate to get the round inforamtion
 *   c. calculate the upflow and new mantissa
 * You can reference the FpuF2iPlugin and FpuPackerPlugin.
 *
 * 3. exponent >= mantissaWidth
 * This case means no need to round, just keep it as it is
 */
class FpuRoundPlugin(val layer: LaneLayer,
                     var shiftAt: Int = 0,
                     var roundAt: Int = 1,
                     var packAt: Int = 2) extends FiberPlugin {
  val p = FpuUtils

  val SEL = Payload(Bool())
  val NX = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fpp = host[FpuPackerPlugin]
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fup.elaborationLock, fpp.elaborationLock)
    awaitBuild()

    val packParam = FloatUnpackedParam(
      exponentMax   = p.unpackedConfig.exponentMax,
      exponentMin   = p.unpackedConfig.exponentMin,
      mantissaWidth = p.unpackedConfig.mantissaWidth+2
    )
    val packPort = fpp.createPort(List(packAt), packParam)

    layer.lane.setDecodingDefault(SEL, False)
    def f2i(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(decodings)
      spec.addDecoding(SEL -> True)
      fup.unpack(uop, RS1)
      packPort.uopsAt += spec -> packAt
    }

    val f128 = FORMAT -> FpuFormat.QUAD
    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT
    val f16 = FORMAT -> FpuFormat.HALF

    f2i(Rvfd.FROUND_S  , f32, NX -> False)
    f2i(Rvfd.FROUNDNX_S, f32, NX -> True)
    if (Riscv.RVD) {
      f2i(Rvfd.FROUND_D  , f64, NX -> False)
      f2i(Rvfd.FROUNDNX_D, f64, NX -> True)
    }
    if (Riscv.RVZfh) {
      f2i(Rvfd.FROUND_H  , f16, NX -> False)
      f2i(Rvfd.FROUNDNX_H, f16, NX -> True)
    }
    if (Riscv.RVQ) {
      f2i(Rvfd.FROUND_Q  , f128, NX -> False)
      f2i(Rvfd.FROUNDNX_Q, f128, NX -> True)
    }

    uopLock.release()

    val RS1_FP = fup(RS1)

    val onShift = new layer.Execute(shiftAt) {
      val shiftFull = AFix(p.mantissaWidth) - RS1_FP.exponent
      val SHIFT = insert(U(shiftFull.raw).resize(log2Up(p.mantissaWidth + 1)))
      val ROUND = insert(Shift.rightWithScrap(True ## RS1_FP.mantissa.raw ## False, SHIFT - 1).takeLow(3))
    }

    val onRound = new layer.Execute(roundAt) {
      val sign = RS1_FP.sign
      val isNonZeroMan = RS1_FP.mantissa.raw.orR

      // exponent < 0
      val small = new Area {
        val SEL = RS1_FP.exponent.isNegative()
        val isHalf = RS1_FP.exponent === AFix(-1)
        val increment = ROUNDING.mux(
          FpuRoundMode.RNE -> (isHalf && isNonZeroMan),
          FpuRoundMode.RTZ -> False,
          FpuRoundMode.RDN -> sign,
          FpuRoundMode.RUP -> !sign,
          FpuRoundMode.RMM -> isHalf,
        )
      }

      // 0 <= exponent < mantissaWidth
      val normal = new Area {
        val SEL = !RS1_FP.exponent.isNegative() && RS1_FP.exponent < AFix(p.mantissaWidth)
        val round = this(onShift.ROUND)
        val discard = round(1) || round(0)
        val increment = ROUNDING.mux(
          FpuRoundMode.RNE -> (round(1) && (round(0) || round(2))),
          FpuRoundMode.RTZ -> False,
          FpuRoundMode.RDN -> (discard && sign),
          FpuRoundMode.RUP -> (discard && !sign),
          FpuRoundMode.RMM -> round(1),
        )

        val mask = U(p.mantissaWidth + 1 bits, default -> True) |<< onShift.SHIFT
        val fullMantissa = U(True ## RS1_FP.mantissa.raw) & mask
        val inc = U(1, p.mantissaWidth + 1 bits) |<< onShift.SHIFT
        val roundedMantissa = fullMantissa +^ inc.andMask(increment)
        val upflow = roundedMantissa.msb
      }

      val RESULT = Payload(FloatUnpacked(packParam))
      val FF_NX = insert(False)
      val FF_NV = insert(RS1_FP.isNanSignaling)

      RESULT := RS1_FP

      when(RS1_FP.isNan) {
        RESULT.setNanQuiet
      }

      when (RS1_FP.isNormal) {
        // exponent < 0
        when (small.SEL) {
          FF_NX := NX
          when (small.increment) {
            RESULT.exponent := AFix(0)
            RESULT.mantissa.raw := 0
          } otherwise {
            RESULT.setZero
          }
        }

        // 0 <= exponent < mantissaWidth
        when (normal.SEL) {
          FF_NX := NX && normal.discard
          when (normal.upflow) {
            RESULT.exponent := (RS1_FP.exponent + AFix(1)).sat(packParam.exponentMax, packParam.exponentMin)
          }
          RESULT.mantissa.raw := normal.roundedMantissa.takeLow(p.mantissaWidth) ## B(0, packParam.mantissaWidth - p.mantissaWidth bits)
        }

        // exponent >= mantissaWidth
        // as-is
      }
    }

    val onPack = new layer.Execute(packAt) {
      packPort.cmd.at(0) := isValid && SEL
      packPort.cmd.format := FORMAT
      packPort.cmd.roundMode := FpuRoundMode.RTZ
      packPort.cmd.hartId := Global.HART_ID
      packPort.cmd.uopId := Decode.UOP_ID
      packPort.cmd.value := onRound.RESULT
      packPort.cmd.flags.assign(NX = onRound.FF_NX, NV = onRound.FF_NV)
    }

    buildBefore.release()
  }
}
