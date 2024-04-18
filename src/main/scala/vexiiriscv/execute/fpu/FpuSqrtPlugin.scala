package vexiiriscv.execute.fpu

import spinal.core.{out, _}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.FORMAT
import vexiiriscv.riscv.Riscv.XLEN
import vexiiriscv.riscv._


class FpuSqrtPlugin(val layer : LaneLayer,
                    var exeAt : Int = 0) extends FiberPlugin{
  val p = FpuUtils

  val SEL = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fpp = host[FpuPackerPlugin]
    val mp  = host[MulReuse]
    val buildBefore = retains(layer.el.pipelineLock)
    val uopLock = retains(layer.el.uopLock, fup.elaborationLock, fpp.elaborationLock)
    awaitBuild()

    val packParam = FloatUnpackedParam(
      exponentMax   = p.unpackedConfig.exponentMax/2,
      exponentMin   = p.unpackedConfig.exponentMin/2,
      mantissaWidth = p.unpackedConfig.mantissaWidth+2
    )
    val wb = fpp.createPort(exeAt, packParam)

    layer.el.setDecodingDefault(SEL, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(SEL -> True)
      spec.addDecoding(decodings)
      uop.resources.foreach {
        case RfResource(_, rs: RfRead) => fup.unpack(uop, rs)
        case _ =>
      }
      wb.uops += spec
    }

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    add(Rvfd.FSQRT_S, f32)
    if(Riscv.RVD) {
      add(Rvfd.FSQRT_D, f64)
    }

    uopLock.release()


    val RS1_FP = fup(RS1)
    val internalMantissaSize = p.mantissaWidth
    val sqrt = FpuSqrt(internalMantissaSize)

    val onExecute = new layer.Execute(exeAt) {
      val cmdSent = RegInit(False) setWhen (sqrt.io.input.fire) clearWhen (isReady)
      sqrt.io.input.valid := isValid && SEL && !cmdSent
      sqrt.io.input.a := U(RS1_FP.exponent.raw.lsb ? (B"1" ## RS1_FP.mantissa ## B"0") | (B"01" ## RS1_FP.mantissa))
      sqrt.io.flush := isReady
      sqrt.io.output.ready := False

      val unscheduleRequest = RegNext(isCancel) clearWhen (isReady) init (False)
      val freeze = isValid && SEL && !sqrt.io.output.valid & !unscheduleRequest
      layer.el.freezeWhen(freeze)

      val exp = (RS1_FP.exponent >>| 1)
      val scrap = sqrt.io.output.remain =/= 0

      wb.cmd.valid := isValid && SEL
      wb.cmd.format := FORMAT
      wb.cmd.roundMode := FpuUtils.ROUNDING
      wb.cmd.hartId := Global.HART_ID
      wb.cmd.uopId := Decode.UOP_ID

      wb.cmd.value.setNormal
      wb.cmd.value.quiet := False
      wb.cmd.value.sign := RS1_FP.sign
      wb.cmd.value.exponent := exp
      wb.cmd.value.mantissa.raw := sqrt.io.output.result ## scrap
      val negative = !RS1_FP.isNan && !RS1_FP.isZero && RS1_FP.sign
      when(RS1_FP.isInfinity) {
        wb.cmd.value.setInfinity
      }

      val NV = False //TODO FPU FLAG
      when(negative) {
        wb.cmd.value.setNanQuiet
        NV := True
      }
      when(RS1_FP.isNan) {
        wb.cmd.value.setNanQuiet
        NV := !RS1_FP.quiet
      }
      when(RS1_FP.isZero) {
        wb.cmd.value.setZero
      }
    }

    buildBefore.release()
  }
}
