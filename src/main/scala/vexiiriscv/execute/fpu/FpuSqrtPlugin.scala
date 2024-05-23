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
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fup.elaborationLock, fpp.elaborationLock)
    awaitBuild()

    val packParam = FloatUnpackedParam(
      exponentMax   = p.unpackedConfig.exponentMax/2,
      exponentMin   = p.unpackedConfig.exponentMin/2,
      mantissaWidth = p.unpackedConfig.mantissaWidth+2
    )
    val packPort = fpp.createPort(List(exeAt), packParam)

    layer.lane.setDecodingDefault(SEL, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(SEL -> True)
      spec.addDecoding(decodings)
      uop.resources.foreach {
        case RfResource(_, rs: RfRead) => fup.unpack(uop, rs)
        case _ =>
      }
      packPort.uopsAt += spec -> exeAt
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
      sqrt.io.input.valid := isValid && SEL && fup.unpackingDone(exeAt)&& !cmdSent
      sqrt.io.input.a := U(RS1_FP.exponent.raw.lsb ? (B"1" ## RS1_FP.mantissa ## B"0") | (B"01" ## RS1_FP.mantissa))
      sqrt.io.flush := isReady
      sqrt.io.output.ready := False

      val unscheduleRequest = RegNext(isCancel) clearWhen (isReady) init (False)
      val freeze = isValid && SEL && !sqrt.io.output.valid & !unscheduleRequest
      layer.lane.freezeWhen(freeze)

      val exp = (RS1_FP.exponent >>| 1)
      val scrap = sqrt.io.output.remain =/= 0

      packPort.cmd.at(0) := isValid && SEL
      packPort.cmd.format := FORMAT
      packPort.cmd.roundMode := FpuUtils.ROUNDING
      packPort.cmd.hartId := Global.HART_ID
      packPort.cmd.uopId := Decode.UOP_ID
      packPort.cmd.value.setNormal
      packPort.cmd.value.quiet := False
      packPort.cmd.value.sign := RS1_FP.sign
      packPort.cmd.value.exponent := exp
      packPort.cmd.value.mantissa.raw := sqrt.io.output.result ## scrap
      val negative = !RS1_FP.isNan && !RS1_FP.isZero && RS1_FP.sign
      when(RS1_FP.isInfinity) {
        packPort.cmd.value.setInfinity
      }

      val NV = False
      when(negative) {
        packPort.cmd.value.setNanQuiet
        NV := True
      }
      when(RS1_FP.isNan) {
        packPort.cmd.value.setNanQuiet
        NV := !RS1_FP.quiet
      }
      when(RS1_FP.isZero) {
        packPort.cmd.value.setZero
      }

      packPort.cmd.flags.assign(NV = NV)
    }

    buildBefore.release()
  }
}
