package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.FORMAT
import vexiiriscv.riscv._


class FpuDivPlugin(val layer : LaneLayer,
                   var exeAt : Int = 0) extends FiberPlugin{
  val p = FpuUtils

  val SEL = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fpp = host[FpuPackerPlugin]
    val dr  = host[DivReuse]
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fup.elaborationLock, fpp.elaborationLock)
    awaitBuild()

    val packParam = FloatUnpackedParam(
      exponentMax   = p.unpackedConfig.exponentMax-p.unpackedConfig.exponentMin,
      exponentMin   = p.unpackedConfig.exponentMin-p.unpackedConfig.exponentMax-1,
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

    add(Rvfd.FDIV_S, FORMAT -> FpuFormat.FLOAT)
    if(Riscv.RVD) {
      add(Rvfd.FDIV_D, FORMAT -> FpuFormat.DOUBLE)
    }

    uopLock.release()


    val RS1_FP = fup(RS1)
    val RS2_FP = fup(RS2)
    val internalMantissaSize = p.mantissaWidth
    val bitsNeeded = 1+internalMantissaSize+2+1
    val interations = 1+(bitsNeeded-1 + (log2Up(dr.divRadix)-1)) / log2Up(dr.divRadix)
    val pickAt = (log2Up(dr.divRadix) - ((bitsNeeded-1) % log2Up(dr.divRadix))) % log2Up(dr.divRadix)
    
    val onExecute = new layer.Execute(exeAt) {
      when(isValid && SEL && fup.unpackingDone(exeAt)) {
        dr.divInject(layer, exeAt, U(B"1" ## RS1_FP.mantissa.raw), U(B"1" ## RS2_FP.mantissa.raw), interations-1)
      }
      val DIVIDER_RSP = insert(dr.divRsp.result(pickAt, bitsNeeded bits) | U(dr.divRsp.remain.orR || dr.divRsp.result(0, pickAt bits).orR).resized)

      val needShift = !DIVIDER_RSP.msb
      val mantissa = needShift.mux(DIVIDER_RSP(0, internalMantissaSize+2 bits), DIVIDER_RSP(1, internalMantissaSize+2 bits) | U(DIVIDER_RSP(0)).resized)
      val exponent = RegNext(RS1_FP.exponent - RS2_FP.exponent) - AFix(U(needShift))


      packPort.cmd.at(0) := isValid && SEL
      packPort.cmd.value.setNormal
      packPort.cmd.value.quiet := False
      packPort.cmd.value.sign := RS1_FP.sign ^ RS2_FP.sign
      packPort.cmd.value.exponent := exponent
      packPort.cmd.value.mantissa := mantissa
      packPort.cmd.format := FORMAT
      packPort.cmd.roundMode := FpuUtils.ROUNDING
      packPort.cmd.hartId := Global.HART_ID
      packPort.cmd.uopId := Decode.UOP_ID

      val forceOverflow = RS1_FP.isInfinity || RS2_FP.isZero
      val infinitynan = RS1_FP.isZero && RS2_FP.isZero || RS1_FP.isInfinity && RS2_FP.isInfinity
      val forceNan = RS1_FP.isNan || RS2_FP.isNan || infinitynan
      val forceZero = RS1_FP.isZero || RS2_FP.isInfinity

      packPort.cmd.flags.NX := False
      packPort.cmd.flags.UF := False
      packPort.cmd.flags.OF := False
      packPort.cmd.flags.DZ := !forceNan && !RS1_FP.isInfinity && RS2_FP.isZero
      packPort.cmd.flags.NV := False

      when(forceNan) {
        packPort.cmd.value.setNanQuiet
        packPort.cmd.flags.NV setWhen((infinitynan || RS1_FP.isNanSignaling || RS2_FP.isNanSignaling))
      } elsewhen(forceOverflow) {
        packPort.cmd.value.setInfinity
      } elsewhen(forceZero) {
        packPort.cmd.value.setZero
      }
    }

    buildBefore.release()
  }
}
