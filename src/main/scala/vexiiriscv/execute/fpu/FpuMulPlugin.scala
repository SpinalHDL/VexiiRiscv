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


class FpuMulPlugin(val layer : LaneLayer,
                   withFma : Boolean = true,
                   fmaFullAccuracy : Boolean = true,
                   var expAt : Int = 0,
                   var normAt: Int = 3,
                   var packAt : Int = 3) extends FiberPlugin{
  val p = FpuUtils

  val SEL = Payload(Bool())
  val FMA = Payload(Bool())
  val SUB1 = Payload(Bool())
  val SUB2 = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fpp = host[FpuPackerPlugin]
    val fasp = host[FpuAddSharedPlugin]
    val mp  = host[MulReuse]
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fup.elaborationLock, fpp.elaborationLock, mp.mulLock)
    awaitBuild()
    mp.injectWidth(p.unpackedConfig.mantissaWidth + 2, p.unpackedConfig.mantissaWidth + 2, 2 * (p.unpackedConfig.mantissaWidth + 2))

    val packParam = FloatUnpackedParam(
      exponentMax   = p.unpackedConfig.exponentMax * 2 + 1,
      exponentMin   = p.unpackedConfig.exponentMin * 2,
      mantissaWidth = p.unpackedConfig.mantissaWidth + 2
    )

    val addParam = FloatUnpackedParam(
      exponentMax = p.unpackedConfig.exponentMax * 2 + 1,
      exponentMin = p.unpackedConfig.exponentMin * 2,
      mantissaWidth = fmaFullAccuracy.mux(p.unpackedConfig.mantissaWidth * 2 + 1, packParam.mantissaWidth)
    )

    val packPort = fpp.createPort(List(packAt), packParam)
    val addPort = withFma generate fasp.createPort(List(packAt), addParam, FpuUtils.unpackedConfig)


    layer.lane.setDecodingDefault(SEL, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(SEL -> True)
      spec.addDecoding(decodings)
      uop.resources.foreach {
        case RfResource(_, rs: RfRead) => fup.unpack(uop, rs)
        case _ =>
      }
      spec
    }

    def mul(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = add(uop, decodings: _*)
      spec.addDecoding(FMA -> False)
      packPort.uopsAt += (spec -> packAt)
    }

    def fma(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = add(uop, decodings: _*)
      spec.addDecoding(FMA -> True)
      addPort.uopsAt += (spec -> packAt)
    }

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    mul(Rvfd.FMUL_S, f32)
    if(Riscv.RVD) {
      mul(Rvfd.FMUL_D, f64)
    }

    if(withFma){
      fma(Rvfd.FMADD_S , f32, SUB1 -> False, SUB2 -> False)
      fma(Rvfd.FMSUB_S , f32, SUB1 -> False, SUB2 -> True )
      fma(Rvfd.FNMSUB_S, f32, SUB1 -> True , SUB2 -> False)
      fma(Rvfd.FNMADD_S, f32, SUB1 -> True , SUB2 -> True)
      if (Riscv.RVD) {
        fma(Rvfd.FMADD_D , f64, SUB1 -> False, SUB2 -> False)
        fma(Rvfd.FMSUB_D , f64, SUB1 -> False, SUB2 -> True)
        fma(Rvfd.FNMSUB_D, f64, SUB1 -> True , SUB2 -> False)
        fma(Rvfd.FNMADD_D, f64, SUB1 -> True , SUB2 -> True)
      }
    }


    uopLock.release()


    val RS1_FP = fup(RS1)
    val RS2_FP = fup(RS2)

    val calc = new layer.Execute(expAt){
      val EXP_ADD        = insert(RS1_FP.exponent + RS2_FP.exponent)
      val SIGN           = insert(RS1_FP.sign ^ RS2_FP.sign)
      val FORCE_ZERO     = insert(RS1_FP.isZero || RS2_FP.isZero)
      val FORCE_OVERFLOW = insert(RS1_FP.isInfinity || RS2_FP.isInfinity)
      val INFINITY_NAN   = insert(((RS1_FP.isInfinity || RS2_FP.isInfinity) && (RS1_FP.isZero || RS2_FP.isZero)))
      val FORCE_NAN      = insert(RS1_FP.isNan || RS2_FP.isNan || INFINITY_NAN)
    }

    import calc._


    val mulCmd = new layer.Execute(mp.cmdAt){
      val m1 = B"1" ## RS1_FP.mantissa.raw.asUInt
      val m2 = B"1" ## RS2_FP.mantissa.raw.asUInt
      when(SEL) {
        mp.inject(m1.resized, m2.resized)
      }
    }

    val mulRsp = new layer.Execute(mp.rspAt){
      val MUL_RESULT = insert(mp.rsp.take(widthOf(mulCmd.m1) + widthOf(mulCmd.m2)))
    }
    import mulRsp.MUL_RESULT

    val norm = new layer.Execute(normAt){
      val needShift = MUL_RESULT.msb
      val EXP = insert(EXP_ADD + AFix(U(needShift)))
      val MAN = insert(AFix(U(needShift ? MUL_RESULT.dropHigh(1) | (MUL_RESULT.dropHigh(2) << 1)), 1-widthOf(MUL_RESULT) exp))
    }
    import norm._

    val onPack = new layer.lane.Execute(packAt) {
      val nv = False

      val mode = FloatMode.NORMAL()
      when(FORCE_NAN) {
        mode := FloatMode.NAN
        nv setWhen ((INFINITY_NAN || RS1_FP.isNanSignaling || RS2_FP.isNanSignaling))
      }.elsewhen(FORCE_OVERFLOW) {
        mode := FloatMode.INF
      }.elsewhen(FORCE_ZERO) {
        mode := FloatMode.ZERO
      }

      packPort.cmd.at(0) := isValid && SEL && !FMA
      packPort.cmd.value.sign := SIGN
      packPort.cmd.value.exponent := EXP
      packPort.cmd.value.mantissa := MAN.rounded(RoundType.SCRAP)
      packPort.cmd.value.mode := mode
      packPort.cmd.value.quiet := True
      packPort.cmd.format := FORMAT
      packPort.cmd.roundMode := FpuUtils.ROUNDING
      packPort.cmd.hartId := Global.HART_ID
      packPort.cmd.uopId := Decode.UOP_ID
      packPort.cmd.flags.assign(NV = nv)

      if(withFma) {
        addPort.cmd.at(0) := isValid && SEL && FMA
        addPort.cmd.rs1.sign := SIGN ^ SUB1
        addPort.cmd.rs1.exponent := EXP
        addPort.cmd.rs1.mantissa := MAN.rounded(RoundType.FLOOR)
        addPort.cmd.rs1.mode := mode
        addPort.cmd.rs1.quiet := True
        addPort.cmd.rs2 := fup(RS3)
        addPort.cmd.rs2.sign.removeAssignments() := fup(RS3).sign ^ SUB2
        addPort.cmd.format := FORMAT
        addPort.cmd.roundMode := FpuUtils.ROUNDING
        addPort.cmd.hartId := Global.HART_ID
        addPort.cmd.uopId := Decode.UOP_ID
        addPort.cmd.flags.assign(NV = nv)
      }
    }

    buildBefore.release()
  }
}
