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
                   var expAt : Int = 0,
                   var normAt: Int = 3,
                   var packAt : Int = 4) extends FiberPlugin{
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
      exponentMax   = p.unpackedConfig.exponentMax*2+1,
      exponentMin   = p.unpackedConfig.exponentMin*2,
      mantissaWidth = p.unpackedConfig.mantissaWidth+2
    )
    val wb = fpp.createPort(packAt, packParam)

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

    add(Rvfd.FMUL_S, f32)
    if(Riscv.RVD) {
      add(Rvfd.FMUL_D, f64)
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
        assert(!(Riscv.RVD && XLEN.get == 32))
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

    val onPack = new layer.el.Execute(packAt) {
      val RESULT = Payload(FloatUnpacked(packParam))

      RESULT.sign := SIGN
      RESULT.exponent := EXP
      RESULT.mantissa := MAN.rounded(RoundType.SCRAP)
      RESULT.mode := FloatMode.NORMAL
      RESULT.quiet := True

      val NV = insert(False)

      when(FORCE_NAN) {
        RESULT.setNanQuiet
        NV setWhen (INFINITY_NAN || RS1_FP.isNanSignaling || RS2_FP.isNanSignaling)
      }.elsewhen(FORCE_OVERFLOW) {
        RESULT.setInfinity
      }.elsewhen(FORCE_ZERO) {
        RESULT.setZero
      }

      wb.cmd.valid := isValid && SEL
      wb.cmd.value := RESULT
      wb.cmd.format := FORMAT
      wb.cmd.roundMode := FpuUtils.ROUNDING
      wb.cmd.hartId := Global.HART_ID
      wb.cmd.uopId := Decode.UOP_ID
    }



    buildBefore.release()
  }
}
