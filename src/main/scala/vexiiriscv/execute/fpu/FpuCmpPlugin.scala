package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.{FORMAT, muxDouble}
import vexiiriscv.riscv._


object FpuCmpFloatOp extends SpinalEnum{
  val MIN_MAX, SGNJ = newElement()
}

class FpuCmpPlugin(val layer : LaneLayer,
                   var cmpAt : Int = 0,
                   var intWbAt: Int = 1,
                   var floatWbAt: Int = 1) extends FiberPlugin{
  val p = FpuUtils

  val SEL_FLOAT = Payload(Bool())
  val SEL_CMP = Payload(Bool())
  val LESS = Payload(Bool())
  val EQUAL = Payload(Bool())
  val FLOAT_OP = Payload(FpuCmpFloatOp())
  val INVERT = Payload(Bool())
  val SGNJ_RS1 = Payload(Bool())


  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fwbp = host.find[WriteBackPlugin](p => p.lane == layer.el && p.rf == FloatRegFile)
    val iwbp = host.find[IntFormatPlugin](p => p.lane == layer.el)
    val ffwbp = host.find[FpuFlagsWritebackPlugin](p => p.lane == layer.el)
    val buildBefore = retains(layer.el.pipelineLock)
    val uopLock = retains(layer.el.uopLock, fup.elaborationLock, fwbp.elaborationLock, iwbp.elaborationLock, ffwbp.elaborationLock)
    awaitBuild()

    val ffwb = ffwbp.createPort(List(cmpAt))
    val fwb = fwbp.createPort(floatWbAt)
    val iwb = iwbp.access(intWbAt)

    layer.el.setDecodingDefault(SEL_FLOAT, False)
    layer.el.setDecodingDefault(SEL_CMP, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(decodings)
      uop.resources.foreach {
        case RfResource(_, rs: RfRead) => fup.unpack(uop, rs)
        case RfResource(rf, rs: RfWrite) if rf == FloatRegFile =>
          spec.addDecoding(SEL_FLOAT -> True)
          spec.setCompletion(floatWbAt)
          fwbp.addMicroOp(fwb, spec)
        case RfResource(rf, rs: RfWrite) if rf == IntRegFile =>
          spec.addDecoding(SEL_CMP -> True)
          iwbp.addMicroOp(iwb, spec)
        case _ =>
      }
    }

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    add(Rvfd.FSGNJ_S , f32, FLOAT_OP -> FpuCmpFloatOp.SGNJ, INVERT -> False, SGNJ_RS1 -> False)
    add(Rvfd.FSGNJN_S, f32, FLOAT_OP -> FpuCmpFloatOp.SGNJ, INVERT -> True , SGNJ_RS1 -> False)
    add(Rvfd.FSGNJX_S, f32, FLOAT_OP -> FpuCmpFloatOp.SGNJ, INVERT -> False, SGNJ_RS1 -> True )
    add(Rvfd.FMIN_S  , f32, FLOAT_OP -> FpuCmpFloatOp.MIN_MAX, LESS -> True)
    add(Rvfd.FMAX_S  , f32, FLOAT_OP -> FpuCmpFloatOp.MIN_MAX, LESS -> False)
    add(Rvfd.FLE_S   , f32, EQUAL -> True , LESS -> True)
    add(Rvfd.FEQ_S   , f32, EQUAL -> True , LESS -> False)
    add(Rvfd.FLT_S   , f32, EQUAL -> False, LESS -> True)

    if(Riscv.RVD) {
      add(Rvfd.FSGNJ_D , f64, FLOAT_OP -> FpuCmpFloatOp.SGNJ, INVERT -> False, SGNJ_RS1 -> False)
      add(Rvfd.FSGNJN_D, f64, FLOAT_OP -> FpuCmpFloatOp.SGNJ, INVERT -> True , SGNJ_RS1 -> False)
      add(Rvfd.FSGNJX_D, f64, FLOAT_OP -> FpuCmpFloatOp.SGNJ, INVERT -> False, SGNJ_RS1 -> True)
      add(Rvfd.FMIN_D  , f64, FLOAT_OP -> FpuCmpFloatOp.MIN_MAX, LESS -> True)
      add(Rvfd.FMAX_D  , f64, FLOAT_OP -> FpuCmpFloatOp.MIN_MAX, LESS -> False)
      add(Rvfd.FLE_D   , f64, EQUAL -> True , LESS -> True )
      add(Rvfd.FEQ_D   , f64, EQUAL -> True , LESS -> False)
      add(Rvfd.FLT_D   , f64, EQUAL -> False, LESS -> True )
    }

    uopLock.release()

    val RS1_FP = fup(RS1)
    val RS2_FP = fup(RS2)

    val onCmp = new layer.Execute(cmpAt) {
      val signalQuiet = SEL_CMP && LESS
      val rs1NanNv = RS1_FP.isNan && (!RS1_FP.quiet || signalQuiet)
      val rs2NanNv = RS2_FP.isNan && (!RS2_FP.quiet || signalQuiet)
      val NV = insert(rs1NanNv || rs2NanNv)

      val bothZero = insert(RS1_FP.isZero && RS2_FP.isZero)
      val expEqual = insert(RS1_FP.exponent === RS2_FP.exponent)
      val rs1Equal = insert(RS1_FP.sign === RS2_FP.sign && expEqual && RS1_FP.mantissa === RS2_FP.mantissa)
      val rs1ExpSmaller = insert(RS1_FP.exponent < RS2_FP.exponent)
      val rs1MantissaSmaller = insert(RS1_FP.mantissa < RS2_FP.mantissa)

      val rs1AbsSmaller = rs1ExpSmaller || expEqual && rs1MantissaSmaller
      rs1AbsSmaller.setWhen(RS2_FP.isInfinity)
      rs1AbsSmaller.setWhen(RS1_FP.isZero)
      rs1AbsSmaller.clearWhen(RS2_FP.isZero)
      rs1AbsSmaller.clearWhen(RS1_FP.isInfinity)
      rs1Equal setWhen (RS1_FP.sign === RS2_FP.sign && RS1_FP.isInfinity && RS2_FP.isInfinity)
      val rs1Smaller = (RS1_FP.sign ## RS2_FP.sign).mux(
        0 -> rs1AbsSmaller,
        1 -> False,
        2 -> True,
        3 -> (!rs1AbsSmaller && !rs1Equal)
      )

      val MIN_MAX_RS2 = insert(!((rs1Smaller ^ !LESS) && !RS1_FP.isNan || RS2_FP.isNan))
      val CMP_RESULT = insert(rs1Smaller && !bothZero && LESS || (rs1Equal || bothZero) && EQUAL)
      when(RS1_FP.isNan || RS2_FP.isNan) {
        CMP_RESULT := False
      }

      val SGNJ_RESULT = insert((RS1_FP.sign && SGNJ_RS1) ^ RS2_FP.sign ^ INVERT)

      ffwb.ats(0) := (SEL_FLOAT && FLOAT_OP === FpuCmpFloatOp.MIN_MAX || SEL_CMP)
      ffwb.flags.assign(NV = NV)
    }

    val onIntWb = new layer.Execute(intWbAt) {
      iwb.valid := SEL_CMP
      iwb.payload := onCmp.CMP_RESULT.asBits.resized
    }

    val onFloatWb = new layer.Execute(floatWbAt) {
      fwb.valid := SEL_FLOAT
      fwb.payload := (FLOAT_OP === FpuCmpFloatOp.MIN_MAX && onCmp.MIN_MAX_RS2).mux(up(layer.el(FloatRegFile, RS2)), up(layer.el(FloatRegFile, RS1)))
      val doNan = RS1_FP.isNan && RS2_FP.isNan && FLOAT_OP === FpuCmpFloatOp.MIN_MAX
      val wb = fwb.payload
      when(doNan) {
        p.whenDouble(FORMAT)(wb(52, 11 bits).setAll())(wb(23, 8 bits).setAll())
        p.whenDouble(FORMAT)(wb(0, 52 bits).clearAll())(wb(0, 23 bits).clearAll())
        p.whenDouble(FORMAT)(wb(51) := True)(wb(22) := True)
        p.whenDouble(FORMAT)(wb(63) := False)(wb(31) := False)
        if (p.rvd) when(FORMAT === FpuFormat.FLOAT) {
          wb(63 downto 32).setAll()
        }
      }
      when(FLOAT_OP === FpuCmpFloatOp.SGNJ){
        p.whenDouble(FORMAT)(wb(63) := onCmp.SGNJ_RESULT)(wb(31) := onCmp.SGNJ_RESULT)
        if(Riscv.RVD) when(fup.getBadBoxing(RS1)){
          doNan := True
        }
      }
    }

    buildBefore.release()
  }
}
