package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.FORMAT
import vexiiriscv.riscv._


class FpuCmpPlugin(val layer : LaneLayer,
                   var cmpAt : Int = 0,
                   var intWbAt: Int = 1,
                   var floatWbAt: Int = 1) extends FiberPlugin{
  val p = FpuUtils

  val SEL_FLOAT = Payload(Bool())
  val SEL_INT = Payload(Bool())
  val LESS = Payload(Bool())
  val EQUAL = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fwbp = host.find[FpuPackerPlugin](p => p.lane == layer.el)
    val iwbp = host.find[IntFormatPlugin](p => p.laneName == layer.laneName)
    val buildBefore = retains(layer.el.pipelineLock)
    val uopLock = retains(layer.el.uopLock, fup.elaborationLock, fwbp.elaborationLock, iwbp.elaborationLock)
    awaitBuild()
    
    val packPort = fwbp.createPort(List(floatWbAt), FpuUtils.unpackedConfig)
    val iwb = iwbp.access(intWbAt)

    layer.el.setDecodingDefault(SEL_FLOAT, False)
    layer.el.setDecodingDefault(SEL_INT, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(decodings)
      uop.resources.foreach {
        case RfResource(_, rs: RfRead) => fup.unpack(uop, rs)
        case RfResource(rf, rs: RfWrite) if rf == FloatRegFile =>
          spec.addDecoding(SEL_FLOAT -> True)
          packPort.uopsAt += spec -> floatWbAt
        case RfResource(rf, rs: RfWrite) if rf == IntRegFile =>
          spec.addDecoding(SEL_INT -> True)
          iwbp.addMicroOp(iwb, spec)
        case _ =>
      }
    }

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    add(Rvfd.FMIN_S, f32, LESS -> True)
    add(Rvfd.FMAX_S, f32, LESS -> False)
    add(Rvfd.FLE_S , f32, EQUAL -> True , LESS -> True)
    add(Rvfd.FEQ_S , f32, EQUAL -> True , LESS -> False)
    add(Rvfd.FLT_S , f32, EQUAL -> False, LESS -> True)

    if(Riscv.RVD) {
      add(Rvfd.FMIN_D, f64, LESS -> True)
      add(Rvfd.FMAX_D, f64, LESS -> False)
      add(Rvfd.FLE_D , f64, EQUAL -> True , LESS -> True )
      add(Rvfd.FEQ_D , f64, EQUAL -> True , LESS -> False)
      add(Rvfd.FLT_D , f64, EQUAL -> False, LESS -> True )
    }

    uopLock.release()

    val RS1_FP = fup(RS1)
    val RS2_FP = fup(RS2)

    val onCmp = new layer.Execute(cmpAt) {
      val signalQuiet = SEL_INT && !LESS
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
      val MIN_MAX_QUIET = insert(RS1_FP.isNan && RS2_FP.isNan)
      val CMP_RESULT = insert(rs1Smaller && !bothZero && LESS || (rs1Equal || bothZero) && EQUAL)
      when(RS1_FP.isNan || RS2_FP.isNan) {
        CMP_RESULT := False
      }
    }

    val onIntWb = new layer.Execute(intWbAt) {
      iwb.valid := SEL_INT
      iwb.payload := onCmp.CMP_RESULT.asBits.resized //TODO NV !!
    }

    val onFloatWb = new layer.Execute(floatWbAt) {
      packPort.cmd.at(0) := isValid && SEL_FLOAT
      packPort.cmd.value := onCmp.MIN_MAX_RS2.mux[FloatUnpacked](RS2_FP, RS1_FP)
      when(!RS2_FP.quiet) { packPort.cmd.value.quiet := False }
      when(onCmp.MIN_MAX_QUIET) { packPort.cmd.value.setNanQuiet }
      packPort.cmd.format := FORMAT
      packPort.cmd.roundMode := FpuUtils.ROUNDING
      packPort.cmd.hartId := Global.HART_ID
      packPort.cmd.uopId := Decode.UOP_ID
      packPort.cmd.flags.assign(NV=onCmp.NV)
    }

    buildBefore.release()
  }
}
