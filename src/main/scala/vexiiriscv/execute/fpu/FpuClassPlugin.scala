package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.{FORMAT, muxDouble}
import vexiiriscv.riscv.Riscv.RVC
import vexiiriscv.riscv._


class FpuClassPlugin(val layer : LaneLayer,
                     var wbAt : Int = 1 //Relax int ALU
                    ) extends FiberPlugin{
  val p = FpuUtils
  val SEL = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val iwbp = host.find[IntFormatPlugin](p => p.lane == layer.lane)
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fup.elaborationLock, iwbp.elaborationLock)
    awaitBuild()

    val iwb = iwbp.access(wbAt)

    layer.lane.setDecodingDefault(SEL, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(decodings)
      spec.addDecoding(SEL -> True)
      iwbp.addMicroOp(iwb, spec)
      fup.unpack(uop, RS1)
    }

    add(Rvfd.FCLASS_S, FORMAT -> FpuFormat.FLOAT)
    if(Riscv.RVD.get){
      add(Rvfd.FCLASS_D, FORMAT -> FpuFormat.DOUBLE)
    }

    uopLock.release()

    val RS1_FP = fup(RS1)

    val onWb = new layer.Execute(wbAt) {
      val fclassResult = B(0, 10 bits)
      val expSubnormal = muxDouble[SInt](FORMAT)(-1023)(-127)
      val RS1_FP_SUBNORMAL = fup.getSubnormal(RS1)
      fclassResult(0) := RS1_FP.sign && RS1_FP.isInfinity
      fclassResult(1) := RS1_FP.sign && RS1_FP.isNormal && !RS1_FP_SUBNORMAL
      fclassResult(2) := RS1_FP.sign && RS1_FP.isNormal && RS1_FP_SUBNORMAL
      fclassResult(3) := RS1_FP.sign && RS1_FP.isZero
      fclassResult(4) := !RS1_FP.sign && RS1_FP.isZero
      fclassResult(5) := !RS1_FP.sign && RS1_FP.isNormal && RS1_FP_SUBNORMAL
      fclassResult(6) := !RS1_FP.sign && RS1_FP.isNormal && !RS1_FP_SUBNORMAL
      fclassResult(7) := !RS1_FP.sign && RS1_FP.isInfinity
      fclassResult(8) := RS1_FP.isNan && !RS1_FP.quiet
      fclassResult(9) := RS1_FP.isNan && RS1_FP.quiet

      iwb.valid := SEL
      iwb.payload := fclassResult.resized
    }

    buildBefore.release()
  }
}
