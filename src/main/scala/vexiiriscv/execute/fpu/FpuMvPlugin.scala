package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.{FORMAT, muxDouble}
import vexiiriscv.riscv._



class FpuMvPlugin(val layer : LaneLayer,
                   var intWbAt: Int = 0,
                   var floatWbAt: Int = 0) extends FiberPlugin{
  val p = FpuUtils

  val SEL_FLOAT = Payload(Bool())
  val SEL_INT = Payload(Bool())

  val logic = during setup new Area{
    val fwbp = host.find[WriteBackPlugin](p => p.lane == layer.lane && p.rf == FloatRegFile)
    val iwbp = host.find[IntFormatPlugin](p => p.lane == layer.lane)
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fwbp.elaborationLock, iwbp.elaborationLock)
    awaitBuild()

    val fwb = fwbp.createPort(floatWbAt)
    val iwb = iwbp.access(intWbAt)

    layer.lane.setDecodingDefault(SEL_FLOAT, False)
    layer.lane.setDecodingDefault(SEL_INT, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(decodings)
      uop.resources.foreach {
        case RfResource(rf, rs: RfRead) =>
          spec.addRsSpec(rs, 0)
        case RfResource(rf, rs: RfWrite) if rf == FloatRegFile =>
          spec.addDecoding(SEL_FLOAT -> True)
          spec.setCompletion(floatWbAt)
          fwbp.addMicroOp(fwb, spec)
        case RfResource(rf, rs: RfWrite) if rf == IntRegFile =>
          spec.addDecoding(SEL_INT -> True)
          iwbp.addMicroOp(iwb, spec)
        case _ =>
      }
    }

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    add(Rvfd.FMV_W_X, f32, SEL_FLOAT -> True)
    add(Rvfd.FMV_X_W, f32, SEL_INT   -> True)
    if (Riscv.RVD && Riscv.XLEN.get == 64) {
      add(Rvfd.FMV_D_X, f64, SEL_FLOAT -> True)
      add(Rvfd.FMV_X_D, f64, SEL_INT   -> True)
      iwbp.signExtend(iwb, layer(Rvfd.FMV_X_W), 32)
    }

    uopLock.release()

    val onIntWb = new layer.Execute(intWbAt) {
      iwb.valid   := SEL_INT
      iwb.payload := up(layer.lane(FloatRegFile, RS1)).resized
    }

    val onFloatWb = new layer.Execute(floatWbAt) {
      fwb.valid := SEL_FLOAT
      fwb.payload(31 downto 0) := up(layer.lane(IntRegFile, RS1))(31 downto 0)
      if(Riscv.RVD.get) {
        fwb.payload(63 downto 32) := muxDouble(FORMAT)(up(layer.lane(IntRegFile, RS1))(63 downto 32))(B"xFFFFFFFF")
      }
    }

    buildBefore.release()
  }
}
