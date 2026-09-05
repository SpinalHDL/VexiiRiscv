package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.FORMAT
import vexiiriscv.riscv._


/**
 * Implement the RISC-V floating point <> integer binary casts
 */
class FpuMvPlugin(val layer : LaneLayer,
                   var intWbAt: Int = 1, //Relax int ALU
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
    val f16 = FORMAT -> FpuFormat.HALF

    add(Rvfd.FMV_W_X, f32, SEL_FLOAT -> True)
    add(Rvfd.FMV_X_W, f32, SEL_INT   -> True)
    if (Riscv.XLEN.get == 64) {
      iwbp.signExtend(iwb, layer(Rvfd.FMV_X_W), 32)
      if (Riscv.RVD) {
        add(Rvfd.FMV_D_X, f64, SEL_FLOAT -> True)
        add(Rvfd.FMV_X_D, f64, SEL_INT -> True)
      }
    }
    if (Riscv.RVZfh) {
      add(Rvfd.FMV_H_X, f16, SEL_FLOAT -> True)
      add(Rvfd.FMV_X_H, f16, SEL_INT -> True)
      iwbp.signExtend(iwb, layer(Rvfd.FMV_X_H), 16)
    }

    uopLock.release()

    val onIntWb = new layer.Execute(intWbAt) {
      iwb.valid   := SEL_INT
      iwb.payload := up(layer.lane(FloatRegFile, RS1)).resized
    }

    val onFloatWb = new layer.Execute(floatWbAt) {
      fwb.valid := SEL_FLOAT
      val value = fwb.payload.getAllTrue
      // For simplicity, let allow override this when XLEN = FLEN
      value.allowOverride()

      val rs1 = up(layer.lane(IntRegFile, RS1))

      p.whenFormat(FORMAT) {
        case FpuFormat.FLOAT => value(31 downto 0) := rs1(31 downto 0)
        case FpuFormat.DOUBLE if Riscv.XLEN.get == 64 => value(63 downto 0) := rs1(63 downto 0)
        case FpuFormat.HALF => value(15 downto 0) := rs1(15 downto 0)
      }

      fwb.payload := value
    }

    buildBefore.release()
  }
}
