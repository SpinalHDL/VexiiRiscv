package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.FORMAT
import vexiiriscv.riscv.Riscv.XLEN
import vexiiriscv.riscv._


class FpuAddPlugin(val layer : LaneLayer,
                   addAt : Int = 0) extends FiberPlugin{
  val p = FpuUtils

  val SEL = Payload(Bool())
  val SUB = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fasp = host[FpuAddSharedPlugin]
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fasp.elaborationLock, fup.elaborationLock)
    awaitBuild()

    val addPort = fasp.createPort(List(addAt), FpuUtils.unpackedConfig, FpuUtils.unpackedConfig)

    layer.lane.setDecodingDefault(SEL, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(SEL -> True)
      spec.addDecoding(decodings)
      uop.resources.foreach {
        case RfResource(_, rs: RfRead) => fup.unpack(uop, rs)
        case _ =>
      }
      addPort.uopsAt += (spec -> addAt)
    }

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    add(Rvfd.FADD_S, f32, SUB -> False)
    add(Rvfd.FSUB_S, f32, SUB -> True )
    if(Riscv.RVD) {
      add(Rvfd.FADD_D, f64, SUB -> False)
      add(Rvfd.FSUB_D, f64, SUB -> True )
    }

    uopLock.release()


    val RS1_FP = fup(RS1)
    val RS2_FP = fup(RS2)

    val onAdd = new layer.Execute(addAt){
      addPort.cmd.at(0) := isValid && SEL
      addPort.cmd.rs1 := fup(RS1)
      addPort.cmd.rs2 := fup(RS2)
      addPort.cmd.rs2.sign.removeAssignments() := fup(RS2).sign ^ SUB
      addPort.cmd.format := FORMAT
      addPort.cmd.roundMode := FpuUtils.ROUNDING
      addPort.cmd.hartId := Global.HART_ID
      addPort.cmd.uopId := Decode.UOP_ID
      addPort.cmd.flags.clearAll()
    }

    buildBefore.release()
  }
}
