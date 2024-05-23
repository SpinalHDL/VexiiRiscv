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


class FpuXxPlugin(val layer : LaneLayer,
                  var packAt : Int = 0) extends FiberPlugin{
  val p = FpuUtils

  val SEL = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fpp = host[FpuPackerPlugin]
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fup.elaborationLock, fpp.elaborationLock)
    awaitBuild()

    val packParam = FloatUnpackedParam(
      exponentMax   = p.unpackedConfig.exponentMax,
      exponentMin   = p.unpackedConfig.exponentMin,
      mantissaWidth = p.unpackedConfig.mantissaWidth+2
    )
    val packPort = fpp.createPort(List(packAt), packParam)

    layer.lane.setDecodingDefault(SEL, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(SEL -> True)
      spec.addDecoding(decodings)
      uop.resources.foreach {
        case RfResource(_, rs: RfRead) => fup.unpack(uop, rs)
        case _ =>
      }
      packPort.uopsAt += spec -> packAt
    }

    assert(Riscv.RVD.get)
    add(Rvfd.FCVT_D_S, FORMAT -> FpuFormat.FLOAT)
    add(Rvfd.FCVT_S_D, FORMAT -> FpuFormat.DOUBLE)
    uopLock.release()

    val RS1_FP = fup(RS1)

    val onPack = new layer.Execute(packAt) {
      packPort.cmd.at(0) := isValid && SEL
      packPort.cmd.format :=  (FORMAT === FpuFormat.FLOAT).mux(FpuFormat.DOUBLE, FpuFormat.FLOAT)
      packPort.cmd.roundMode := FpuUtils.ROUNDING
      packPort.cmd.hartId := Global.HART_ID
      packPort.cmd.uopId := Decode.UOP_ID
      packPort.cmd.value := RS1_FP
      packPort.cmd.value.quiet.removeAssignments() := True
      packPort.cmd.flags.assign(NV = RS1_FP.isNan && !RS1_FP.quiet)
    }

    buildBefore.release()
  }
}
