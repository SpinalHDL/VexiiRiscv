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

/**
 * Convert floating point 32 bits to/from 64 bits
 * packAt at 1 to reduce timing pressure (unpack -> pack)
 *
 * The FpuPackerPlugin will do the heavy work (rounding)
 */
class FpuXxPlugin(val layer : LaneLayer,
                  var packAt : Int = 1) extends FiberPlugin{
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

    if (p.rvd) {
      add(Rvfd.FCVT_D_S, FORMAT -> FpuFormat.FLOAT)
      add(Rvfd.FCVT_S_D, FORMAT -> FpuFormat.DOUBLE)
    }
    if (p.rvzfh) {
      add(Rvfd.FCVT_H_S, FORMAT -> FpuFormat.FLOAT)
      add(Rvfd.FCVT_S_H, FORMAT -> FpuFormat.HALF)
    }
    if (p.rvq) {
      add(Rvfd.FCVT_Q_S, FORMAT -> FpuFormat.FLOAT)
      add(Rvfd.FCVT_S_Q, FORMAT -> FpuFormat.QUAD)
    }
    if (p.rvd && p.rvzfh) {
      add(Rvfd.FCVT_H_D, FORMAT -> FpuFormat.DOUBLE)
      add(Rvfd.FCVT_D_H, FORMAT -> FpuFormat.HALF)
    }
    if (p.rvd && p.rvq) {
      add(Rvfd.FCVT_Q_D, FORMAT -> FpuFormat.DOUBLE)
      add(Rvfd.FCVT_D_Q, FORMAT -> FpuFormat.QUAD)
    }
    if (p.rvzfh && p.rvq) {
      add(Rvfd.FCVT_Q_H, FORMAT -> FpuFormat.HALF)
      add(Rvfd.FCVT_H_Q, FORMAT -> FpuFormat.QUAD)
    }
    uopLock.release()

    val RS1_FP = fup(RS1)

    val onPack = new layer.Execute(packAt) {
      val packFormat = FpuFormat()
      packFormat.assignFromBits(Decode.UOP(25, 2 bits))

      packPort.cmd.at(0) := isValid && SEL
      packPort.cmd.format := packFormat
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
