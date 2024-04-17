package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute._
import vexiiriscv.riscv._


class FpuMulPlugin(layer : LaneLayer, unpackAt : Int, packAt : Int) extends FiberPlugin{
  val p = FpuUtils

  val SEL = Payload(Bool())

  val logic = during setup new Area{
    val fup = host[FpuUnpackerPlugin]
    val fpp = host[FpuPackerPlugin]
    val buildBefore = retains(layer.el.pipelineLock)
    val uopLock = retains(layer.el.uopLock, fup.elaborationLock, fpp.elaborationLock)
    awaitBuild()

    val packParam = FloatUnpackedParam(
      exponentMax   = ???,
      exponentMin   = ???,
      mantissaWidth = ???
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
      wb.uops += uop
    }

    add(Rvfd.FMUL_S)
    if(Riscv.RVD) {
      add(Rvfd.FMUL_D)
    }

    uopLock.release()

    

    buildBefore.release()
  }
}
