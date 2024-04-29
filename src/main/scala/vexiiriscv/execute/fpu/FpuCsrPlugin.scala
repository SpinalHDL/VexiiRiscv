package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.misc.InflightService
import vexiiriscv.regfile.{RegFileWriter, RegFileWriterService, RegfileService}
import vexiiriscv.riscv._


object FpuCsrPlugin extends AreaObject {
  val DIRTY = Payload(Bool())
}

trait FpuDirtyService{
  def gotDirty() : Bool
}

class FpuCsrPlugin(lanes : Seq[ExecuteLaneService], dirtyAt : Int) extends FiberPlugin with FpuDirtyService {
  val api = during build new Area{
    val rm = Reg(Bits(3 bits)) init (0)
    val flags = Reg(FpuFlags())
    val gotDirty = False
  }

  override def gotDirty(): Bool = api.gotDirty

  val logic = during setup new Area{
    val cp = host[CsrService]
    val buildBefore = retains(lanes.map(_.pipelineLock) :+ cp.csrLock)
    val uopLock = retains(lanes.map(_.uopLock))
    awaitBuild()

    lanes.foreach(_.setDecodingDefault(FpuCsrPlugin.DIRTY, False))
    uopLock.release()

    assert(Global.HART_COUNT.get == 1)
    api.flags.NV init (False)
    api.flags.DZ init (False)
    api.flags.OF init (False)
    api.flags.UF init (False)
    api.flags.NX init (False)

    cp.readWrite(CSR.FCSR, 5 -> api.rm)
    cp.readWrite(CSR.FCSR, 0 -> api.flags)
    cp.readWrite(CSR.FRM, 0 -> api.rm)
    cp.readWrite(CSR.FFLAGS, 0 -> api.flags)

    for(lane <- lanes) new lane.Execute(0){
      val instrRounding = Decode.UOP(Const.funct3Range)
      FpuUtils.ROUNDING.assignFromBits((instrRounding === B"111").mux(api.rm, instrRounding))
    }
    for (lane <- lanes) new lane.Execute(dirtyAt) {
      api.gotDirty setWhen (isValid && isReady && !isCancel && Global.COMMIT && FpuCsrPlugin.DIRTY)
    }

    val csrDirty = CsrListFilter(List(CSR.FRM, CSR.FCSR, CSR.FFLAGS))
    cp.onWrite(csrDirty, true) {
      api.gotDirty := True
    }

    //Flush the pipeline if the rounding mode changed
    cp.trapNextOnWrite += CsrListFilter(List(CSR.FRM, CSR.FCSR))

    buildBefore.release()
  }
}
