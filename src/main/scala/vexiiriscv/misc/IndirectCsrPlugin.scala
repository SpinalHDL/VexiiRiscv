package vexiiriscv.misc

import spinal.core.{Bool, _}
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrCondFilter, CsrHartApi, CsrListFilter, CsrRamPlugin, CsrRamService, ExecuteLanePlugin, LaneLayer}
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._
import vexiiriscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class IndirectCsrApiParameters(
  api: CsrHartApi,
  iselectId: Int,
  iregIds: Seq[Int]
)

case class IndirectCsrApi(p: IndirectCsrApiParameters) extends Area {
  import p._

  val iselect = RegInit(U(0, XLEN bits))
  api.readWrite(iselect, iselectId)

  def csrCondFilter(selectCheck: (UInt) => Bool, targetCsr: Int, cond: Bool = True): CsrCondFilter = {
    assert(iregIds.contains(targetCsr), s"${targetCsr} is not an indirect CSR alias")

    CsrCondFilter(targetCsr, selectCheck(iselect) && cond)
  }

  def csrFilter(indirectId: Int, targetCsr: Int, cond: Bool = True) = csrCondFilter(iselect => iselect === indirectId, targetCsr, cond)

  def csrRangeFilter(indirectId: (Int, Int), targetCsr: Int, cond: Bool = True) = csrCondFilter(iselect => iselect >= indirectId._1 && iselect <= indirectId._2, targetCsr, cond)

  for (ireg <- iregIds) api.allowCsr(CsrCondFilter(ireg, False), True)
}

class IndirectCsrPlugin(val withSupervisor : Boolean, val withHypervisor : Boolean) extends FiberPlugin {
  val logic = during setup new Area {
    val cap = host[CsrAccessPlugin]
    val buildBefore = retains(cap.csrLock)

    awaitBuild()

    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val api = cap.hart(hartId)

      val m = IndirectCsrApi(IndirectCsrApiParameters(
        api = api,
        iselectId = CSR.MISELECT,
        iregIds = Seq(CSR.MIREG, CSR.MIREG2, CSR.MIREG3, CSR.MIREG4, CSR.MIREG5, CSR.MIREG6)
      ))

      val s = withSupervisor generate IndirectCsrApi(IndirectCsrApiParameters(
        api = api,
        iselectId = CSR.SISELECT,
        iregIds = Seq(CSR.SIREG, CSR.SIREG2, CSR.SIREG3, CSR.SIREG4, CSR.SIREG5, CSR.SIREG6)
      ))

      val vs = withHypervisor generate IndirectCsrApi(IndirectCsrApiParameters(
        api = api,
        iselectId = CSR.VSISELECT,
        iregIds = Seq(CSR.VSIREG, CSR.VSIREG2, CSR.VSIREG3, CSR.VSIREG4, CSR.VSIREG5, CSR.VSIREG6)
      ))
    }

    buildBefore.release()
  }
}
