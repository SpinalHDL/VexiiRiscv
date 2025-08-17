package vexiiriscv.misc

import spinal.core.{Bool, _}
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrCondFilter, CsrListFilter, CsrRamPlugin, CsrRamService}
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._
import vexiiriscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IndirectCsrPlugin(val withSupervisor : Boolean) extends FiberPlugin {
  val logic = during setup new Area {
    val cap = host[CsrAccessPlugin]
    val buildBefore = retains(cap.csrLock)

    awaitBuild()

    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val api = cap.hart(hartId)

      val m = new Area {
        val iselect = RegInit(U(0, XLEN bits))
        api.readWrite(iselect, CSR.MISELECT)

        val iregs = for (i <- 0 until 6) yield (i + CSR.MIREG)

        def csrFilter(indirectId: Int, targetCsr: Int, cond: Bool = True): CsrCondFilter = {
          assert(iregs.contains(targetCsr), s"${targetCsr} is not an indirect CSR alias")

          CsrCondFilter(targetCsr, (iselect === indirectId) && cond)
        }
      }

      val s = withSupervisor generate new Area {
        val iselect = RegInit(U(0, XLEN bits))
        api.readWrite(iselect, CSR.SISELECT)

        val iregs = for (i <- 0 until 6) yield (i + CSR.SIREG)

        def csrFilter(indirectId: Int, targetCsr: Int, cond: Bool = True): CsrCondFilter = {
          assert(iregs.contains(targetCsr), s"${targetCsr} is not an indirect CSR alias")

          CsrCondFilter(targetCsr, (iselect === indirectId) && cond)
        }
      }
    }

    buildBefore.release()
  }
}
