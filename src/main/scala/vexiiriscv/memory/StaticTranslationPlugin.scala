package vexiiriscv.memory

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import vexiiriscv.Global._
import vexiiriscv.riscv.Riscv

import scala.collection.mutable.ArrayBuffer

/**
 * This plugin implement a dummy memory translation (for CPU without MMU).
 * That way, the plugins like the LSU/Fetch always have a MMU-like interface, which symplify their code.
 */

class StaticTranslationPlugin(var physicalWidth: Int) extends FiberPlugin with AddressTranslationService {
  override def mayNeedRedo: Boolean = false
  override def newStorage(pAny: Any, pmuStorageId : Int): Any = { }
  override def getStorageId(s: Any): Int = 0
  override def getStorageIdWidth(): Int = 0

  case class PortSpec(stages: Seq[NodeBaseApi],
                      req: AddressTranslationReq,
                      usage: AddressTranslationPortUsage,
                      rsp: AddressTranslationRsp)

  val portSpecs = ArrayBuffer[PortSpec]()
  override def newTranslationPort(stages: Seq[NodeBaseApi],
                                  req: AddressTranslationReq,
                                  usage: AddressTranslationPortUsage,
                                  portSpec: Any,
                                  storageSpec: Any): AddressTranslationRsp = {
    portSpecs.addRet(
      new PortSpec(
        stages = stages,
        req = req,
        usage = usage,
        rsp = new AddressTranslationRsp(this, 0)
      )
    ).rsp
  }

  override def getSignExtension(kind: AddressTranslationPortUsage, rawAddress: UInt): Bool = False

  val logic = during build new Area {
    PHYSICAL_WIDTH.set(physicalWidth)
    VIRTUAL_WIDTH.set(physicalWidth)
    MIXED_WIDTH.set(physicalWidth)
    PC_WIDTH.set(physicalWidth)
    TVAL_WIDTH.set(physicalWidth)

    portsLock.await()
    assert(refillPorts.isEmpty)

    regionRetainer.await()
    val ports = for(spec <- portSpecs) yield new Area{
      val node = spec.stages.last

      import node._
      import spec.rsp.keys._

      // Implement a pass through
      REFILL := False
      HAZARD := False
      TRANSLATED := spec.req.PRE_ADDRESS.resized //PC RESIZED
      PAGE_FAULT := False
      ACCESS_FAULT := spec.req.PRE_ADDRESS.drop(physicalWidth) =/= 0
      BYPASS_TRANSLATION := True
    }
  }
}
