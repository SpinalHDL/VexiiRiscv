package vexiiriscv.memory

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import vexiiriscv.Global._

class StaticTranslationPlugin(var physicalWidth: Int,
                              var ioRange: UInt => Bool,
                              var fetchRange: UInt => Bool) extends FiberPlugin with AddressTranslationService {

  override def newStorage(pAny: Any): Any = { }
  override def newTranslationPort(nodes: Seq[NodeBaseApi],
                                  rawAddress: Payload[UInt],
                                  allowRefill: Payload[Bool],
                                  usage: AddressTranslationPortUsage,
                                  portSpec: Any,
                                  storageSpec: Any): AddressTranslationRsp = {
    new AddressTranslationRsp(this, 0, wayCount = 0) {
      val node = nodes.last
      import node._
      import keys._

      REDO := False
      TRANSLATED := rawAddress
      IO := ioRange(TRANSLATED)
      ALLOW_EXECUTE := True
      ALLOW_READ := True
      ALLOW_WRITE := True
      PAGE_FAULT := False
      ACCESS_FAULT := False
      wake := True
      ALLOW_EXECUTE clearWhen (!fetchRange(TRANSLATED))
    }
  }


  val logic = during build new Area {
    PHYSICAL_WIDTH.set(physicalWidth)
    VIRTUAL_WIDTH.set(physicalWidth)
    MIXED_WIDTH.set(physicalWidth)
    PC_WIDTH.set(physicalWidth)
    TVAL_WIDTH.set(physicalWidth)
  }
}