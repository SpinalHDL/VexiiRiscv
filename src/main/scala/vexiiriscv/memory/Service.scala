package vexiiriscv.memory

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import vexiiriscv.Global

trait AddressTranslationPortUsage
object AddressTranslationPortUsage{
  object FETCH extends AddressTranslationPortUsage
  object LOAD_STORE extends AddressTranslationPortUsage
}

trait AddressTranslationService extends Area {
  val elaborationLock = Lock()
  def newStorage(pAny: Any): Any

  def newTranslationPort(nodes: Seq[NodeBaseApi],
                         rawAddress: Payload[UInt],
                         allowRefill: Payload[Bool],
                         usage: AddressTranslationPortUsage,
                         portSpec: Any,
                         storageSpec: Any): AddressTranslationRsp
}

class AddressTranslationRsp(s : AddressTranslationService, wakesCount : Int, val wayCount : Int) extends Area{
  val keys = new Area {
    setName("MMU")
    val TRANSLATED = Payload(UInt(Global.PHYSICAL_WIDTH bits))
    val IO = Payload(Bool())
    val REDO = Payload(Bool())
    val ALLOW_READ, ALLOW_WRITE, ALLOW_EXECUTE = Payload(Bool())
    val PAGE_FAULT = Payload(Bool())
    val ACCESS_FAULT = Payload(Bool())
    val WAYS_OH  = Payload(Bits(wayCount bits))
    val WAYS_PHYSICAL  = Payload(Vec.fill(wayCount)(UInt(Global.PHYSICAL_WIDTH bits)))
    val BYPASS_TRANSLATION = Payload(Bool())
  }
  val wake = Bool()
  val pipelineLock = Lock().retain()
}
