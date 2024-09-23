package vexiiriscv.memory

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import vexiiriscv.Global
import vexiiriscv.Global._
import vexiiriscv.riscv.Riscv

import scala.collection.mutable.ArrayBuffer

trait AddressTranslationPortUsage
object AddressTranslationPortUsage{
  object FETCH extends AddressTranslationPortUsage
  object LOAD_STORE extends AddressTranslationPortUsage
}

case class AddressTranslationRefillCmd(storageWidth : Int) extends Bundle{
  val address = MIXED_ADDRESS()
  val storageId = UInt(storageWidth bits)
}

case class AddressTranslationRefillRsp() extends Bundle{
  val pageFault, accessFault = Bool()
}

case class AddressTranslationRefill(storageWidth : Int) extends Bundle{
  val cmd = Stream(AddressTranslationRefillCmd(storageWidth))
  val rsp = Flow(AddressTranslationRefillRsp())
}

case class AddressTranslationInvalidationCmd() extends Bundle {
  val hartId = HART_ID()
}

case class AddressTranslationInvalidation() extends Bundle {
  val cmd = Stream(AddressTranslationInvalidationCmd())
}

trait AddressTranslationService extends Area {
  def mayNeedRedo : Boolean
  val storageLock = Retainer()
  val portsLock = Retainer()
  def newStorage(pAny: Any, pmuEventId : Int): Any
  def getStorageId(s : Any) : Int
  def getStorageIdWidth() : Int
  def getSignExtension(kind : AddressTranslationPortUsage, rawAddress : UInt) : Bool

  val regionRetainer = Retainer()

  def newTranslationPort(nodes: Seq[NodeBaseApi],
                         rawAddress: Payload[UInt],
                         forcePhysical: Payload[Bool],
                         usage: AddressTranslationPortUsage,
                         portSpec: Any,
                         storageSpec: Any): AddressTranslationRsp

  val refillPorts = ArrayBuffer[AddressTranslationRefill]()
  def newRefillPort() = refillPorts.addRet(AddressTranslationRefill(getStorageIdWidth()))

  val invalidationPorts = ArrayBuffer[AddressTranslationInvalidation]()
  def newInvalidationPort() = invalidationPorts.addRet(AddressTranslationInvalidation())
}

class AddressTranslationRsp(s : AddressTranslationService, val wayCount : Int) extends Area{
  val keys = new Area {
    setName("MMU")
    val TRANSLATED = Payload(PHYSICAL_ADDRESS)
    val HAZARD = Payload(Bool())
    val REFILL = Payload(Bool())
    val ALLOW_READ, ALLOW_WRITE, ALLOW_EXECUTE = Payload(Bool())
    val PAGE_FAULT = Payload(Bool())
    val ACCESS_FAULT = Payload(Bool())
    val WAYS_OH  = Payload(Bits(wayCount bits))
    val WAYS_PHYSICAL  = Payload(Vec.fill(wayCount)(PHYSICAL_ADDRESS()))
    val BYPASS_TRANSLATION = Payload(Bool())
  }
}


trait DBusAccessService{
  def accessRefillCount : Int
  def accessWake: Bits
  def newDBusAccess() : DBusAccess = dbusAccesses.addRet(new DBusAccess(accessRefillCount))
  val dbusAccesses = ArrayBuffer[DBusAccess]()
  val accessRetainer = Retainer()
}

case class DBusAccess(refillCount : Int) extends Bundle {
  val cmd = Stream(DBusAccessCmd())
  val rsp = Flow(DBusAccessRsp(refillCount))
}

case class DBusAccessCmd() extends Bundle {
  val address = Global.PHYSICAL_ADDRESS()
  val size = UInt(2 bits)
}

case class DBusAccessRsp(refillCount : Int) extends Bundle {
  val data = Bits(Riscv.XLEN bits)
  val error = Bool()
  val redo = Bool()
  val waitSlot = Bits(refillCount bits)
  val waitAny  = Bool()
}

