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

case class AddressTranslationRefillCmdPerm() extends Bundle{
  val read = Bool()
  val write = Bool()
  val execute = Bool()
}

case class AddressTranslationRefillCmd(storageWidth : Int, addressWidth : Int) extends Bundle{
  val address = UInt(addressWidth bits)
  /*
   * For first-stage MMU:
   *    false : this request is a one-stage translation.
   *    true  : this request is a two-stage translation.
   *
   * For shadow (second-stage) MMU
   *    false : this request is for explicit memory access.
   *    true  : this request is for implicit memory access.
   */
  val indirect = Bool()
  val storageId = UInt(storageWidth bits)
  val storageEnable = Bool()

  val permission = AddressTranslationRefillCmdPerm()
}

case class AddressTranslationRefillRsp(addressWidth : Int) extends Bundle{
  val pageFault, accessFault, guestFault = Bool()

  val bypass = Bool()

  val ae_ptw = Bool()
  val ae_final = Bool()

  val pf  = Bool()
  val gf  = Bool()
  val hr  = Bool()
  val hw  = Bool()
  val hx  = Bool()

  val pte = new Bundle{
    val ppn = UInt(addressWidth - 12 bits)
    val flags = MmuEntryFlags()
  }

  val address = UInt(addressWidth bits)

  val level = UInt(2 bits)
}

/**
 * Interface used by the TrapPlugin to ask the MMU's page walker to do work
 */
case class AddressTranslationRefill(storageWidth : Int, requestWidth : Int, translatedWidth : Int) extends Bundle{
  val cmd = Stream(AddressTranslationRefillCmd(storageWidth, requestWidth))
  val rsp = Stream(AddressTranslationRefillRsp(translatedWidth))

  cmd.payload.setName("bits")
  rsp.payload.setName("bits")
}

case class AddressTranslationInvalidationParam(
  asidWidth: Int = 0,
  requestAddress: Boolean = false,
  requestGuest: Boolean = false
)

case class AddressTranslationInvalidationCmd(p: AddressTranslationInvalidationParam) extends Bundle {
  def withAnyAsid = p.asidWidth > 0
  def withAnyAddress = p.requestAddress

  val hartId = HART_ID()
  val asid = Bits(p.asidWidth bits)
  val address = p.requestAddress generate MIXED_ADDRESS()
  val guest = p.requestGuest generate Bool()
  val anyAddress = p.requestAddress generate Bool()
  val anyAsid = withAnyAsid generate Bool()
  /* For reset */
  val force = Bool()
}

/**
 * Used by the TrapPlugin to ask the MmuPlugin to invalidate its TLB (on SFENCE.VMA)
 */
case class AddressTranslationInvalidation(p: AddressTranslationInvalidationParam) extends Bundle {
  val cmd = Stream(AddressTranslationInvalidationCmd(p))
}

/**
 * Implemented by the MmuPlugin, allows other plugins to create new address translation interfaces
 */
trait AddressTranslationService extends Area {
  def isShadowMmu : Boolean
  def mayNeedRedo : Boolean
  def requestWidth : Int
  def translatedWidth : Int
  val storageLock = Retainer()
  val portsLock = Retainer()
  def newStorage(pAny: Any, pmuEventId : Int): Any
  def getStorageId(s : Any) : Int
  def getStorageIdWidth() : Int
  def getSignExtension(kind : AddressTranslationPortUsage, rawAddress : UInt) : Bool
  def getInvalidationPortParam : AddressTranslationInvalidationParam

  val regionRetainer = Retainer()

  // New Address translation interfaces are directly bound into a provided pipeline (nodes)
  def newTranslationPort(nodes: Seq[NodeBaseApi],
                         req: AddressTranslationReq,
                         usage: AddressTranslationPortUsage,
                         portSpec: Any,
                         storageSpec: Any): AddressTranslationRsp

  val refillPorts = ArrayBuffer[AddressTranslationRefill]()
  def newRefillPort() = refillPorts.addRet(AddressTranslationRefill(getStorageIdWidth(), requestWidth, translatedWidth))

  val invalidationPorts = ArrayBuffer[AddressTranslationInvalidation]()
  def newInvalidationPort() = invalidationPorts.addRet(AddressTranslationInvalidation(getInvalidationPortParam))
}

case class AddressTranslationReq(
  PRE_ADDRESS: Payload[UInt],
  LOAD: Payload[Bool],
  STORE: Payload[Bool],
  EXECUTE: Payload[Bool],
  FORCE_GUEST: Payload[Bool],
  FORCE_PHYSICAL: Payload[Bool]
)

class AddressTranslationRsp(s : AddressTranslationService, val wayCount : Int) extends Area {
  val keys = new Area {
    // setName("MMU")
    val TRANSLATED = Payload(UInt(s.translatedWidth bits))
    val HAZARD = Payload(Bool())
    val REFILL = Payload(Bool())
    val PAGE_FAULT = Payload(Bool())
    val ACCESS_FAULT = Payload(Bool())
    val WAYS_OH  = Payload(Bits(wayCount bits))
    val WAYS_PHYSICAL  = Payload(Vec.fill(wayCount)(UInt(s.translatedWidth bits)))
    val BYPASS_TRANSLATION = Payload(Bool())
    val ADDRESS_EXTENSION = Payload(Bool())
  }
}

trait PmpService extends Area {
  val portsLock = Retainer()
  def createPmpPort(nodes: Seq[NodeBaseApi],
                    physicalAddress: Payload[UInt],
                    forceCheck: NodeBaseApi => Bool,
                    read: NodeBaseApi => Bool,
                    write: NodeBaseApi => Bool,
                    execute: NodeBaseApi => Bool,
                    portSpec: Any,
                    storageSpec: Any): PmpRsp
  def getPmpNum() : Int
}

class PmpRsp extends Area{
  val ACCESS_FAULT = Payload(Bool())
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

/*
 * Two-stage translation abstract
 */
trait TranslatedDBusAccessService{
  def newDBusAccess(requestGuest: Boolean) : TranslatedDBusAccess = dbusAccesses.addRet(new TranslatedDBusAccess(requestGuest))
  val dbusAccesses = ArrayBuffer[TranslatedDBusAccess]()
  val accessRetainer = Retainer()
}

case class TranslatedDBusAccess(requestGuest : Boolean) extends Bundle {
  val cmd = Stream(TranslatedDBusAccessCmd(requestGuest))
  val rsp = Flow(TranslatedDBusAccessRsp())
}

case class TranslatedDBusAccessCmd(requestGuest : Boolean) extends Bundle {
  val addressWidth = Global.PHYSICAL_WIDTH.get max requestGuest.mux(Global.VIRTUAL_WIDTH.get + 2, Global.PHYSICAL_WIDTH.get)
  val address = UInt(addressWidth bits)
  val guest = requestGuest generate Bool()
  val size = UInt(2 bits)
}

case class TranslatedDBusAccessRsp() extends Bundle {
  val data = Bits(Riscv.XLEN bits)
  val error = Bits(2 bits)
}
