package vexiiriscv.execute

import spinal.core.fiber.{Handle, Lock}
import spinal.core._
import spinal.lib._
import vexiiriscv.Global

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class CsrSpec(val csrFilter : Any)
case class CsrIsReadingCsr(override val csrFilter : Any, value : Bool) extends CsrSpec(csrFilter)
case class CsrOnRead (override val csrFilter : Any, onlyOnFire : Boolean, body : () => Unit) extends CsrSpec(csrFilter)
case class CsrOnReadToWrite (override val csrFilter : Any, body : () => Unit) extends CsrSpec(csrFilter) //Allow the fancy supervisor external interrupt logic
case class CsrOnWrite(override val csrFilter : Any, onlyOnFire : Boolean, body : () => Unit) extends CsrSpec(csrFilter)
case class CsrOnDecode (override val csrFilter : Any, priority : Int, body : () => Unit) extends CsrSpec(csrFilter)
case class CsrRamSpec(override val csrFilter : Any, alloc : CsrRamAllocation) extends CsrSpec(csrFilter)
case class CsrWriteCancel(override val csrFilter : Any, cond : Bool) extends CsrSpec(csrFilter)

case class CsrOnReadData (bitOffset : Int, value : Bits)
case class CsrIsReadingHartId(hartId : Int, value : Bool)


case class CsrListFilter(mapping : Seq[Int]) extends Nameable
trait CsrService {
  val csrLock = Lock()
  val spec = ArrayBuffer[CsrSpec]()
  val reads = ArrayBuffer[CsrOnReadData]()
  val isReadingCsrMap = mutable.LinkedHashMap[Any, CsrIsReadingCsr]()
  val onReadingHartIdMap = mutable.LinkedHashMap[Int, Bool]()
  val isReadingHartIdCsrMap = mutable.LinkedHashMap[(Int, Any), Bool]()
  val onWritingHartIdMap = mutable.LinkedHashMap[Int, Bool]()

  def onDecode(csrFilter : Any, priority : Int = 0)(body : => Unit) = spec += CsrOnDecode(csrFilter, priority, () => body)
  def onDecodeTrap() : Unit
  def onDecodeUntrap() : Unit
  def onDecodeFlushPipeline() : Unit
  def onDecodeRead : Bool
  def onDecodeWrite : Bool
  def onDecodeHartId : UInt
  def onDecodeAddress : UInt

  def onRead (csrFilter : Any, onlyOnFire : Boolean)(body : => Unit) = spec += CsrOnRead(csrFilter, onlyOnFire, () => body)
  def onReadAddress: UInt
  def onReadHartId: UInt
  def onReadHalt(): Unit

  def onReadToWrite (csrFilter : Any)(body : => Unit) = spec += CsrOnReadToWrite(csrFilter, () => body)
  def onReadToWriteBits: Bits

  def onWrite(csrFilter : Any, onlyOnFire : Boolean)(body : => Unit) = spec += CsrOnWrite(csrFilter, onlyOnFire, () => body)
  def onWriteHalt() : Unit
  def onWriteBits : Bits
  def onWriteAddress : UInt
  def onWriteHartId: UInt
  def onWriteFlushPipeline() : Unit

  def getCsrRam() : CsrRamService
  def onReadMovingOff : Bool
  def onWriteMovingOff : Bool

  def allowCsr(csrFilter : Any) = onDecode(csrFilter){}


  def readWrite(alloc : CsrRamAllocation, filters : Any) = spec += CsrRamSpec(filters, alloc)
  def readWriteRam(filters : Int) = {
    val alloc = getCsrRam.ramAllocate(1)
    spec += CsrRamSpec(filters, alloc)
    alloc
  }

  def onReadingCsr(csrFilter : Any): Bool = {
    isReadingCsrMap.getOrElseUpdate(csrFilter, spec.addRet(CsrIsReadingCsr(csrFilter, Bool())).asInstanceOf[CsrIsReadingCsr]).value
  }
  def onReadingHartId(hartId : Int): Bool = {
    onReadingHartIdMap.getOrElseUpdate(hartId, Bool())
  }
  def onReadingHartIdCsr(hartId: Int, csrFilter : Any): Bool = {
    isReadingHartIdCsrMap.getOrElseUpdate(hartId -> csrFilter, onReadingHartId(hartId) && onReadingCsr(csrFilter))
  }

  def onWritingHartId(hartId: Int): Bool = {
    onWritingHartIdMap.getOrElseUpdate(hartId, Bool())
  }

  def read[T <: Data](value: T, csrFilter: Any, bitOffset: Int = 0): Unit = {
    val converted = value match {
      case v : Bits => v
      case v => v.asBits
    }
    reads += CsrOnReadData(bitOffset, converted.andMask(onReadingCsr(csrFilter)))
  }

  def write[T <: Data](value : T, csrId : Int, bitOffset : Int = 0) : Unit = {
    onWrite(csrId, true){ value.assignFromBits(onWriteBits(bitOffset, widthOf(value) bits)) }
  }
  def writeWhen[T <: Data](value : T, cond : Bool, csrId : Int, bitOffset : Int = 0) : Unit = {
    onWrite(csrId, true){ when(cond) { value.assignFromBits(onWriteBits(bitOffset, widthOf(value) bits)) }}
  }
  def readWrite[T <: Data](value : T, csrId : Int, bitOffset : Int = 0) : Unit = {
    read(value, csrId, bitOffset)
    write(value, csrId, bitOffset)
  }

  def readToWrite[T <: Data](value : T, csrFilter : Any, bitOffset : Int = 0) : Unit = {
    onReadToWrite(csrFilter){
      onReadToWriteBits(bitOffset, widthOf(value) bits) := value.asBits
    }
  }

  def readWrite(csrAddress : Int, thats : (Int, Data)*) : Unit = for(that <- thats) readWrite(that._2, csrAddress, that._1)
  def write(csrAddress : Int, thats : (Int, Data)*) : Unit = for(that <- thats) write(that._2, csrAddress, that._1)
  def read(csrAddress : Int, thats : (Int, Data)*) : Unit = for(that <- thats) read(that._2, csrAddress, that._1)


  //Warning currently do not apply on ram writes
  def writeCancel(csrFilter : Any, cond : Bool) ={
    spec += CsrWriteCancel(csrFilter, cond)
  }

  def hart(hartId : Int) = new CsrHartApi(this, hartId)
}

class CsrHartApi(csrService: CsrService, hartId : Int){
  def read[T <: Data](value: T, csrFilter: Any, bitOffset: Int = 0): Unit = {
    val converted = value match {
      case v: Bits => v
      case v => v.asBits
    }
    csrService.reads += CsrOnReadData(bitOffset, converted.andMask(csrService.onReadingHartIdCsr(hartId, csrFilter)))
  }

  def write[T <: Data](value: T, csrFilter: Any, bitOffset: Int = 0): Unit = {
    val hartSel = csrService.onWritingHartId(hartId)
    csrService.onWrite(csrFilter, true) {
      if(Global.HART_COUNT > 1) when(hartSel) {
        value.assignFromBits(csrService.onWriteBits(bitOffset, widthOf(value) bits))
      } else {
        value.assignFromBits(csrService.onWriteBits(bitOffset, widthOf(value) bits))
      }
    }
  }

  def readWrite[T <: Data](value: T, csrId: Int, bitOffset: Int = 0): Unit = {
    read(value, csrId, bitOffset)
    write(value, csrId, bitOffset)
  }

  class Csr(csrFilter : Any) extends Area{
      def read[T <: Data](value: T, bitOffset: Int = 0): Unit = {
        CsrHartApi.this.read(value, csrFilter, bitOffset)
      }
      def write[T <: Data](value: T, bitOffset: Int = 0): Unit = {
        CsrHartApi.this.write(value, csrFilter, bitOffset)
      }
      def readWrite[T <: Data](value: T, bitOffset: Int = 0): Unit = {
        read(value, bitOffset)
        write(value, bitOffset)
      }

      def readWrite(thats: (Int, Data)*): Unit = for (that <- thats) readWrite(that._2, that._1)
      def write(thats: (Int, Data)*): Unit = for (that <- thats) write(that._2, that._1)
      def read(thats: (Int, Data)*): Unit = for (that <- thats) read(that._2, that._1)
  }

  def onCsr(csrFilter : Any) = new Csr(csrFilter)
}

class CsrRamAllocation(val entries : Int){
  var at = -1
  var addressWidth = -1
  def getAddress(offset : UInt) : UInt = {
    U(at, addressWidth bits) | offset
  }
  def getAddress() = U(at, addressWidth bits)

  val entriesLog2 = 1 << log2Up(entries)
}
case class CsrRamRead(addressWidth : Int, dataWidth : Int, priority : Int) extends Bundle{
  val valid, ready = Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits) //One cycle after fired

  def fire = valid && ready
}

case class CsrRamWrite(addressWidth : Int, dataWidth : Int, priority : Int) extends Bundle{
  val valid, ready = Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)

  def fire = valid && ready
}


object CsrRamService{
  //Priorities are arranged in a way to improve ports.ready timings
  val priority = new {
    val INIT    = 0
    val TRAP    = 1
    val COUNTER = 2
    val CSR     = 3  //This is the very critical path
  }
}
//usefull for, for instance, mscratch scratch mtvec stvec mepc sepc mtval stval satp pmp stuff
trait CsrRamService {
  def ramAllocate(entries : Int) : CsrRamAllocation
  def ramReadPort(priority : Int) : Handle[CsrRamRead]
  def ramWritePort(priority : Int) : Handle[CsrRamWrite]
  val allocationLock = Lock()
  val portLock = Lock()
}
