package vexiiriscv.execute

import spinal.core.fiber.{Handle, Retainer}
import spinal.core._
import spinal.lib._
import vexiiriscv.Global
import vexiiriscv.riscv.Riscv

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class CsrSpec(val csrFilter : Any)
case class CsrIsReadingCsr(override val csrFilter : Any, value : Bool) extends CsrSpec(csrFilter)
case class CsrOnRead (override val csrFilter : Any, onlyOnFire : Boolean, body : () => Unit) extends CsrSpec(csrFilter)
case class CsrOnReadToWrite (override val csrFilter : Any, body : () => Unit) extends CsrSpec(csrFilter) //Allow the fancy supervisor external interrupt logic
case class CsrOnWrite(override val csrFilter : Any, onlyOnFire : Boolean, body : () => Unit) extends CsrSpec(csrFilter)
case class CsrOnDecode (override val csrFilter : Any, priority : Int, body : () => Unit) extends CsrSpec(csrFilter)
case class CsrWriteCancel(override val csrFilter : Any, cond : Bool) extends CsrSpec(csrFilter)

case class CsrOnReadData (bitOffset : Int, value : Bits)
case class CsrIsReadingHartId(hartId : Int, value : Bool)


case class CsrListFilter(mapping : Seq[Int]) extends Nameable

case class CsrDecode() extends Bundle {
  val exception = Bool()
  val read, write = Bool()
  val hartId = Global.HART_ID()
  val address = UInt(12 bits)
  val trap = Bool()
  val trapCode = Global.CODE()

  def doException(): Unit = {
    exception := True
  }
  def doTrap(code: Int): Unit = {
    trap := True
    trapCode := code
  }
}

case class CsrRead() extends Bundle {
  val valid, moving = Bool()
  val address = UInt(12 bits)
  val halt = Bool()
  val hartId = Global.HART_ID()
  val toWriteBits = Bits(Riscv.XLEN bits)
  val data = Bits(Riscv.XLEN bits)

  def doHalt(): Unit = halt := True
}

case class CsrWrite() extends Bundle {
  val valid, moving = Bool()
  val halt = Bool()
  val bits = Bits(Riscv.XLEN bits)
  val address = UInt(12 bits)
  val hartId = Global.HART_ID()

  def doHalt(): Unit = halt := True
}

case class CsrBus() extends Bundle {
  val decode = CsrDecode()
  val read = CsrRead()
  val write = CsrWrite()

  def setup(): this.type = {
    decode.exception := False
    decode.trap := False
    decode.trapCode.assignDontCare()
    read.halt := False
    write.halt := False
    this
  }
}


trait CsrService {
  val csrLock = Retainer()
  val spec = ArrayBuffer[CsrSpec]()
  val reads = ArrayBuffer[CsrOnReadData]()
  val isReadingCsrMap = mutable.LinkedHashMap[Any, CsrIsReadingCsr]()
  val onReadingHartIdMap = mutable.LinkedHashMap[Int, Bool]()
  val isReadingHartIdCsrMap = mutable.LinkedHashMap[(Int, Any), Bool]()
  val onWritingHartIdMap = mutable.LinkedHashMap[Int, Bool]()
  val trapNextOnWrite = mutable.LinkedHashSet[Any]()
  def waitElaborationDone() : Unit

  val bus = Handle[CsrBus]

  def onDecode(csrFilter : Any, priority : Int = 0)(body : => Unit) = spec += CsrOnDecode(csrFilter, priority, () => body)
  def onRead (csrFilter : Any, onlyOnFire : Boolean)(body : => Unit) = spec += CsrOnRead(csrFilter, onlyOnFire, () => body)
  def onReadToWrite (csrFilter : Any)(body : => Unit) = spec += CsrOnReadToWrite(csrFilter, () => body)
  def onWrite(csrFilter : Any, onlyOnFire : Boolean)(body : => Unit) = spec += CsrOnWrite(csrFilter, onlyOnFire, () => body)
  def allowCsr(csrFilter : Any) = onDecode(csrFilter){}

  def readingCsr(csrFilter : Any): Bool = {
    isReadingCsrMap.getOrElseUpdate(csrFilter, spec.addRet(CsrIsReadingCsr(csrFilter, Bool())).asInstanceOf[CsrIsReadingCsr]).value
  }
  def readingHartId(hartId : Int): Bool = {
    onReadingHartIdMap.getOrElseUpdate(hartId, Bool())
  }
  def readingHartIdCsr(hartId: Int, csrFilter : Any): Bool = {
    isReadingHartIdCsrMap.getOrElseUpdate(hartId -> csrFilter, readingHartId(hartId) && readingCsr(csrFilter))
  }

  def writingHartId(hartId: Int): Bool = {
    onWritingHartIdMap.getOrElseUpdate(hartId, Bool())
  }

  def read[T <: Data](value: T, csrFilter: Any, bitOffset: Int = 0): Unit = {
    val converted = value match {
      case v : Bits => v
      case v => v.asBits
    }
    reads += CsrOnReadData(bitOffset, converted.andMask(readingCsr(csrFilter)))
  }

  def readAlways[T <: Data](value: T, bitOffset: Int = 0): Unit = {
    val converted = value match {
      case v: Bits => v
      case v => v.asBits
    }
    reads += CsrOnReadData(bitOffset, converted)
  }

  def write[T <: Data](value : T, csrId : Int, bitOffset : Int = 0) : T = {
    onWrite(csrId, true){ value.assignFromBits(bus.write.bits(bitOffset, widthOf(value) bits)) }
    value
  }
  def writeWhen[T <: Data](value : T, cond : Bool, csrId : Int, bitOffset : Int = 0) : T = {
    onWrite(csrId, true){ when(cond) { value.assignFromBits(bus.write.bits(bitOffset, widthOf(value) bits)) }}
    value
  }
  def readWrite[T <: Data](value : T, csrId : Int, bitOffset : Int = 0) : T = {
    read(value, csrId, bitOffset)
    write(value, csrId, bitOffset)
    value
  }

  def readToWrite[T <: Data](value : T, csrFilter : Any, bitOffset : Int = 0) : Unit = {
    onReadToWrite(csrFilter){
      bus.read.toWriteBits(bitOffset, widthOf(value) bits) := value.asBits
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

  def onWrite(csrFilter : Any, onlyOnFire : Boolean)(body : => Unit) = csrService.onWrite(csrFilter, onlyOnFire){
    when(csrService.writingHartId(hartId)){ body }
  }

  def isWriting(csrFilter: Any, onlyOnFire: Boolean) = {
    val ret = False
    csrService.onWrite(csrFilter, onlyOnFire) {
      when(csrService.writingHartId(hartId)) {
        ret := True
      }
    }
    ret
  }

  def writeWhen[T <: Data](value: T, cond: Bool, csrId: Int, bitOffset: Int = 0): Unit = {
    onWrite(csrId, true) {
      when(cond) {
        value.assignFromBits(csrService.bus.write.bits(bitOffset, widthOf(value) bits))
      }
    }
  }

  def onReadToWrite(csrFilter: Any)(body: => Unit) = csrService.onReadToWrite(csrFilter) {
    when(csrService.readingHartId(hartId)) {
      body
    }
  }
  def readToWrite[T <: Data](value: T, csrFilter: Any, bitOffset: Int = 0): Unit = {
    onReadToWrite(csrFilter) {
      csrService.bus.read.toWriteBits(bitOffset, widthOf(value) bits) := value.asBits
    }
  }

  def read[T <: Data](value: T, csrFilter: Any, bitOffset: Int = 0): Unit = {
    val converted = value match {
      case v: Bits => v
      case v => v.asBits
    }
    csrService.reads += CsrOnReadData(bitOffset, converted.andMask(csrService.readingHartIdCsr(hartId, csrFilter)))
  }

  def write[T <: Data](value: T, csrFilter: Any, bitOffset: Int = 0): Unit = {
    val hartSel = csrService.writingHartId(hartId)
    csrService.onWrite(csrFilter, true) {
      if(Global.HART_COUNT > 1) when(hartSel) {
        value.assignFromBits(csrService.bus.write.bits(bitOffset, widthOf(value) bits))
      } else {
        value.assignFromBits(csrService.bus.write.bits(bitOffset, widthOf(value) bits))
      }
    }
  }

  def readWrite[T <: Data](value: T, csrId: Int, bitOffset: Int = 0): Unit = {
    read(value, csrId, bitOffset)
    write(value, csrId, bitOffset)
  }

  def readWrite(csrId: Int, thats: (Int, Data)*): Unit = for (that <- thats) readWrite(that._2, csrId, that._1)
  def write(csrId: Int, thats: (Int, Data)*): Unit = for (that <- thats) write(that._2, csrId, that._1)
  def read(csrId: Int, thats: (Int, Data)*): Unit = for (that <- thats) read(that._2, csrId, that._1)

  class Csr(csrFilter : Any) extends Area{
      def onWrite(onlyOnFire: Boolean)(body: => Unit) = CsrHartApi.this.onWrite(csrFilter, onlyOnFire) {
        body
      }

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
  val priority = new {
    val INIT = 0
    val CSR = 1
    val TRAP = 2
    val COUNTER = 3
  }
}

//usefull for, for instance, mscratch scratch mtvec stvec mepc sepc mtval stval satp pmp stuff
trait CsrRamService extends Area{
  val portLock = Retainer()
  val csrLock = Retainer()

  val allocations = ArrayBuffer[CsrRamAllocation]()
  val reads = ArrayBuffer[Handle[CsrRamRead]]()
  val writes = ArrayBuffer[Handle[CsrRamWrite]]()

  def ramAllocate(entries: Int = 1): CsrRamAllocation = allocations.addRet(new CsrRamAllocation(entries))
  def ramReadPort(priority: Int): Handle[CsrRamRead] = reads.addRet(Handle(CsrRamRead(portAddressWidth, Riscv.XLEN.get, priority)))
  def ramWritePort(priority: Int): Handle[CsrRamWrite] = writes.addRet(Handle(CsrRamWrite(portAddressWidth, Riscv.XLEN.get, priority)))

  case class Mapping(csrFilter: Any, alloc: CsrRamAllocation, offset : Int)
  val csrMappings = mutable.ArrayBuffer[Mapping]()

  def readWriteRam(filters: Int) = {
    val alloc = ramAllocate(1)
    csrMappings += Mapping(filters, alloc, 0)
    alloc
  }

  def readWriteRam(filters: Any, alloc: CsrRamAllocation, offset : Int) = {
    csrMappings += Mapping(filters, alloc, offset)
    alloc
  }

  def portAddressWidth : Int
  def awaitMapping() : Unit
  def holdCsrRead(): Unit
  def holdCsrWrite(): Unit
}
