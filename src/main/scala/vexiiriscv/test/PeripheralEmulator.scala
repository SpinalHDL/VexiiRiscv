package vexiiriscv.test

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import spinal.lib.sim.SparseMemory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * This is used for simulations, it can be used to emulate a minimal sets of peripherals :
 * - Terminal bound to stdin stdout
 * - RISC-V CLINT
 * - Simulation pass/fail commands
 */
abstract class PeripheralEmulator(offset : Long, mei : scala.collection.Seq[Bool], sei : scala.collection.Seq[Bool], msi : scala.collection.Seq[Bool] = Seq.empty, mti : scala.collection.Seq[Bool] = Seq.empty, cd : ClockDomain = null) {
  val PUTC = 0
  val PUT_HEX = 0x8
  val CLINT_BASE = 0x10000
  val CLINT_TIME = CLINT_BASE + 0xBFF8
  val CLINT_TIMEH = CLINT_BASE + 0xBFF8 + 4
  val CLINT_CMP = CLINT_BASE + 0x4000
  val CLINT_CMPH = CLINT_BASE + 0x4000 + 4
  val MACHINE_EXTERNAL_INTERRUPT_CTRL = 0x10
  val SUPERVISOR_EXTERNAL_INTERRUPT_CTRL = 0x18
  val MACHINE_EXTERNAL_INTERRUPT_CTRL_HARTS = 0x1000
  val SUPERVISOR_EXTERNAL_INTERRUPT_CTRL_HARTS = 0x2000
  val EXTERNAL_INTERRUPT_CTRL_HARTS_SIZE = 0x1000
  val GETC = 0x40
  val GETC_EMPTY = 0x44
  val STATS_CAPTURE_ENABLE = 0x50
  val PUT_DEC = 0x60
  val INCR_COUNTER = 0x70
  val FAILURE_ADDRESS = 0x80
  val IO_FAULT_ADDRESS = 0x0FFFFFF0
  val CMB_ADDRESS = 0x100
  val CMB_DATA = 0x108
  val RANDOM = 0xA8

  val cmp = Array.fill(mti.size)(BigInt("FFFFFFFFFFFFFFFF", 16))
  val cmb = new {
    var mem : SparseMemory = null
    var address = 0l
    var data = 0l
  }

  mei.foreach(_ #= false)
  sei.foreach(_ #= false)
  msi.foreach(_ #= false)
  mti.foreach(_ #= false)
  if (mti.nonEmpty) {
    require(cd != null, "A clock domain is required when timer interrupt outputs are provided")
    cd.onSamplings {
      val time = getClintTime()
      mti.zip(cmp).foreach { case (interrupt, compare) => interrupt #= compare <= time }
    }
  }


  def getClintTime() : BigInt

  val putcListeners = ArrayBuffer[Char => Unit]()
  def putc(c : Char) : Unit = {putcListeners.foreach(_(c))}


  var withStdIn = true
  var getcQueue = mutable.Queue[Byte]()
  def getcEmpty(data : Array[Byte]): Unit = {
    data(0) = if (getcQueue.nonEmpty || (withStdIn && System.in.available() > 0)) 0 else 1
  }
  def getc(data : Array[Byte]): Unit = {
    if(getcQueue.nonEmpty){
      data(0) = getcQueue.dequeue(); return
    }
    if (withStdIn && System.in.available() > 0) {
      data(0) = System.in.read().toByte; return
    }
    for (i <- 0 until data.size) data(i) = 0xFF.toByte
  }


  def access(write : Boolean, address : Long, data : Array[Byte]) : Boolean = {
    val addressPatched = address - offset
    if(write) {
      val raw = BigInt(data.map(_.toByte).reverse.toArray)
      val v = raw.toLong
      addressPatched.toInt match {
        case PUTC => {
          val c = data(0).toChar
          print(c.toString match {
            case s => s
          })
          putc(c)
        }
        case PUT_HEX => print(data.reverse.map(v => f"$v%02x").mkString(""))
        case PUT_DEC => print(f"${BigInt(data.map(_.toByte).reverse.toArray)}%d")
        case MACHINE_EXTERNAL_INTERRUPT_CTRL => {
          if (mei.isEmpty) return true
          mei.head #= raw != 0
        }
        case SUPERVISOR_EXTERNAL_INTERRUPT_CTRL => {
          if (sei.isEmpty) return true
          sei.head #= raw != 0
        }
        case address if address >= MACHINE_EXTERNAL_INTERRUPT_CTRL_HARTS && address < MACHINE_EXTERNAL_INTERRUPT_CTRL_HARTS + EXTERNAL_INTERRUPT_CTRL_HARTS_SIZE => {
          val offset = address - MACHINE_EXTERNAL_INTERRUPT_CTRL_HARTS
          val hartId = offset / 4
          if (hartId >= mei.size) return true
          mei(hartId) #= raw != 0
        }
        case address if address >= SUPERVISOR_EXTERNAL_INTERRUPT_CTRL_HARTS && address < SUPERVISOR_EXTERNAL_INTERRUPT_CTRL_HARTS + EXTERNAL_INTERRUPT_CTRL_HARTS_SIZE => {
          val offset = address - SUPERVISOR_EXTERNAL_INTERRUPT_CTRL_HARTS
          val hartId = offset / 4
          if (hartId >= sei.size) return true
          sei(hartId) #= raw != 0
        }
        case address if address >= CLINT_BASE && address < CLINT_CMP => {
          val offset = address - CLINT_BASE
          val hartId = offset / 4
          if (hartId >= msi.size) return true
          msi(hartId) #= (data(0).toInt & 1).toBoolean
        }
        case address if address >= CLINT_CMP && address < CLINT_TIME => {
          val offset = address - CLINT_CMP
          val hartId = offset / 8
          val wordOffset = offset & 7
          if (hartId >= cmp.length) return true
          (data.size, wordOffset) match {
            case (4, 0) => cmp(hartId) = (cmp(hartId) & BigInt("FFFFFFFF00000000", 16)) | (raw & BigInt("00000000FFFFFFFF", 16))
            case (4, 4) => cmp(hartId) = (cmp(hartId) & 0xFFFFFFFFl) | ((raw & 0xFFFFFFFFl) << 32)
            case (8, 0) => cmp(hartId) = raw & BigInt("FFFFFFFFFFFFFFFF", 16)
            case _ => return true
          }
        }
        case IO_FAULT_ADDRESS => {
          return true
        }
        case CMB_ADDRESS => cmb.address = v & 0xFFFFFFFFl
        case CMB_DATA => {
          val dut = data
          val ref = cmb.mem.readBytes(cmb.address, data.size)
          if((dut, ref).zipped.exists(_ != _)){
            println("CMB write mismatch")
            simFailure()
          }
        }
        case _ => {
          val message = f"In PeripheralEmulator, invalid write at address 0x$address%x of value $data"
          println(message)
          simFailure(message)
        }
      }
    } else {
      def readLong(that : Long) : Unit = {
        for (i <- 0 until data.size) data(i) = (that >> i*8).toByte
      }
      for(i <- 0 until data.size) data(i) = 0
      addressPatched.toInt match {
        case IO_FAULT_ADDRESS => {
          simRandom.nextBytes(data)
          return true;
        }
        case GETC => getc(data)
        case GETC_EMPTY => getcEmpty(data)
        case RANDOM => simRandom.nextBytes(data)
        case CLINT_TIME => readLong(getClintTime().toLong)
        case CLINT_TIMEH => readLong((getClintTime() >> 32).toLong)
        case CMB_DATA => cmb.mem.readBytes(cmb.address, data.size, data, 0)
        case _ => {
          val message = f"In PeripheralEmulator, invalid read at address 0x$address%x"
          println(message)
          simFailure(message)
        }
      }
    }
    false
  }

  // Map the PeripheralEmulator as a simulation slave for the given tilelink bus.
  def bind(bus : tilelink.Bus, cd : ClockDomain) = new tilelink.sim.MonitorSubscriber{
    val monitor = new tilelink.sim.Monitor(bus, cd).add(this)
    val driver = new tilelink.sim.SlaveDriver(bus, cd)

    import tilelink.sim._
    import tilelink._
    override def onA(a: TransactionA) = {
      val d = TransactionD(a)
      a.opcode match {
        case Opcode.A.PUT_FULL_DATA => {
          d.opcode = Opcode.D.ACCESS_ACK
          d.denied = access(true, a.address.toInt, a.data)
        }
        case Opcode.A.GET => {
          d.opcode = Opcode.D.ACCESS_ACK_DATA
          d.data = Array.fill(a.bytes)(0)
          d.denied = access(false, a.address.toInt, d.data)
        }
      }

      driver.scheduleD(d)
    }
  }

}
