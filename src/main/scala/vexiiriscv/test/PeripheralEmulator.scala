package vexiiriscv.test

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class PeripheralEmulator(offset : Long, mei : Bool, sei : Bool, msi : Bool = null, mti : Bool = null, cd : ClockDomain = null) {
  val PUTC = 0
  val PUT_HEX = 0x8
  val CLINT_BASE = 0x10000
  val CLINT_TIME = CLINT_BASE + 0xBFF8
  val CLINT_TIMEH = CLINT_BASE + 0xBFF8 + 4
  val CLINT_CMP = CLINT_BASE + 0x4000
  val CLINT_CMPH = CLINT_BASE + 0x4000 + 4
  val MACHINE_EXTERNAL_INTERRUPT_CTRL = 0x10
  val SUPERVISOR_EXTERNAL_INTERRUPT_CTRL = 0x18
  val GETC = 0x40
  val STATS_CAPTURE_ENABLE = 0x50
  val PUT_DEC = 0x60
  val INCR_COUNTER = 0x70
  val FAILURE_ADDRESS = 0x80
  val IO_FAULT_ADDRESS = 0x0FFFFFF0
  val RANDOM = 0xA8
  var cmp = 0l

  if (mei != null) mei #= false
  if (sei != null) sei #= false
  if (msi != null) msi #= false
  if (mti != null) {
    mti #= false
    cd.onSamplings {
      mti #= cmp < getClintTime()
    }
  }


  def getClintTime() : Long

  val putcListeners = ArrayBuffer[Char => Unit]()
  def putc(c : Char) : Unit = {putcListeners.foreach(_(c))}


  var withStdIn = true
  var getcQueue = mutable.Queue[Byte]()
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
    if(write){
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
        case MACHINE_EXTERNAL_INTERRUPT_CTRL => mei #= data(0).toBoolean
        case SUPERVISOR_EXTERNAL_INTERRUPT_CTRL => sei #= data(0).toBoolean
        case CLINT_BASE => msi #= (data(0).toInt & 1).toBoolean
        case CLINT_CMP => {
          val v = BigInt(data.map(_.toByte).reverse.toArray).toLong
          data.size match {
            case 4 => cmp = cmp & 0xFFFFFFFF00000000l | v
            case 8 => cmp = v
          }
        }
        case CLINT_CMPH => cmp = cmp & 0xFFFFFFFFl | (BigInt(data.map(_.toByte).reverse.toArray).toLong << 32)
        case IO_FAULT_ADDRESS => {
          return true
        }
        case _ => {
          println(address)
          simFailure()
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
        case RANDOM => simRandom.nextBytes(data)
        case CLINT_TIME => readLong(getClintTime())
        case CLINT_TIMEH => readLong(getClintTime() >> 32)
        case _ => {
          println(address)
          simFailure()
        }
      }
    }
    false
  }


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

