package vexiiriscv.test

import spinal.core._
import spinal.core.sim._

abstract class PeripheralEmulator(offset : Long, mei : Bool, sei : Bool) {
  val PUTC = 0
  val PUT_HEX = 0x8
  val CLINT_BASE = 0x10000
  val CLINT_TIME = CLINT_BASE + 0xBFF8
  val MACHINE_EXTERNAL_INTERRUPT_CTRL = 0x10
  val SUPERVISOR_EXTERNAL_INTERRUPT_CTRL = 0x18
  val GETC = 0x40
  val STATS_CAPTURE_ENABLE = 0x50
  val PUT_DEC = 0x60
  val INCR_COUNTER = 0x70
  val FAILURE_ADDRESS = 0x80
  val IO_FAULT_ADDRESS = 0x0FFFFFF0
  val RANDOM = 0xA8

  if(mei != null) mei #= false
  if(sei != null) sei #= false

  def getClintTime() : Long

  def access(write : Boolean, address : Long, data : Array[Byte]) : Boolean = {
    val addressPatched = address - offset
    if(write){
      addressPatched.toInt match {
        case PUTC => print(data(0).toChar)
        case PUT_HEX => print(data.reverse.map(v => f"$v%02x").mkString(""))
        case MACHINE_EXTERNAL_INTERRUPT_CTRL => mei #= data(0).toBoolean
        case SUPERVISOR_EXTERNAL_INTERRUPT_CTRL => sei #= data(0).toBoolean
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
        case GETC => {
          if (System.in.available() != 0) {
            data(0) = System.in.read().toByte
          } else {
            for (i <- 0 until data.size) data(i) = 0xFF.toByte
          }
        }
        case RANDOM => {
          simRandom.nextBytes(data)
        }
        case CLINT_TIME => readLong(getClintTime())
        case _ => {
          println(address)
          simFailure()
        }
      }
    }
    false
  }

}

