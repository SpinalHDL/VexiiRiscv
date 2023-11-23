package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Lockable
import spinal.lib.Flow
import vexiiriscv.fetch.JumpCmd

case class FlushCmd(subAgeWidth : Int) extends Bundle{
  val subAge = UInt(subAgeWidth bits)
}

trait FlusherService extends Lockable {

}

case class TrapCmd(age : Int, pcWidth : Int, tvalWidth : Int, causeWidth : Int) extends Bundle {
  val cause      = UInt(causeWidth bits)
  val tval       = Bits(tvalWidth bits)
  val skipCommit = Bool() //Want to skip commit for exceptions, but not for [jump, ebreak, redo]
}

trait ScheduleService extends Lockable {
  def newFlushPort(age: Int): Flow[FlushCmd]
  def newPcPort(age : Int, aggregationPriority : Int = 0) : Flow[JumpCmd]
  def newTrapPort(age : Int, causeWidth : Int = 4) : Flow[TrapCmd]
}



/*
case class ScheduleCmd(canTrap : Boolean, canJump : Boolean, pcWidth : Int, causeWidth : Int, tvalWidth : Int) extends Bundle {
  val trap       = (canTrap && canJump) generate Bool()
  val pcTarget   = canJump generate UInt(pcWidth bits)
  val cause      = canTrap generate UInt(causeWidth bits)
  val tval       = canTrap generate Bits(tvalWidth bits)
  val skipCommit = Bool() //Want to skip commit for exceptions, but not for [jump, ebreak, redo]
//  val reason     = ScheduleReason.hardType()

  def isTrap = (canTrap, canJump) match {
    case (false, true) => False
    case (true, false) => True
    case (true, true) =>  trap
    case _ => ???
  }
}

trait ScheduleService extends Lockable {
  def newFlushPort(age: Int): Flow[FlushCmd]
  def newSchedulePort(canTrap : Boolean, canJump : Boolean, causeWidth : Int = 4, age : Int) : Flow[ScheduleCmd]
}

 */