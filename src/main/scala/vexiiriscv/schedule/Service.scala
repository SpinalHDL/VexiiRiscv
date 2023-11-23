package vexiiriscv.schedule

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib.Flow
import spinal.lib.misc.pipeline.CtrlLink
import vexiiriscv.fetch.JumpCmd

case class FlushCmd(subAgeWidth : Int) extends Bundle{
  val subAge = UInt(subAgeWidth bits)
}

case class TrapCmd(age : Int, pcWidth : Int, tvalWidth : Int, causeWidth : Int) extends Bundle {
  val cause      = UInt(causeWidth bits)
  val tval       = Bits(tvalWidth bits)
  val skipCommit = Bool() //Want to skip commit for exceptions, but not for [jump, ebreak, redo]
}

trait ScheduleService {
  def newFlushPort(age: Int): Flow[FlushCmd]
  def newPcPort(age : Int, aggregationPriority : Int = 0) : Flow[JumpCmd]
  def newTrapPort(age : Int, causeWidth : Int = 4) : Flow[TrapCmd]
  def addCtrl(age : Int, ctrl : CtrlLink) : Unit

  val elaborationLock = Lock()
}


