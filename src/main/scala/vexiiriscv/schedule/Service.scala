package vexiiriscv.schedule

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib.Flow
import spinal.lib.misc.pipeline.CtrlLink
import vexiiriscv.Global
import vexiiriscv.fetch.JumpCmd

object Ages {
  val STAGE = 10
  val PREDICTION = -1
  val FETCH = 0
  val DECODE = 1000
  val EU = 2000
}

case class FlushCmd(age : Int/*, subAgeWidth : Int*/) extends Bundle{
//  val subAge = UInt(subAgeWidth bits)
  val hartId = Global.HART_ID()
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
  def isFlushedAt(age: Int, tid : UInt): Option[Bool]
//  def addCtrl(age : Int, ctrl : CtrlLink) : Unit

  val elaborationLock = Lock()
}


