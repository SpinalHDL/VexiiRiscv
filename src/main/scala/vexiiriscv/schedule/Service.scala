package vexiiriscv.schedule

import spinal.core._
import spinal.core.fiber.{Retainer, Lockable}
import spinal.lib.Flow
import spinal.lib.misc.pipeline.CtrlLink
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute.Execute
import vexiiriscv.fetch.JumpCmd

object Ages {
  val STAGE = 10
  val NOT_PREDICTION = 1
  val FETCH = 0
  val DECODE = 1000
  val EU = 2000
  val TRAP = 3000
}

case class FlushCmd(age : Int, laneAgeWidth : Int, withUopId : Boolean) extends Bundle{
  val hartId = Global.HART_ID()
  val uopId = withUopId generate Decode.UOP_ID()
  val laneAge = UInt(laneAgeWidth bits)
  val self = Bool()
}

case class TrapCmd(age : Int, pcWidth : Int, tvalWidth : Int, causeWidth : Int, trapArgWidth : Int) extends Bundle {
  val cause      = UInt(causeWidth bits)
  val tval       = Bits(tvalWidth bits)
  val arg        = Bits(trapArgWidth bits)
  val skipCommit = Bool() //Want to skip commit for exceptions, but not for [jump, ebreak, redo]
}

trait ScheduleService {
  def newFlushPort(age: Int, laneAgeWidth: Int, withUopId: Boolean): Flow[FlushCmd]
//  def sharedFlushPort(age: Int, laneAgeWidth: Int, withUopId: Boolean, key : Nameable): Flow[FlushCmd]
//  def newPcPort(age : Int, aggregationPriority : Int = 0) : Flow[JumpCmd]
  def newTrapPort(age : Int, causeWidth : Int = 4) : Flow[TrapCmd]
  def isFlushedAt(age: Int, hartId : UInt, laneAge : UInt): Option[Bool]
//  def addCtrl(age : Int, ctrl : CtrlLink) : Unit

  val elaborationLock = Retainer()
}


