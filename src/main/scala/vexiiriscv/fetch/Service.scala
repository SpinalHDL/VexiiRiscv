package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.Plugin
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.memory.AddressTranslationService

case class JumpCmd(laneAgeWidth : Int) extends Bundle{
  val pc = PC()
  val hartId = HART_ID()
  val laneAge = UInt(laneAgeWidth bits)
}

trait PcService {
  def createJumpInterface(age: Int, laneAgeWidth : Int, aggregationPriority : Int) : Flow[JumpCmd] //High priority win
  val elaborationLock = Lock()
  def simSetPc(value : Long) : Unit
  def forcedSpawn() : Bool
}

trait InitService{
  def initHold() : Bool
}