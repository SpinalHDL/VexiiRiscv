package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.Plugin
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.memory.AddressTranslationService

import scala.collection.mutable.ArrayBuffer

case class JumpCmd(laneAgeWidth : Int) extends Bundle{
  val pc = PC()
  val hartId = HART_ID()
  val laneAge = UInt(laneAgeWidth bits)
}

case class PcServiceHoldPortSpec(hartId : Int, valid : Bool)
trait PcService {
  val elaborationLock = Lock()
  def createJumpInterface(age: Int, laneAgeWidth : Int, aggregationPriority : Int) : Flow[JumpCmd] //High priority win
  def simSetPc(value : Long) : Unit
  def forcedSpawn() : Bool
  def newHoldPort(hartId : Int) : Bool = holdPorts.addRet(PcServiceHoldPortSpec(hartId, Bool())).valid
  val holdPorts = ArrayBuffer[PcServiceHoldPortSpec]()
}

trait InitService{
  def initHold() : Bool
}