package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.{Retainer, Lockable}
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.Plugin
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.memory.AddressTranslationService

import scala.collection.mutable.ArrayBuffer

case class JumpCmd(laneAgeWidth : Int) extends Bundle{
  val fault = Bool()
  val pc = PC()
  val hartId = HART_ID()
  val laneAge = UInt(laneAgeWidth bits)
}

case class PcServiceHoldPortSpec(hartId : Int, valid : Bool)
trait PcService {
  val elaborationLock = Retainer()
  def newJumpInterface(age: Int, laneAgeWidth : Int, aggregationPriority : Int) : Flow[JumpCmd] //High priority win
  def simSetPc(value : Long) : Unit
  def forcedSpawn() : Bool //As fetch stage 0 isn't persistant, this provide the information when a persistance break happend (ex to fork again a transaction to the memory system)
  def newHoldPort(hartId : Int) : Bool = holdPorts.addRet(PcServiceHoldPortSpec(hartId, Bool())).valid
  val holdPorts = ArrayBuffer[PcServiceHoldPortSpec]()
}

trait InitService{
  def initHold() : Bool
}