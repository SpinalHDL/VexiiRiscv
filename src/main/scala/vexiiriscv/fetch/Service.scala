package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.{Retainer, Lockable}
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.Plugin
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.memory.AddressTranslationService

import scala.collection.mutable.ArrayBuffer

/**
 * Interface which ask the CPU to start fetching another PC.
 */
case class JumpCmd(laneAgeWidth : Int) extends Bundle{
  val fault = Bool() // Allows to force a fetch trap, used when the PC overflow the pyhsical memory space
  val pc = PC()
  val hartId = HART_ID()
  val laneAge = UInt(laneAgeWidth bits) //allows to dynamicaly established the priority between multiple request which would come from the same CPU stage
}

case class PcServiceHoldPortSpec(hartId : Int, valid : Bool)

/**
 * Service which mainly allows other plugins to manipulate the PC being fetched
 */
trait PcService {
  val elaborationLock = Retainer()
  def newJumpInterface(age: Int, laneAgeWidth : Int, aggregationPriority : Int) : Flow[JumpCmd] // Craete a jump interface, Higher priority win
  def simSetPc(value : Long) : Unit // Used by SpinalSim testbench to force the PC at a given value
  def forcedSpawn() : Bool //As fetch stage 0 isn't persistant, this provide the information when a persistance break happend (ex to fork again a transaction to the memory system)
  def newHoldPort(hartId : Int) : Bool = holdPorts.addRet(PcServiceHoldPortSpec(hartId, Bool())).valid // Creates an interface to force the fetch to wait
  val holdPorts = ArrayBuffer[PcServiceHoldPortSpec]()
}

/**
 * After the CPU reset, this service allows a plugin to make the CPU wait longer before starting fetching instruction.
 */
trait InitService{
  def initHold() : Bool
}