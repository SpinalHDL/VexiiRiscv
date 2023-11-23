package vexiiriscv.decode

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib._
import spinal.lib.logic.Masked
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{MicroOp, RegfileSpec, RfRead}

trait DecoderService {
  val elaborationLock = Lock()

//  def addEuOp(fu: ExecuteUnitService, microOp : MicroOp) : Unit
//  def addResourceDecoding(resource : Resource, stageable : Stageable[Bool])
//  def covers() : Seq[Masked] //List of all instruction implemented
//  def euGroups : Seq[EuGroup]

  def addMicroOpDecoding[T <: BaseType](microOp: MicroOp, key : Payload[T], value: T) : Unit = addMicroOpDecoding(microOp, DecodeList(key -> value))
  def addMicroOpDecoding(microOp: MicroOp, decoding: DecodeListType)
  def addMicroOpDecodingDefault(key : Payload[_ <: BaseType], value : BaseType) : Unit

//  def rsCount(rf : RegfileSpec)  : Int
//  def rsCountMax()  : Int
//  def rsPhysicalDepthMax : Int
//  def getTrap() : Flow[DecoderTrap]
//
//  //The trap interface allow the privilegied plugin to ask the decoder to produce trap
//  def trapHalt() : Unit
//  def trapRaise() : Unit
//  def trapReady() : Bool
//
//  //Used by the debug trigger module to implement hardware breakpoints (trigger in the frontend.decoded stage)
//  def debugEnter(slotId : Int) : Unit
}