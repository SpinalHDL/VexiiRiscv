package vexiiriscv.decode

import spinal.core._
import spinal.core.fiber.Lockable
import spinal.lib._
import spinal.lib.logic.Masked
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{RegfileSpec, RfRead}

trait DecoderService extends Lockable {
//  def addEuOp(fu: ExecuteUnitService, microOp : MicroOp) : Unit
//  def addResourceDecoding(resource : Resource, stageable : Stageable[Bool])
//  def covers() : Seq[Masked] //List of all instruction implemented
//  def euGroups : Seq[EuGroup]
//  def addMicroOpDecoding(microOp: MicroOp, decoding: DecodeListType)
//  def addMicroOpDecodingDefault(key : Stageable[_ <: BaseType], value : BaseType) : Unit
//  def addDecodingToRob(key : Stageable[_ <: BaseType])
//
//  def READ_RS(id : Int)  : SignalKey[Bool]
//  def ARCH_RS(id : Int)  : SignalKey[UInt]
//  def PHYS_RS(id : Int)  : SignalKey[UInt]
//
//  def READ_RS(id : RfRead)  : SignalKey[Bool]
//  def ARCH_RS(id : RfRead)  : SignalKey[UInt]
//  def PHYS_RS(id : RfRead)  : SignalKey[UInt]
//
//  def WRITE_RD : SignalKey[Bool]
//  def PHYS_RD  : SignalKey[UInt]
//  def PHYS_RD_FREE : SignalKey[UInt]
//  def ARCH_RD  : SignalKey[UInt]
//
//  def REGFILE_RD : RegFileSel
//  def REGFILE_RS(id : Int) : RegFileSel
//  def REGFILE_RS(id : RfRead) : RegFileSel
//
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