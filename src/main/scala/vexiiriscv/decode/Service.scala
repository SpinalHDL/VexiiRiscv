package vexiiriscv.decode

import spinal.core._
import spinal.core.fiber.{Lockable, Retainer}
import spinal.lib._
import spinal.lib.logic.Masked
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{MicroOp, RegfileSpec, RfRead}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DecodingCtx(val node : NodeBaseApi, val legal : Bool)

/**
 * Provide an API which allows other plugins to ask additional instruction decoding in the decode pipeline,
 * providing decoded values in the DecodePipeline payloads
 */
trait DecoderService {
  val elaborationLock = Retainer()
  val decodingLock = Retainer()
  def covers() : scala.collection.Seq[Masked] //List of all instruction implemented

  def addMicroOpDecoding[T <: BaseType](microOp: MicroOp, key : Payload[T], value: T) : Unit = addMicroOpDecoding(microOp, DecodeList(key -> value))
  def addMicroOpDecoding(microOp: MicroOp, decoding: DecodeListType)
  def addMicroOpDecodingDefault(key : Payload[_ <: BaseType], value : BaseType) : Unit
  def addDecodingLogic(body : DecodingCtx => Unit)
  def addIllegalCheck(body : CtrlLaneApi => Bool)
}

/**
 * Provide an API which allows other plugin to carry pipeline payload from Fetch to Decode.
 * The payload carried can be specified to come from the first or the last fetch-word of a given instruction.
 * This is used by plugins like branch prediction to carry data through the different pipelines
 */
trait AlignerService{
  val lastSliceData, firstSliceData = mutable.LinkedHashSet[NamedType[_ <: Data]]()
  val elaborationLock = Retainer()
  def addLastSliceDataCtx(that : NamedType[_ <: Data]) = lastSliceData += that
  def addFirstSliceDataCtx(that : NamedType[_ <: Data]) = firstSliceData += that
}

/**
 * Provide an API which allows to inject an instruction in the CPU pipeline.
 * This is used by the PrivilegedPlugin to implement the RISC-V External Debug Support spec.
 */
trait InjectorService {
  val injectRetainer = Retainer()
  var injectPorts = ArrayBuffer[Flow[Bits]]()
  def injectPort() = injectPorts.addRet(Flow(Decode.INSTRUCTION))
}