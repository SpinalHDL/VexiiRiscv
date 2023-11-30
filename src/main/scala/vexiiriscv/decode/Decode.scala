package vexiiriscv.decode

import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.Global
import vexiiriscv.fetch.Fetch
import vexiiriscv.riscv.{RegfileSpec, RfAccess}

import java.util
import scala.collection.mutable

object Decode extends AreaObject {
  val LANES  = blocking[Int]
  val INSTRUCTION_WIDTH = blocking[Int]
  val DOP_ID_WIDTH = blocking[Int]
  val UOP_ID_WIDTH = blocking[Int]

  val INSTRUCTION = Payload(Bits(INSTRUCTION_WIDTH bits))
  val UOP = Payload(Bits(INSTRUCTION_WIDTH bits))

  val rfaKeys = blocking[mutable.LinkedHashMap[RfAccess, AccessKeys]]

  val LEGAL = Payload(Bool())
  val DOP_ID = Payload(UInt(DOP_ID_WIDTH bits))
  val UOP_ID = Payload(UInt(UOP_ID_WIDTH bits))
  def laneIds = 0 until LANES

  val INSTRUCTION_SLICE_COUNT = Payload(UInt(log2Up(INSTRUCTION_WIDTH/Fetch.SLICE_WIDTH) bits)) // minus one => RVC => 0, normal => 1
}

case class AccessKeys(physWidth : Int, rfMapping : Seq[RegfileSpec]) extends Area{
  val rfIdWidth = log2Up(rfMapping.size)
  def is(rfs: RegfileSpec, that: UInt) = that === idOf(rfs)
  def idOf(rfs: RegfileSpec) = rfMapping.indexOf(rfs)

  val ENABLE = Payload(Bool())
  val PHYS = Payload(UInt(physWidth bits))
  val RFID = Payload(UInt(rfIdWidth bits))
  def ARCH = PHYS
}