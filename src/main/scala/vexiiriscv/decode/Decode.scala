package vexiiriscv.decode

import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline.SignalKey
import vexiiriscv.Global
import vexiiriscv.riscv.{RegfileSpec, RfAccess}

import java.util
import scala.collection.mutable

object Decode extends AreaRoot{
  val LANES  = blocking[Int]
  val INSTRUCTION_WIDTH = blocking[Int]

  val INSTRUCTION = SignalKey(Bits(INSTRUCTION_WIDTH bits))
  val MICRO_OP = SignalKey(Bits(INSTRUCTION_WIDTH bits))
  val ALIGNED_MASK = SignalKey(Bool())

  val rfaKeys = blocking[mutable.LinkedHashMap[RfAccess, AccessKeys]]

  val LEGAL = SignalKey(Bool())
}

case class AccessKeys(physWidth : Int, rfMapping : Seq[RegfileSpec]) extends Area{
  val rfIdWidth = log2Up(rfMapping.size)
  def is(rfs: RegfileSpec, that: UInt) = that === idOf(rfs)
  def idOf(rfs: RegfileSpec) = rfMapping.indexOf(rfs)

  val ENABLE = SignalKey(Bool())
  val PHYS = SignalKey(UInt(physWidth bits))
  val RFID = SignalKey(UInt(rfIdWidth bits))
}