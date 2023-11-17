package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Lockable
import spinal.lib._
import spinal.lib.logic.Masked
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{MicroOp, RegfileSpec, RfRead}

import scala.collection.mutable.ArrayBuffer

trait ExecuteUnitService extends Lockable {
  def euName() : String
//  def pushPort() : ExecutionUnitPush
//  def staticLatencies() : ArrayBuffer[StaticLatency] = ArrayBuffer[StaticLatency]()
//  def addMicroOp(enc : MicroOp)

  def getMicroOp(): Seq[MicroOp]
}