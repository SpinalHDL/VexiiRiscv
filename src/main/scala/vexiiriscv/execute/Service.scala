package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib._
import spinal.lib.logic.Masked
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{MicroOp, RegfileSpec, RfRead}

import scala.collection.mutable.ArrayBuffer


case class RdSpec(DATA: Payload[Bits],
                  rfReadableAt: Int,
                  bypassesAt : Seq[Int])

class MicroOpSpec(val op: MicroOp) {
  var rd = Option.empty[RdSpec]
  var completion = Option.empty[Int]
}

trait ExecuteUnitService {
  val uopLock = Lock()
  val pipelineLock = Lock()

  def euName() : String
//  def pushPort() : ExecutionUnitPush
//  def staticLatencies() : ArrayBuffer[StaticLatency] = ArrayBuffer[StaticLatency]()
//  def addMicroOp(enc : MicroOp)

  def executeAt: Int
  def rfReadAt: Int
  def ctrl(id : Int): CtrlLaneApi
  def getMicroOp(): Seq[MicroOp]
  def getMicroOpSpecs(): Iterable[MicroOpSpec]
  def dispatchPriority : Int
  def getSpec(op : MicroOp) : MicroOpSpec
}

case class CompletionPayload() extends Bundle{
  val hartId = Global.HART_ID()
  val microOpId = Decode.UOP_ID()
}

//case class RetirePayload() extends Bundle{
//  val hartId = Global.HART_ID()
//  val microOpId = Decode.UOP_ID()
//}

trait CompletionService{
  def getCompletions() : Seq[Flow[CompletionPayload]]
}

//trait RetireService{
//  def getRetires() : Seq[Flow[RetirePayload]]
//}