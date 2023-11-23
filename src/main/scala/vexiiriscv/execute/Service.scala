package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Lockable
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
}

trait ExecuteUnitService extends Lockable {
  def euName() : String
//  def pushPort() : ExecutionUnitPush
//  def staticLatencies() : ArrayBuffer[StaticLatency] = ArrayBuffer[StaticLatency]()
//  def addMicroOp(enc : MicroOp)

  def executeAt: Int
  def rfReadAt: Int
  def nodeAt(id : Int): Node
  def getMicroOp(): Seq[MicroOp]
  def getMicroOpSpecs(): Iterable[MicroOpSpec]
  def dispatchPriority : Int
  def insertNode : Node
  def getSpec(op : MicroOp) : MicroOpSpec

  val linkLock = new Lockable {}
}

case class CompletionPayload() extends Bundle{
  val hartId = Global.HART_ID()
  val microOpId = Decode.UOP_ID()
}
trait CompletionService{
  def getCompletions() : Seq[Flow[CompletionPayload]]
}