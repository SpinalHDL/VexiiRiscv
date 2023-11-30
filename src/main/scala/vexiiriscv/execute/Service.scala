package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.idslplugin.Location
import spinal.lib._
import spinal.lib.logic.Masked
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{MicroOp, RD, RegfileSpec, RfRead, RfResource}

import scala.collection.mutable.ArrayBuffer


case class RdSpec(DATA: Payload[Bits],
                  rfReadableAt: Int,
                  bypassesAt : Seq[Int])

class MicroOpSpec(val op: MicroOp) {
  var rd = Option.empty[RdSpec]
  var completion = Option.empty[Int]
  var mayFlushUpTo = Option.empty[Int]
  var dontFlushFrom = Option.empty[Int]
}

trait ExecuteLaneService {
  val uopLock = Lock()
  val pipelineLock = Lock()

  def laneName() : String
//  def pushPort() : ExecutionUnitPush
//  def staticLatencies() : ArrayBuffer[StaticLatency] = ArrayBuffer[StaticLatency]()
//  def addMicroOp(enc : MicroOp)

  def executeAt: Int
  def rfReadAt: Int
  def rfReadLatencyMax : Int
  def ctrl(id : Int): CtrlLaneApi
  def getMicroOp(): Seq[MicroOp]
  def getMicroOpSpecs(): Iterable[MicroOpSpec]
  def dispatchPriority : Int
  def getSpec(op : MicroOp) : MicroOpSpec

  def getRdReadableAtMax() = getMicroOpSpecs().filter(_.op.resources.exists{
    case RfResource(_, `RD`) => true
    case _ => false
  }).map(_.rd.get.rfReadableAt).max
}

case class CompletionPayload() extends Bundle{
  val hartId = Global.HART_ID()
  val uopId = Decode.UOP_ID()
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