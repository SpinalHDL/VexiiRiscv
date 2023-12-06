package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.idslplugin.Location
import spinal.lib._
import spinal.lib.logic.Masked
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{MicroOp, RD, RegfileSpec, RfAccess, RfRead, RfResource}
import vexiiriscv.schedule.Ages

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class RdSpec(DATA: Payload[Bits],
                  rfReadableAt: Int,
                  bypassesAt : Seq[Int])

case class RsSpec(rs : RfRead, from : Int)

class LaneLayer(val name : String, val el : ExecuteLaneService, var priority : Int){
  val uops = mutable.LinkedHashMap[MicroOp, UopLayerSpec]()
  el.add(this)

  def apply(uop: MicroOp) = uops(uop)
  def add(uop: MicroOp) = uops.getOrElseUpdate(uop, new UopLayerSpec(uop, this, el))
  def laneName = el.laneName
}

class UopLayerSpec(val uop: MicroOp, val elImpl : LaneLayer, val el : ExecuteLaneService) {
  var rd = Option.empty[RdSpec]
  var rs = ArrayBuffer[RsSpec]()
  var completion = Option.empty[Int]
  var mayFlushUpTo = Option.empty[Int]
  var dontFlushFrom = Option.empty[Int]

  val decodings = mutable.LinkedHashMap[Payload[_ <: BaseType], Masked]()

  def setRdSpec(data: Payload[Bits], rfReadableAt: Int, bypassesAt: Seq[Int]): Unit = {
    assert(rd.isEmpty)
    rd = Some(RdSpec(data, rfReadableAt + el.executeAt, bypassesAt.map(_ + el.executeAt)))
  }

  def addDecoding(head: (Payload[_ <: BaseType], Any), tail: (Payload[_ <: BaseType], Any)*): Unit = addDecoding(head :: tail.toList)
  def addDecoding(values: Seq[(Payload[_ <: BaseType], Any)]): Unit = {
    for ((key, value) <- values) {
      decodings(key) = Masked(value)
    }
  }

  def setCompletion(executeCtrlId: Int): Unit = {
    assert(completion.isEmpty)
    completion = Some(executeCtrlId + el.executeAt)
  }

  def mayFlushUpTo(executeCtrlId: Int): Unit = {
    assert(mayFlushUpTo.isEmpty)
    mayFlushUpTo = Some(executeCtrlId + el.executeAt)
  }

  def dontFlushFrom(executeCtrlId: Int): Unit = {
    assert(dontFlushFrom.isEmpty)
    dontFlushFrom = Some(executeCtrlId + el.executeAt)
  }
}

trait ExecuteLaneService {
  val uopLock = Lock()
  val pipelineLock = Lock()

  def laneName : String
//  def pushPort() : ExecutionUnitPush
//  def staticLatencies() : ArrayBuffer[StaticLatency] = ArrayBuffer[StaticLatency]()
//  def addMicroOp(enc : MicroOp)

  def executeAt: Int
  def rfReadAt: Int
  def rfReadLatencyMax : Int
  def ctrl(id: Int): CtrlLaneApi
  def execute(id: Int): CtrlLaneApi
  def getUops(): Iterable[MicroOp]
  def getUopLayerSpec(): Iterable[UopLayerSpec]
  def getLayers(): Iterable[LaneLayer]
  def add(layer : LaneLayer) : Unit
  def setDecodingDefault(key: Payload[_ <: BaseType], value: BaseType)

  def getStageable(r: RfResource): Payload[Bits]
  def apply(rf: RegfileSpec, access: RfAccess) = getStageable(rf -> access)
  def apply(r: RfResource) = getStageable(r)
//  def getSpec(op : MicroOp) : MicroOpSpec

//  def getRdReadableAtMax() = getMicroOpSpecs().filter(_.op.resources.exists{
//    case RfResource(_, `RD`) => true
//    case _ => false
//  }).map(_.rd.get.rfReadableAt).max

  val LAYER_SEL = Payload(Bits(log2Up(getLayers.size) bits))
  def getLayerId(ll : LaneLayer) = getLayers().toSeq.sortBy(_.priority).indexOf(ll)

  class Execute(id: Int) extends CtrlLaneMirror(execute(id))
  class Ctrl(id: Int) extends CtrlLaneMirror(ctrl(id))

  def getAge(at: Int, prediction: Boolean): Int = Ages.EU + at * Ages.STAGE + prediction.toInt * Ages.PREDICTION
  def getExecuteAge(at : Int) = getAge(at + executeAt, false)

  def freezeWhen(cond: Bool)(implicit loc: Location)
  def isFreezed(): Bool
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