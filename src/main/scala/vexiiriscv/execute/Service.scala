package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Retainer, Lockable}
import spinal.idslplugin.Location
import spinal.lib._
import spinal.lib.logic.Masked
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{MicroOp, RD, RegfileSpec, RfAccess, RfRead, RfResource, RfWrite}
import vexiiriscv.schedule.{Ages, FlushCmd}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class RdSpec(DATA: Payload[Bits],
                  broadcastedFrom : Int,
                  rfReadableFrom : Int)

case class RsSpec(rs : RfRead){
  var from = 0
}

class LaneLayer(val name : String, val el : ExecuteLaneService, var priority : Int){
  val uops = mutable.LinkedHashMap[MicroOp, UopLayerSpec]()
  el.add(this)

  def apply(uop: MicroOp) = uops(uop)
  def add(uop: MicroOp) = uops.getOrElseUpdate(uop, new UopLayerSpec(uop, this, el))
  def laneName = el.laneName
  def getRsUseAtMin(): Int = {
    uops.flatMap(_._2.rs.map(_._2.from)).fold(100)(_ min _)
  }

  def doChecks(): Unit = {
    for(uop <- uops.values) uop.doCheck()
  }
}

class UopLayerSpec(val uop: MicroOp, val elImpl : LaneLayer, val el : ExecuteLaneService) {
  var rd = Option.empty[RdSpec]
  var rs = mutable.LinkedHashMap[RfRead, RsSpec]()
  var completion = Option.empty[Int]
  var mayFlushUpTo = Option.empty[Int]
  var dontFlushFrom = Option.empty[Int]
  val decodings = mutable.LinkedHashMap[Payload[_ <: BaseType], Masked]()

  def doCheck(): Unit = {
    uop.resources.foreach{
      case RfResource(_, rfRead: RfRead) => assert(rs.contains(rfRead), s"$elImpl $uop doesn't has the $rfRead specification set")
      case RfResource(_, rfWrite: RfWrite) => assert(rd.nonEmpty, s"$elImpl $uop doesn't has the rd specification set")
      case _ =>
    }
  }

  def addRsSpec(rfRead : RfRead, executeAt : Int) = {
    assert(!rs.contains(rfRead))
    val rsSpec = rs.getOrElseUpdate(rfRead, new RsSpec(rfRead))
    rsSpec.from = executeAt + el.executeAt
  }
  def setRdSpec(data: Payload[Bits], broadcastedFrom : Int, rfReadableFrom : Int): Unit = {
    assert(rd.isEmpty)
    rd = Some(RdSpec(data, broadcastedFrom + el.executeAt, rfReadableFrom + el.executeAt))
  }

  def addDecoding(head: (Payload[_ <: BaseType], Any), tail: (Payload[_ <: BaseType], Any)*): Unit = addDecoding(head :: tail.toList)
  def addDecoding(values: Seq[(Payload[_ <: BaseType], Any)]): Unit = {
    for ((key, value) <- values) {
      decodings(key) = Masked(value)
    }
  }

  def setCompletion(executeCtrlId: Int): this.type = {
    assert(completion.isEmpty)
    completion = Some(executeCtrlId + el.executeAt)
    this
  }

  def mayFlushUpTo(executeCtrlId: Int): Unit = {
    var at = executeCtrlId + el.executeAt
    mayFlushUpTo.foreach(v => v max at)
    mayFlushUpTo = Some(at)
  }

  def dontFlushFrom(executeCtrlId: Int): Unit = {
    var at = executeCtrlId + el.executeAt
    dontFlushFrom.foreach(v => v min at)
    dontFlushFrom = Some(at)
  }
}

trait ExecuteLaneService extends Area{
  val uopLock = Retainer()
  val pipelineLock = Retainer()

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
  def withBypasses : Boolean
  def rfReadHazardFrom(usedAt : Int) : Int
//  def newFlushPort(executeId : Int) : Flow[FlushCmd]

  def getStageable(r: RfResource): Payload[Bits]
  def apply(rf: RegfileSpec, access: RfAccess) = getStageable(rf -> access)
  def apply(r: RfResource) = getStageable(r)
//  def getSpec(op : MicroOp) : MicroOpSpec

  def getRdBroadcastedFromMax() = getUopLayerSpec().flatMap(s => s.rd.map(v => v.broadcastedFrom)).max
  def getRfReadableAtMax() = getUopLayerSpec().flatMap(s => s.rd.map(v => v.rfReadableFrom)).max

  val LAYER_SEL = Payload(Bits(log2Up(getLayers.size) bits))
  def getLayerId(ll : LaneLayer) = getLayers().toSeq.sortBy(_.priority).indexOf(ll)

  class Execute(id: Int) extends CtrlLaneMirror(execute(id))
  class Ctrl(id: Int) extends CtrlLaneMirror(ctrl(id))

  def getCtrlAge(at: Int): Int = Ages.EU + at * Ages.STAGE
  def getExecuteAge(at : Int) = getCtrlAge(at + executeAt)

  def freezeIt()(implicit loc: Location)
  def freezeWhen(cond: Bool)(implicit loc: Location)
  def isFreezed(): Bool
  def atRiskOfFlush(executeId : Int) : Bool
}

case class CompletionPayload() extends Bundle{
  val hartId = Global.HART_ID()
  val uopId = Decode.UOP_ID()
  val trap = Bool()
  val commit = Bool()
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