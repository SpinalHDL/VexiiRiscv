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

/**
 * Used to model the RISC-V RD usage of a given micro-op
 */
case class RdSpec(rf : RegfileSpec,
                  DATA: Payload[Bits],
                  broadcastedFrom : Int,
                  rfReadableFrom : Int)

/**
 * Used to model the RISC-V RS1/RS2 usage of a given micro-op
 */
case class RsSpec(rf : RegfileSpec,
                  rs : RfRead){
  var from = 0
}

/**
 * Used to model an execution lane layer.
 * For isntance if you have a dual issue pipeline with early and late ALUs, then you will have 4 LaneLayers
 *
 * So, this class will store the model of every micro-op that it supports, aswell as the timing that they have
 * (when do they use RS1/RS2, when do they provide a RD value, ..., by using the UopLayerSpec class.
 */
class LaneLayer(val name : String, val lane : ExecuteLaneService, var priority : Int){
  val uops = mutable.LinkedHashMap[MicroOp, UopLayerSpec]()
  lane.add(this)

  def apply(uop: MicroOp) = uops(uop)
  def add(uop: MicroOp) = uops.getOrElseUpdate(uop, new UopLayerSpec(uop, this, lane))
  def laneName = lane.laneName
  def getRsUseAtMin(): Int = {
    uops.flatMap(_._2.rs.map(_._2.from)).fold(100)(_ min _)
  }

  def doChecks(): Unit = {
    for(uop <- uops.values) uop.doCheck()
  }
  class Execute(id: Int) extends CtrlLaneMirror(lane.execute(id))
}

/**
 * Specifies how a given MicroOp is implemented in a given LaneLayer
 * - RD/RS1/RS2 timings and usages
 * - Completion timing
 * - Flush supported/behavior
 * - Additional decoding required
 * - Shared hardware reservations
 */
class UopLayerSpec(val uop: MicroOp, val elImpl : LaneLayer, val el : ExecuteLaneService) {
  var rdOutOfPip = false
  var rd = Option.empty[RdSpec]
  var rs = mutable.LinkedHashMap[RfRead, RsSpec]()
  var completion = Option.empty[Int]
  var mayFlushUpTo = Option.empty[Int]
  var dontFlushFrom = Option.empty[Int]
  val decodings = mutable.LinkedHashMap[Payload[_ <: BaseType], Masked]()
  val reservations = mutable.LinkedHashMap[Nameable, mutable.LinkedHashSet[Int]]()

  def reserve(what : Nameable, at : Int) = {
    val spec = reservations.getOrElseUpdate(what, mutable.LinkedHashSet[Int]())
    spec += at+el.executeAt
  }

  def doCheck(): Unit = {
    uop.resources.foreach{
      case RfResource(_, rfRead: RfRead) => assert(rs.contains(rfRead), s"$elImpl $uop doesn't has the $rfRead specification set")
      case RfResource(_, rfWrite: RfWrite) => assert(rdOutOfPip || rd.nonEmpty, s"$elImpl $uop doesn't has the rd specification set")
      case _ =>
    }
  }

  def addRsSpec(rfRead : RfRead, executeAt : Int) = {
    assert(!rs.contains(rfRead), s"$rfRead was already added for the spec of $uop")
    val rf = uop.resources.collectFirst{
      case r : RfResource if r.access == rfRead => r.rf
    }.get
    val rsSpec = rs.getOrElseUpdate(rfRead, new RsSpec(rf, rfRead))
    rsSpec.from = executeAt + el.executeAt
  }
  def setRdSpec(data: Payload[Bits], broadcastedFrom : Int, rfReadableFrom : Int): Unit = {
    assert(rd.isEmpty)
    val rf = uop.resources.collectFirst {
      case r: RfResource if r.access == RD => r.rf
    }.get
    rd = Some(RdSpec(rf, data, broadcastedFrom + el.executeAt, rfReadableFrom + el.executeAt))
  }

  def addDecoding(head: (Payload[_ <: BaseType], Any), tail: (Payload[_ <: BaseType], Any)*): Unit = addDecoding(head :: tail.toList)
  def addDecoding(values: Seq[(Payload[_ <: BaseType], Any)]): Unit = {
    for ((key, value) <- values) {
      decodings(key) = Masked(value)
    }
  }

  /** Set from which point in the pipeline the instruction can be considered as done */ 
  def setCompletion(executeCtrlId: Int): this.type = {
    assert(completion.isEmpty, s"completion is already set at ${completion.get} for $uop")
    completion = Some(executeCtrlId + el.executeAt)
    this
  }

  /** Set until which point in the pipeline the instruction may flush younger instructions */
  def mayFlushUpTo(executeCtrlId: Int): Unit = {
    var at = executeCtrlId + el.executeAt
    mayFlushUpTo.foreach(v => v max at)
    mayFlushUpTo = Some(at)
  }

  /** Set from which point in the pipeline the instruction should not be flushed anymore because it already had produced side effects */
  def dontFlushFrom(executeCtrlId: Int): Unit = {
    var at = executeCtrlId + el.executeAt
    dontFlushFrom.foreach(v => v min at)
    dontFlushFrom = Some(at)
  }
}

/**
 * Provide an API to access an execution lanes.
 */
trait ExecuteLaneService extends Area {
  val uopLock = Retainer()
  val pipelineLock = Retainer()

  // Query the model of the execute lane.
  def getUops(): Iterable[MicroOp]
  def getUopLayerSpec(): Iterable[UopLayerSpec]
  def getLayers(): Iterable[LaneLayer]

  // Feed the execute lane model with additional information
  def add(layer : LaneLayer) : Unit
  def setDecodingDefault(key: Payload[_ <: BaseType], value: BaseType)

  // API which allows to get the pipeline Payload to access the RS1/RS2 hardware values
  def getStageable(r: RfResource): Payload[Bits]
  def apply(rf: RegfileSpec, access: RfAccess) = getStageable(rf -> access)
  def apply(r: RfResource) = getStageable(r)

  def getRdBroadcastedFromMax(regFiles : Seq[RegfileSpec]) = getUopLayerSpec().filter(e => e.rd.nonEmpty && regFiles.contains(e.rd.get.rf) ).flatMap(s => s.rd.map(v => v.broadcastedFrom)).max
  def getRfReadableAtMax() = getUopLayerSpec().flatMap(s => s.rd.map(v => v.rfReadableFrom)).max

  val LAYER_SEL = Payload(Bits(log2Up(getLayers.size) bits))
  def getLayerId(ll : LaneLayer) = getLayers().toSeq.sortBy(_.priority).indexOf(ll)

  // Get an API to access a given stage of the pipeline
  // ctrl is the raw API, where id 0 map the stage directly connected to the dispatcher
  def ctrl(id: Int): CtrlLaneApi
  // execute provide the pipeline API offseted to were the ALU plugins should start to operate.
  def execute(id: Int): CtrlLaneApi

  // Create Area which implicitly work in a given stage of the execute lane
  class Execute(id: Int) extends CtrlLaneMirror(execute(id))
  class Ctrl(id: Int) extends CtrlLaneMirror(ctrl(id))

  // Trap priority computing
  def getCtrlAge(at: Int): Int = Ages.EXECUTE + at * Ages.STAGE
  def getExecuteAge(at : Int) = getCtrlAge(at + executeAt)

  // Flow control API
  def freezeIt()(implicit loc: Location)
  def freezeWhen(cond: Bool)(implicit loc: Location)
  def isFreezed(): Bool
  def atRiskOfFlush(executeId : Int) : Bool

  // Various API
  def laneName : String
  def executeAt: Int // Stage id at which the ALU are meant to start operate.
  def rfReadAt: Int
  def rfReadLatencyMax : Int
  def withBypasses : Boolean
  def rfReadHazardFrom(usedAt : Int) : Int

}

case class CompletionPayload() extends Bundle {
  val hartId = Global.HART_ID()
  val uopId = Decode.UOP_ID()
  val trap = Bool()
  val commit = Bool()
}

/**
 * Allows a plugin to publish a list of interface which provide micro-op completion notifications
 */
trait CompletionService{
  def getCompletions() : Seq[Flow[CompletionPayload]]
}
