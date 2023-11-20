package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.decode.Decode
import vexiiriscv.misc.{CtrlPipelinePlugin, PipelineService}
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{MicroOp, RegfileSpec, RfAccess, RfRead, RfResource}
import vexiiriscv.schedule.DispatchPlugin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ExecuteUnitPlugin(val euId : String, val priority : Int) extends FiberPlugin with PipelineService with ExecuteUnitService {
  withPrefix(euId)

  during setup {
    host.list[RegfileService].foreach(_.retain())
  }

  override def euName(): String = euId
  override def insertNode: Node = logic.ctrls.head.up
  override def dispatchPriority: Int = priority
  override def getMicroOp(): Seq[MicroOp] = {
    lock.await()
    microOps.keys.toSeq
  }

  class MicroOpSpec(val microOp : MicroOp){
    var latency = Option.empty[Int]
  }
  val microOps = mutable.LinkedHashMap[MicroOp, MicroOpSpec]()
  def addMicroOp(op : MicroOp): Unit = {
    microOps.getOrElseUpdate(op, new MicroOpSpec(op))
  }

  def setLatency(op : MicroOp, latency : Int): Unit = {
    microOps(op).latency = Some(latency)
  }

  def setDecodingDefault(key: SignalKey[_ <: BaseType], value: BaseType): Unit = {
    getDecodingSpec(key).setDefault(Masked(value))
  }

  def addDecoding(microOp: MicroOp, values: Seq[(SignalKey[_ <: BaseType], Any)]): Unit = {
    val op = Masked(microOp.key)
    for ((key, value) <- values) {
      getDecodingSpec(key).addNeeds(op, Masked(value))
    }
  }

  def getDecodingSpec(key: SignalKey[_ <: BaseType]) = decodingSpecs.getOrElseUpdate(key, new DecodingSpec(key))
  val decodingSpecs = mutable.LinkedHashMap[SignalKey[_ <: BaseType], DecodingSpec[_ <: BaseType]]()

  val rfStageables = mutable.LinkedHashMap[RfResource, SignalKey[Bits]]()

  def apply(rf: RegfileSpec, access: RfAccess) = getStageable(rf -> access)
  def apply(r: RfResource) = getStageable(r)
  def getStageable(r: RfResource): SignalKey[Bits] = {
    rfStageables.getOrElseUpdate(r, SignalKey(Bits(r.rf.width bits)).setName(s"${r.rf.getName()}_${r.access.getName()}"))
  }


  val pipelineLock = new Lockable(){}
  override def getConnectors(): Seq[Connector] = logic.connectors
  val idToCtrl = mutable.LinkedHashMap[Int, CtrlConnector]()
  def ctrl(id : Int) = idToCtrl.getOrElseUpdate(id, CtrlConnector().setCompositeName(this, if(id >= 0) "exe" + id else "dis" + -(id + 1)))
  def execute(id: Int) = {
    assert(id >= 0)
    ctrl(id)
  }
  val logic = during build new Area{
    pipelineLock.await()

    val specs = microOps.values
    val resources = specs.flatMap(_.microOp.resources).distinctLinked

    val decodeAt = -1
    val decodeCtrl = ctrl(decodeAt)
    val decoding = new decodeCtrl.Area {
      val coverAll = getMicroOp().map(e => Masked(e.key))
      for ((key, spec) <- decodingSpecs) {
        key.assignFromBits(spec.build(Decode.MICRO_OP, coverAll).asBits)
      }
    }

    val rf = new Area{
      val rfSpecs = rfStageables.keys.map(_.rf).distinctLinked
      val rfPlugins = rfSpecs.map(spec => host.find[RegfileService](_.rfSpec == spec))
      val readLatencyMax = rfPlugins.map(_.readLatency).max
      val readAt = -(readLatencyMax+1)
      val readCtrl = ctrl(readAt)

      val reads = for((spec, payload) <- rfStageables) yield new Area{
        val rfa = Decode.rfaKeys.get(spec.access)
        val rfPlugin = host.find[RegfileService](_.rfSpec == spec.rf)
        val port = rfPlugin.newRead(false)
        port.valid := readCtrl.isValid && readCtrl(rfa.ENABLE) && rfa.is(spec.rf, readCtrl(rfa.RFID))
        port.address := readCtrl(rfa.PHYS)

        ctrl(readAt + rfPlugin.readLatency)(payload) := port.data
      }
    }

    val idMax = (0 +: idToCtrl.keys.toList).max
    for(i <- 0 to idMax) ctrl(i) //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    val sc = for((from, to) <- (ctrls, ctrls.tail).zipped) yield new StageConnector(from.down, to.up).withoutCollapse()
    ctrls.last.down.setAlwaysReady()
    val connectors = (sc ++ ctrls).toSeq
    host.list[RegfileService].foreach(_.release())
  }
}
