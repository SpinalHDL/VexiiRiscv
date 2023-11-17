package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.misc.{CtrlPipelinePlugin, PipelineService}
import vexiiriscv.riscv.{MicroOp, RegfileSpec, RfAccess, RfResource}
import vexiiriscv.schedule.DispatchPlugin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ExecuteUnitPlugin(val euId : String, val priority : Int) extends FiberPlugin with PipelineService with ExecuteUnitService {
  withPrefix(euId)

  override def euName(): String = euId

  override def getMicroOp(): Seq[MicroOp] = {
    lock.await()
    microOps
  }

  val microOps = ArrayBuffer[MicroOp]()
  def addMicroOp(op : MicroOp): Unit = {
    microOps += op
  }

  def setLatency(op : MicroOp, latency : Int): Unit = {

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
//  val robStageable = mutable.LinkedHashSet[SignalKey[_ <: Data]]()

  def apply(rf: RegfileSpec, access: RfAccess) = getStageable(rf -> access)
  def apply(r: RfResource) = getStageable(r)
  def getStageable(r: RfResource): SignalKey[Bits] = {
    rfStageables.getOrElseUpdate(r, SignalKey(Bits(r.rf.width bits)).setName(s"${r.rf.getName()}_${r.access.getName()}"))
  }


  val pipelineLock = new Lockable(){}
  override def getConnectors(): Seq[Connector] = logic.connectors
  val idToCtrl = mutable.LinkedHashMap[Int, CtrlConnector]()
  def ctrl(id : Int) = idToCtrl.getOrElseUpdate(id, CtrlConnector())
  def up = ctrl(0).up
  val logic = during build new Area{
    pipelineLock.await()
    val idMax = (0 +: idToCtrl.keys.toList).max
    for(i <- 0 to idMax) ctrl(i).unsetName() //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    val sc = for((from, to) <- (ctrls, ctrls.tail).zipped) yield new StageConnector(from.down, to.up) //.withoutCollapse()
    ctrls.last.down.setAlwaysReady()
    val connectors = (sc ++ ctrls).toSeq
  }
}
