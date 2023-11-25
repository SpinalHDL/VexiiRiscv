package vexiiriscv.schedule

import spinal.core.{Bool, UInt}
import spinal.lib._
import spinal.lib.misc.pipeline.CtrlLink
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.fetch.PcService
import vexiiriscv.misc.PipelineBuilderPlugin

import scala.collection.mutable.ArrayBuffer

class ReschedulePlugin extends FiberPlugin with ScheduleService {
  buildBefore(host[PcService].elaborationLock)
  buildBefore(host[PipelineBuilderPlugin].elaborationLock)

  val flushPorts = ArrayBuffer[Flow[FlushCmd]]()
  val trapPorts  = ArrayBuffer[Flow[TrapCmd]]()
  val ctrls      = ArrayBuffer[CtrlSpec]()

  case class CtrlSpec(ctrl : CtrlLink)

  override def newPcPort(age: Int, aggregationPriority: Int = 0) = host[PcService].createJumpInterface(age, aggregationPriority)
  override def newFlushPort(age: Int, laneAgeWidth : Int, withUopId : Boolean) = flushPorts.addRet(Flow(FlushCmd(age, laneAgeWidth, withUopId)))
  override def newTrapPort(age : Int, causeWidth : Int = 4) = trapPorts.addRet(Flow(TrapCmd(age, Global.PC_WIDTH, Global.TVAL_WIDTH, causeWidth)))
  override def isFlushedAt(age: Int, hartId: UInt, laneAge : UInt): Option[Bool] = {
    elaborationLock.await()
    val filtred = flushPorts.filter(p => p.age >= age)
    if(filtred.isEmpty) return None
    val hits = filtred.map(p => p.age match {
      case `age` => p.valid && p.hartId === hartId && (p.laneAge < laneAge || p.laneAge === laneAge && p.self)
      case _ => p.valid && p.hartId === hartId
    })
    Some(hits.orR)
  }
}
