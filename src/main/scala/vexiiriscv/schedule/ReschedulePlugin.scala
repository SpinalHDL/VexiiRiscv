package vexiiriscv.schedule

import spinal.core._
import spinal.core.fiber.RetainerGroup
import spinal.lib._
import spinal.lib.misc.pipeline.CtrlLink
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.fetch.PcService
import vexiiriscv.misc.PipelineBuilderPlugin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * The ReschedulePlugin act as a arbiter for all the different plugins which want to reschedule what the CPU should execute,
 * aswell as a registry for all the plugins which need to know if the CPU is being flushed until a given point in the pipeline.
 *
 */
class ReschedulePlugin extends FiberPlugin with ScheduleService {
  val flushPortsShared = mutable.LinkedHashMap[Nameable, Flow[FlushCmd]]()
  val flushPorts = ArrayBuffer[Flow[FlushCmd]]()
  val ctrls      = ArrayBuffer[CtrlSpec]()

  case class CtrlSpec(ctrl : CtrlLink)

  override def newFlushPort(age: Int, laneAgeWidth : Int, withUopId : Boolean) = flushPorts.addRet(Flow(FlushCmd(age, laneAgeWidth, withUopId)))
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


  val logic = during build new Area{
    val ps = host[PcService]
    val pbp = host[PipelineBuilderPlugin]
    val retainer = retains(ps.elaborationLock, pbp.elaborationLock)
    elaborationLock.await()
    retainer.release()
  }
}
