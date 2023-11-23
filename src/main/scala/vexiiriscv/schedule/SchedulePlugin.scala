package vexiiriscv.schedule

import spinal.lib.Flow
import spinal.lib.misc.pipeline.CtrlLink
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.fetch.PcService
import vexiiriscv.misc.{PipelineBuilderPlugin, ScheduleService, TrapCmd}

import scala.collection.mutable.ArrayBuffer

class SchedulePlugin extends FiberPlugin with ScheduleService {
  addLockable(host[PcService])
  addLockable(host[PipelineBuilderPlugin])

  val flushPorts = ArrayBuffer[Flow[FlushCmd]]()
  val trapPorts  = ArrayBuffer[Flow[TrapCmd]]()
  val ctrls      = ArrayBuffer[CtrlSpec]()

  case class CtrlSpec(ctrl : CtrlLink)

  override def newPcPort(age: Int, aggregationPriority: Int = 0) = host[PcService].createJumpInterface(age, aggregationPriority)
  override def newFlushPort(age: Int) = flushPorts.addRet(Flow(FlushCmd(age)))
  override def newTrapPort(age : Int, causeWidth : Int = 4) = trapPorts.addRet(Flow(TrapCmd(age, Global.PC_WIDTH, Global.TVAL_WIDTH, causeWidth)))
  override def addCtrl(age: Int, ctrl: CtrlLink): Unit = ???
}
