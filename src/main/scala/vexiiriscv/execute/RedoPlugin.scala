package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.PcService
import vexiiriscv.prediction.{HistoryPlugin, Prediction}
import vexiiriscv.schedule.ReschedulePlugin

import scala.collection.mutable.ArrayBuffer

class RedoPlugin(val laneName : String) extends FiberPlugin {
  lazy val elp = host.find[ExecuteLaneService](_.laneName == laneName)
  case class Spec(ctrlAt : Int, request : Bool)
  val specs = ArrayBuffer[Spec]()
  def newPort(executeAt : Int) : Bool = specs.addRet(Spec(executeAt + elp.executeAt, Bool())).request


  val elaborationLock = Retainer()
  val logic = during setup new Area{
    val pcs = host[PcService]
    val sp = host[ReschedulePlugin]
    val hp = host.get[HistoryPlugin]
    val buildBefore = retains(hp.map(_.elaborationLock).toList :+ pcs.elaborationLock :+ elp.pipelineLock :+ sp.elaborationLock)
    awaitBuild()

    elaborationLock.await()

    val groups = specs.groupBy(_.ctrlAt)
    val groupsLogic = for((ctrlAt, specs) <- groups) yield new elp.Ctrl(ctrlAt){
      val age = elp.getCtrlAge(ctrlAt)
      val pcPort = pcs.newJumpInterface(age, Execute.LANE_AGE_WIDTH, aggregationPriority = 0)
      val flushPort = sp.newFlushPort(age, laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
      val historyPort = hp.map(_.newPort(age, Execute.LANE_AGE_WIDTH))
      val doIt = specs.map(_.request).orR

      pcPort.valid := doIt
      pcPort.pc := Global.PC
      pcPort.laneAge := Execute.LANE_AGE

      historyPort.foreach { port =>
        port.valid := doIt
        port.history := Prediction.BRANCH_HISTORY
        port.age := Execute.LANE_AGE
      }


      flushPort.valid := doIt
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := True
    }
    buildBefore.release()
  }
}
