package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.PcService
import vexiiriscv.schedule.ReschedulePlugin

import scala.collection.mutable.ArrayBuffer

class RedoPlugin(val laneName : String) extends FiberPlugin {
  lazy val elp = host.find[ExecuteLaneService](_.laneName == laneName)
  lazy val pcs = host[PcService]
  lazy val sp = host[ReschedulePlugin]

  buildBefore(pcs.elaborationLock)
  buildBefore(elp.pipelineLock)
  buildBefore(sp.elaborationLock)

  case class Spec(ctrlAt : Int, request : Bool)
  val specs = ArrayBuffer[Spec]()
  def newPort(executeAt : Int) : Bool = specs.addRet(Spec(executeAt + elp.executeAt, Bool())).request

  val elaborationLock = Lock()
  val logic = during build new Area{
    elaborationLock.await()

    val groups = specs.groupBy(_.ctrlAt)
    val groupsLogic = for((ctrlAt, specs) <- groups) yield new elp.Ctrl(ctrlAt){
      val pcPort = pcs.createJumpInterface(elp.getAge(ctrlAt), Execute.LANE_AGE_WIDTH, aggregationPriority = 0)
      val flushPort = sp.newFlushPort(elp.getExecuteAge(ctrlAt), laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
      val doIt = specs.map(_.request).orR

      pcPort.valid := doIt
      pcPort.pc := Global.PC
      pcPort.laneAge := Execute.LANE_AGE

//      historyPort.foreach { port =>
//        port.valid := doIt
//        port.history := history.next
//      }


      flushPort.valid := doIt
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := True
    }
  }
}
