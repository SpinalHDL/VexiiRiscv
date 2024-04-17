package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute._
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class FpuPackerCmd(p : FloatUnpackedParam) extends Bundle{
  val valid = Bool()
  val value = FloatUnpacked(p)
}

case class FpuPackerPort(cmd : FpuPackerCmd, at : Int){
  val uops = ArrayBuffer[UopLayerSpec]()
}

class FpuPackerPlugin(lane: ExecuteLanePlugin) extends FiberPlugin{
  val p = FpuUtils

  val elaborationLock = Retainer()

  val ports = ArrayBuffer[FpuPackerPort]()
  def createPort(at : Int, p : FloatUnpackedParam): FpuPackerPort = {
    ports.addRet(FpuPackerPort(FpuPackerCmd(p), at))
  }

  val logic = during setup new Area{
    val wbp = host.find[WriteBackPlugin](_.laneName == lane.laneName)
    val buildBefore = retains(lane.pipelineLock, wbp.elaborationLock)
    awaitBuild()
    val latency = ???

    elaborationLock.await()

    val wbPorts = mutable.LinkedHashMap[Int, Flow[Bits]]()
    val atGroups = ports.groupBy(_.at)
    for((at, l) <- atGroups) {
      val port = wbp.createPort(at+latency)
      wbPorts(at) = port
      for(e <- l; uop <- e.uops) wbp.addMicroOp(port, uop)
    }

    buildBefore.release()
  }
}
