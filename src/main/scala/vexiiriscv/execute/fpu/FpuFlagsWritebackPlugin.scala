package vexiiriscv.execute.fpu

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.{misc, _}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.regfile.{RegFileWriter, RegFileWriterService, RegfileService}
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class FpuFlagsWritebackPort(atsSpec : Seq[Int]) extends Bundle{
  val flags = FpuFlags()
  val ats = Bits(atsSpec.size bits)
}

class FpuFlagsWritebackPlugin(val lane : ExecuteLaneService, pipTo : Int) extends FiberPlugin{
  val elaborationLock = Retainer()

  case class Spec(port : FpuFlagsWritebackPort, ats : Seq[Int]){
    var uops = ArrayBuffer[MicroOp]()
  }

  val portToSpec = mutable.LinkedHashMap[FpuFlagsWritebackPort, Spec]()
  def createPort(ats: Seq[Int]): FpuFlagsWritebackPort = {
    val port = FpuFlagsWritebackPort(ats)
    portToSpec(port) = Spec(port, ats)
    port
  }

  val hazardSels = mutable.LinkedHashMap[Int, Payload[Bool]]()
  def addUop(port : FpuFlagsWritebackPort, uop : UopLayerSpec, at : Int): Unit = {
    if(at > pipTo){
      val sel = hazardSels.getOrElseUpdate(at, {
        val v = Payload(Bool()).setCompositeName(this, s"_SEL_" + at)
        lane.setDecodingDefault(v, False)
        v
      })
      uop.addDecoding(sel -> True, FpuCsrPlugin.DIRTY -> True)
    }
    if(at <= pipTo){
      uop.dontFlushFrom(pipTo)
    }
  }

  val logic = during setup new Area{
    val cp = host[CsrService]
    val fcp = host[FpuCsrPlugin]
    val buildBefore = retains(lane.pipelineLock, cp.csrLock)

    awaitBuild()
    elaborationLock.await()

    val FLAGS = Payload(FpuFlags())

    val specs = portToSpec.values
    val specsAts = specs.flatMap(_.ats).distinctLinked.toList.sorted
    val flagsOrInputs = ArrayBuffer[FpuFlags]()
    val beforeCommit = specsAts.filter(_ <= pipTo)
    if(beforeCommit.nonEmpty) {
      for(at <- beforeCommit){
        val ctrl = lane.execute(at)
        val ports = for(spec <- specs; (specAt, atId) <- spec.ats.zipWithIndex; if specAt == at) yield spec.port.flags.andMask(spec.port.ats(atId))
        val reduced = ports.reduceBalancedTree(_ | _)
        val first = at == beforeCommit.head
        first match {
          case true => ctrl.up(FLAGS) := reduced
          case false => ctrl.bypass(FLAGS) := ctrl.up(FLAGS) | reduced
        }
      }
      new lane.Execute(pipTo) {
        flagsOrInputs += FLAGS.andMask(isValid && up(Global.COMMIT)) //Ok because uop.dontFlushFrom(pipTo)
      }
    }

    val afterCommit = for(spec <- specs; ats = spec.ats.zipWithIndex.filter(_._1 > pipTo); if ats.nonEmpty) yield {
      flagsOrInputs += spec.port.flags.andMask(ats.map { e =>
        val exe = lane.execute(e._1)
        exe.isValid && exe.up(Global.COMMIT) && spec.port.ats(e._2)
      }.orR)
    }

    buildBefore.release()

    val flagsOr = flagsOrInputs.reduceBalancedTree(_ | _)
    cp.waitElaborationDone() //Ensure that CSR flags set is done last. This relax timings
    when(!lane.isFreezed()) {
      fcp.api.flags.NV.setWhen(flagsOr.NV)
      fcp.api.flags.DZ.setWhen(flagsOr.DZ)
      fcp.api.flags.OF.setWhen(flagsOr.OF)
      fcp.api.flags.UF.setWhen(flagsOr.UF)
      fcp.api.flags.NX.setWhen(flagsOr.NX)
    }
  }
}
