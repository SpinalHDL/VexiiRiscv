package vexiiriscv.prediction

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv._
import decode._
import fetch._
import schedule._
import riscv._
import Global._
import spinal.lib.logic.{DecodingSpec, Masked}
import vexiiriscv.prediction.Prediction.ALIGNED_JUMPED

/*
The job of this plugin is to prevent instruction which got a fetch prediction, but aren't actualy a branch, to continue in the pipeline
 */
class DecodePredictionPlugin(var decodeAt: Int,
                             var jumpAt: Int) extends FiberPlugin{
  lazy val dpp = host[DecodePipelinePlugin]
  lazy val pcp = host[PcService]
  lazy val rp = host[ReschedulePlugin]
  lazy val dp = host[DecoderService]
  lazy val hp = host.get[HistoryPlugin]
  buildBefore(dpp.elaborationLock)
  buildBefore(pcp.elaborationLock)
  setupRetain(rp.elaborationLock)
  during setup(hp.foreach(_.elaborationLock.retain()))

  val logic = during build new Area{
    val age = dpp.getAge(jumpAt, true)
    val pcPorts = List.fill(Decode.LANES)(pcp.createJumpInterface(age, log2Up(Decode.LANES), 0))
    val flushPorts = List.fill(Decode.LANES)(rp.newFlushPort(age, log2Up(Decode.LANES), true))
    val historyPorts = hp.map(hp => List.tabulate(Decode.LANES)(i => hp.createPort(age + i, 0)))
    rp.elaborationLock.release()
    hp.foreach(_.elaborationLock.release())

    val decodeSpec = new Area{
      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
      val jalKeys = List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key))
      val any = new DecodingSpec(Bool()).setDefault(Masked.zero)
      any.addNeeds(branchKeys ++ jalKeys, Masked.one)
    }

    val lanes = for (slotId <- 0 until Decode.LANES) yield new Area {
      val decoder = new dpp.LaneArea(decodeAt, slotId) {
        val IS_ANY = insert(decodeSpec.any.build(Decode.INSTRUCTION, dp.covers()))
      }

      //TODO Make BTB learn on this
      val fixer = new dpp.LaneArea(jumpAt, slotId){
        val fixIt = up.isValid && ALIGNED_JUMPED && !decoder.IS_ANY
        val fixed = RegInit(False) setWhen(fixIt) clearWhen(up.ready || up.cancel)

        val flushPort = flushPorts(slotId)
        flushPort.valid := fixIt
        flushPort.self := True // that way we don't have to calculate the next PC
        flushPort.hartId := HART_ID
        flushPort.uopId := Decode.UOP_ID //TODO naaaaa not realy good
        flushPort.laneAge := slotId //That may have been static instead ?

        val pcPort = pcPorts(slotId)
        pcPort.valid := fixIt
        pcPort.pc := PC
        pcPort.laneAge := slotId

        if(hp.nonEmpty) {
          val historyPort = historyPorts.get(slotId)
          historyPort.valid := fixIt
          historyPort.history := Prediction.BRANCH_HISTORY
        }
      }
    }
  }
}
