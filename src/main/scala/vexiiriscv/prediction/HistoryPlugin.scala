package vexiiriscv.prediction

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.fetch.FetchPipelinePlugin
import Prediction._
import vexiiriscv.Global

import scala.collection.mutable


case class HistoryJump(laneAgeWidth : Int) extends Bundle{
  val history = BRANCH_HISTORY()
  val age = UInt(laneAgeWidth bits)
  //TODO HART ID
}

//TODO a few history port may be removed to save area, as they are corner case  : DecodePredictionPlugin, and eventualy Lsu io missprediction
class HistoryPlugin(var historyFetchBypass : Boolean = true) extends FiberPlugin {


  case class HistorySpec(priority : Int, laneAgeWidth : Int, port : Flow[HistoryJump])
  val historySpecs = mutable.ArrayBuffer[HistorySpec]()
  def newPort(priority : Int, laneAgeWidth : Int): Flow[HistoryJump] = {
    historySpecs.addRet(HistorySpec(priority, laneAgeWidth, Flow(HistoryJump(laneAgeWidth)))).port
  }

  val elaborationLock = Retainer()
  val logic = during setup new Area{
    val fpp = host[FetchPipelinePlugin]
    val buildBefore = retains(fpp.elaborationLock)
    awaitBuild()

    elaborationLock.await()

    val fetchUsages = host.list[FetchConditionalPrediction]
    val fetchInsertAt = if(fetchUsages.nonEmpty) fetchUsages.map(_.useHistoryAt).min else 0

    assert(Global.HART_COUNT.get == 1)
    val onFetch = new Area{
      val value = Reg(BRANCH_HISTORY) init(0)
      val valueNext = CombInit(value)
      value := valueNext

      val groups = historySpecs.groupBy(_.priority).toSeq.sortBy(_._1)
      val ports = for((_, elements) <- groups) yield {
        val ret = cloneOf(elements.head.port)
        val valids = for(self <- elements) yield self.port.valid && elements.filter(_ != self).map(other => !(other.port.valid && other.port.age < self.port.age)).andR
        ret.valid := elements.map(_.port.valid).orR
        ret.payload := OHMux.or(valids, elements.map(_.port.payload), true)
        ret
      }
      val pushes = for(spec <- ports) yield new Area{
        when(spec.valid) {
          valueNext := spec.history
        }
      }
    }

    val inserter = new fpp.Fetch(fetchInsertAt){
      BRANCH_HISTORY := (if(historyFetchBypass) onFetch.valueNext else onFetch.value)
    }
    buildBefore.release()
  }
}
