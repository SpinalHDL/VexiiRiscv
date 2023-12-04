package vexiiriscv.prediction

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.fetch.FetchPipelinePlugin
import Prediction._
import vexiiriscv.Global

import scala.collection.mutable


case class HistoryJump() extends Bundle{
  val history = BRANCH_HISTORY()
}

class HistoryPlugin(var historyFetchBypass : Boolean = true) extends FiberPlugin {
  lazy val fpp = host[FetchPipelinePlugin]
  buildBefore(fpp.elaborationLock)

  case class HistorySpec(priority : Int, port : Flow[HistoryJump])
  val historySpecs = mutable.ArrayBuffer[HistorySpec]()
  def createPort(priority : Int): Flow[HistoryJump] = {
    historySpecs.addRet(HistorySpec(priority, Flow(HistoryJump()))).port
  }

  val elaborationLock = Lock()
  val logic = during build new Area{
    elaborationLock.await()

    val fetchUsages = host.list[FetchConditionalPrediction]
    val fetchInsertAt = if(fetchUsages.nonEmpty) fetchUsages.map(_.useHistoryAt).min else 0

    assert(Global.HART_COUNT.get == 1)
    val onFetch = new Area{
      val value = Reg(BRANCH_HISTORY) init(0)
      val valueNext = CombInit(value)
      value := valueNext

      val ports = historySpecs.sortBy(_.priority)
      assert(ports.map(_.priority).distinct.size == ports.size)
      val pushes = for(spec <- ports) yield new Area{
        when(spec.port.valid) {
          valueNext := spec.port.history
        }
      }
    }

    val inserter = new fpp.Fetch(fetchInsertAt){
      BRANCH_HISTORY := (if(historyFetchBypass) onFetch.valueNext else onFetch.value)
    }
  }
}
