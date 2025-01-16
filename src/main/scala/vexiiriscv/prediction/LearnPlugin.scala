package vexiiriscv.prediction

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.BranchPlugin

import scala.collection.mutable

trait LearnSource{
  def getLearnPort() : Option[Stream[LearnCmd]]
}

trait ForgetSource{
  def getForgetPort() : Flow[ForgetCmd]
}

// Aggregate all of LearnSource's ports into a single one for the BTB to use.
class LearnPlugin extends FiberPlugin with LearnService {
  override def getLearnPort(): Flow[LearnCmd] = logic.learn

  val logic = during build new Area {
    Prediction.BRANCH_HISTORY_WIDTH.set((0 +: host.list[HistoryUser].map(_.historyWidthUsed)).max)
    learnLock.await()

    val ups = host.list[LearnSource].flatMap(_.getLearnPort())
    val learn = Flow(LearnCmd(learnCtxElements.toSeq))
    val buffered = ups.map(_.pipelined(m2s = ups.size > 1)) // As we may have multiple learning sources and we can only learn 1 per cycle, let's add some buffer
    val arbitrated = StreamArbiterFactory().noLock.roundRobin.on(buffered)
    learn << arbitrated.toFlow
  }
}
