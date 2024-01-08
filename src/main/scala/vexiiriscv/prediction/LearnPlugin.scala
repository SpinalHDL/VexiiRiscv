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

class LearnPlugin extends FiberPlugin with LearnService {
  override def getLearnPort(): Flow[LearnCmd] = logic.learn

  val logic = during build new Area {
    Prediction.BRANCH_HISTORY_WIDTH.set((0 +: host.list[HistoryUser].map(_.historyWidthUsed)).max)
    learnLock.await()

    val ups = host.list[LearnSource].flatMap(_.getLearnPort())
    val learn = Flow(LearnCmd(learnCtxElements.toSeq))

//    learn << StreamArbiterFactory().noLock.roundRobin.on(ups.map(_.queueLowLatency(8))).toFlow; println("REMOVE ME QUEUE")// coremark => 8.3% downto 7.7%
    learn << StreamArbiterFactory().noLock.roundRobin.on(ups.map(_.pipelined(m2s = ups.size > 1))).toFlow
  }
}
