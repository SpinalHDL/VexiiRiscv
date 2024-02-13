package vexiiriscv.prediction

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.Flow
import spinal.lib.misc.database.Database.blocking
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.Fetch
import scala.collection.mutable



object Prediction extends AreaObject{
  //Used by fetch based instruction to inform the aligner plugin about predictions done
  val WORD_JUMPED = Payload(Bool())
  val WORD_JUMP_SLICE = Payload(Fetch.SLICE_ID)
  val WORD_JUMP_PC = Payload(Global.PC)
  val WORD_SLICES_BRANCH = Payload(Bits(Fetch.SLICE_COUNT bits))
  val WORD_SLICES_TAKEN = Payload(Bits(Fetch.SLICE_COUNT bits))
//  val WORD_SLICE_JB = Payload(Vec.fill(Fetch.SLICE_COUNT)(Bool()))

  //Used by decoder based prediction to know the fetch based prediction modified the flow of future instructions
  val ALIGNED_JUMPED = Payload(Bool())
  val ALIGNED_JUMPED_PC = Payload(Global.PC)
  val ALIGNED_SLICES_BRANCH = Payload(Bits(Fetch.SLICE_COUNT bits))
  val ALIGNED_SLICES_TAKEN = Payload(Bits(Fetch.SLICE_COUNT bits))
  val ALIGN_REDO = Payload(Bool()) //Used when for instance when a prediction cut an instruction in two => need to redo it and forget that prediction

  //Used by decode predictor to correct the history
  val BRANCH_HISTORY_WIDTH = blocking[Int]
  val BRANCH_HISTORY = Payload(Bits(BRANCH_HISTORY_WIDTH bits))

  def withHistory = BRANCH_HISTORY_WIDTH.get != 0

//  val BRANCH_HISTORY_PUSH_VALID = Stageable(Bool())
//  val BRANCH_HISTORY_PUSH_SLICE = Stageable(UInt(log2Up(SLICE_COUNT) bits))
//  val BRANCH_HISTORY_PUSH_VALUE = Stageable(Bool())
//
//  //Set by fetch prediction to propose conditional branch prediction, could be used by another fetch level prediction, or later on in decode to correct things
//  val CONDITIONAL_TAKE_IT = Stageable(Bits(SLICE_COUNT bits))
//
//  val IS_BRANCH = Stageable(Bool())
}


// Used just to signal the functionality presence
trait FetchWordPrediction{
  def useAccurateHistory: Boolean
}
trait FetchConditionalPrediction{
  def useHistoryAt : Int
  def getPredictionAt(stageId : Int) : Seq[Bool]
}
trait HistoryUser {
  def historyWidthUsed : Int
}

case class LearnCmd(hmElements : Seq[NamedType[_ <: Data]]) extends Bundle{
  val pcOnLastSlice = Global.PC()
  val pcTarget = Global.PC()
  val taken = Bool()
  val isBranch, isPush, isPop = Bool()
  val wasWrong = Bool()
  val badPredictedTarget = Bool()
  val history = Prediction.BRANCH_HISTORY()
  val uopId = Decode.UOP_ID()
  val hartId = Global.HART_ID()
  val ctx = new HardMap()
  hmElements.foreach(e => ctx.add(e))
}

case class ForgetCmd() extends Bundle{
  val pcOnLastSlice = Global.PC()
  val hartId = Global.HART_ID()
}

trait LearnService{
  val learnLock = Retainer()
  val learnCtxElements = mutable.LinkedHashSet[NamedType[_ <: Data]]()
  def addLearnCtx[T <: Data](that: Payload[T]): Unit = learnCtxElements += that
  def getLearnPort() : Flow[LearnCmd]
}