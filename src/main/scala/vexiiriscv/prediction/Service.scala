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


/**
 * Used to define a few signal which travel on the fetch pipeline for prediction purposes
 */
object Prediction extends AreaObject{
  //Used by the BbtPlugin in the fetch pipeline to notify the AlignerPlugin of the predictions which happend
  val WORD_JUMPED = Payload(Bool()) // A prediction was taken
  val WORD_JUMP_SLICE = Payload(Fetch.SLICE_ID) // Which slice of the fetched word got the prediction (last slice of the instruction)
  val WORD_JUMP_PC = Payload(Global.PC) // Where the prediction jumped to.
  val WORD_SLICES_BRANCH = Payload(Bits(Fetch.SLICE_COUNT bits)) // Specifies which slices of the fetched word was predicted as a branch
  val WORD_SLICES_TAKEN = Payload(Bits(Fetch.SLICE_COUNT bits)) // Which ones of the WORD_SLICES_BRANCH was predicted as taken

  //Used by the decoder based prediction to know the fetch based prediction modified the flow of future instructions
  // Their meaning is the same as the WORD_xxx defined above
  val ALIGNED_JUMPED = Payload(Bool())
  val ALIGNED_JUMPED_PC = Payload(Global.PC)
  val ALIGNED_SLICES_BRANCH = Payload(Bits(Fetch.SLICE_COUNT bits))
  val ALIGNED_SLICES_TAKEN = Payload(Bits(Fetch.SLICE_COUNT bits))
  val ALIGN_REDO = Payload(Bool()) //Used by the AlignerPlugin to notify when a prediction cut an instruction in two => need to redo it and forget that prediction

  def withHistory = BRANCH_HISTORY_WIDTH.get != 0
  val BRANCH_HISTORY_WIDTH = blocking[Int]
  val BRANCH_HISTORY = Payload(Bits(BRANCH_HISTORY_WIDTH bits)) // Used by the BranchPlugin to patch the branch history
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
  val badPredictedTarget = Bool() // Meaning the BTB predicted where it should jump/branch wrongly (independently if the branch is taken)
  val history = Prediction.BRANCH_HISTORY()
  val uopId = Decode.UOP_ID()
  val hartId = Global.HART_ID()
  val ctx = new HardMap() // Used to carry additional context which may be used by specific branch prediction implementations
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