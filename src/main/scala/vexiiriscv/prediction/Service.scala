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


/**
 * Used just to signal the functionality presence
  */
trait FetchWordPrediction{
  def useAccurateHistory: Boolean
}

/**
 * Implemented by the GSharePlugin, allows the BtbPlugin to access the branch prediction at a given stage.
 */
trait FetchConditionalPrediction{
  def useHistoryAt : Int
  def getPredictionAt(stageId : Int) : scala.collection.Seq[Bool]
}

/**
 * Allows branch prediction plugins to specify how many bits of history they need
 */
trait HistoryUser {
  def historyWidthUsed : Int
}

/**
 * Interface used to get traces of the branch/jump instruction execution. This is used by the BtbPlugin/GSharePlugin to learn patterns
 */
case class LearnCmd(hmElements : Seq[NamedType[_ <: Data]]) extends Bundle{
  val pcOnLastSlice = Global.PC() // VexiiRiscv branch prediction always use the address pf the instruction' last slice.
  val pcTarget = Global.PC()
  val taken = Bool()
  val isBranch, isPush, isPop = Bool() // What kind of instruction it was. (push => call, pop => ret, see RISC-V calling convention)
  val wasWrong = Bool() // The BranchPlugin detected that the branch prediction failed
  val badPredictedTarget = Bool() // Meaning the BTB predicted where it should jump/branch wrongly (independently if the branch is taken)
  val history = Prediction.BRANCH_HISTORY()
  val uopId = Decode.UOP_ID()
  val hartId = Global.HART_ID()
  val ctx = new HardMap() // Used to carry additional context which may be used by specific branch prediction implementations
  hmElements.foreach(e => ctx.add(e))
}

/**
 * Interface used by the DecodePlugin to make the BtbPlugin forget a given prediction entry, as that entry produced a
 * prediction which is incompatible with the instruction (ex predicted a branch from a instruction which is a memory load)
 */
case class ForgetCmd() extends Bundle{
  val pcOnLastSlice = Global.PC()
  val hartId = Global.HART_ID()
}

/**
 * Provide an API allowing plugins to access the LearnCmd interface (used by BtbPlugin/GSharePlugin),
 * aswell as a way to specify additional execute pipeline payload which should be collected and feeded to the LearnCmd
 */
trait LearnService{
  val learnLock = Retainer()
  val learnCtxElements = mutable.LinkedHashSet[NamedType[_ <: Data]]()
  def addLearnCtx[T <: Data](that: Payload[T]): Unit = learnCtxElements += that
  def getLearnPort() : Flow[LearnCmd]
}