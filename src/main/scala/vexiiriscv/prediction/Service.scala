package vexiiriscv.prediction

import spinal.core._
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.fetch.Fetch

// Used just to signal the functionality presence
trait FetchWordPrediction

object Prediction extends AreaObject{
  //Used by fetch based instruction to inform the aligner plugin about predictions done
  val WORD_JUMPED = Payload(Bool())
  val WORD_JUMP_SLICE = Payload(Fetch.SLICE_ID)
  val WORD_JUMP_PC = Payload(Global.PC)

//  //Used by decoder based prediction to know the fetch based prediction modified the flow of future instructions
  val ALIGNED_JUMPED = Payload(Bool())
  val ALIGNED_JUMPED_PC = Payload(Global.PC)
//
//  //Used by decode predictor to correct the history
//  val BRANCH_HISTORY_PUSH_VALID = Stageable(Bool())
//  val BRANCH_HISTORY_PUSH_SLICE = Stageable(UInt(log2Up(SLICE_COUNT) bits))
//  val BRANCH_HISTORY_PUSH_VALUE = Stageable(Bool())
//
//  //Set by fetch prediction to propose conditional branch prediction, could be used by another fetch level prediction, or later on in decode to correct things
//  val CONDITIONAL_TAKE_IT = Stageable(Bits(SLICE_COUNT bits))
//
//  val IS_BRANCH = Stageable(Bool())
}


case class LearnCmd() extends Bundle{
  val pcOnLastSlice = Global.PC()
  val pcTarget = Global.PC()
  val taken = Bool()
  val isBranch = Bool()
}