package vexiiriscv.fetch

import spinal.core._
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.Plugin
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.memory.AddressTranslationService


object HartService{
  object Priorities{
    def FETCH_WORD(stage : Int, prediction : Boolean) = stage*2 + (if(prediction) -1 else 0)
    val ALIGNER           = 90
    val DECODE_PREDICTION = 100
    val COMMIT_RESCHEDULE = 200
    val COMMIT_TRAP = 201
  }
}

case class JumpCmd() extends Bundle{
  val pc = PC()
  val hartId = HART_ID()
}

trait HartService extends Area{
  def createJumpInterface(priority : Int) : Flow[JumpCmd] //High priority win
}