package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.Lockable
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.Plugin
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.memory.AddressTranslationService


object PcService{
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

trait PcService extends Lockable{
  def createJumpInterface(age : Int, aggregationPriority : Int = 0) : Flow[JumpCmd] //High priority win
}

trait InitService{
  def initHold() : Bool
}