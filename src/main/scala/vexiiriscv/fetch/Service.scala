package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.Plugin
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.memory.AddressTranslationService

case class JumpCmd() extends Bundle{
  val pc = PC()
  val hartId = HART_ID()
}

trait PcService {
  def createJumpInterface(age : Int, aggregationPriority : Int = 0) : Flow[JumpCmd] //High priority win
  val elaborationLock = Lock()
}

trait InitService{
  def initHold() : Bool
}