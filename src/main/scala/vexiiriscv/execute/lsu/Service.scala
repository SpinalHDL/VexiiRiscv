package vexiiriscv.execute.lsu

import spinal.core.Bundle
import spinal.core.fiber.{Handle, Retainer}
import spinal.lib._
import spinal.lib.system.tag.PmaRegion

import scala.collection.mutable.ArrayBuffer

trait LsuCachelessBusProvider {
  def getLsuCachelessBus() : LsuCachelessBus
}

trait CmoService{
  def withSoftwarePrefetch : Boolean
}

case class LsuL1InvalidationCmd() extends Bundle //Empty for now
case class LsuL1InvalidationBus() extends Bundle {
  val cmd = Stream(LsuL1InvalidationCmd())
}
trait LsuService{
  val invalidationRetainer = Retainer()
  val invalidationPorts = ArrayBuffer[LsuL1InvalidationBus]()
  def newInvalidationPort() = invalidationPorts.addRet(LsuL1InvalidationBus())
  def lsuCommitProbe : Flow[LsuCommitProbe]
  def getBlockSize : Int
}

trait LsuL1Service{
  def withCoherency : Boolean
  val regions = Handle[ArrayBuffer[PmaRegion]]()
}
