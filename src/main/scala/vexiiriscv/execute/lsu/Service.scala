package vexiiriscv.execute.lsu

import spinal.core.Bundle
import spinal.core.fiber.{Handle, Retainer}
import spinal.lib._
import spinal.lib.system.tag.PmaRegion

import scala.collection.mutable.ArrayBuffer

/**
 * Implemented by the LsuPlugins to provide the MmuPlugin's hardware page walker a way to access the memory
 */
trait LsuCachelessBusProvider {
  def getLsuCachelessBus() : LsuCachelessBus
}

/**
 * Allows a plugin to specifies to others that CMO are implemented
 * This is used by the IntAluPlugin to avoid overlaping the ORI instruction with the CMO's prefetch instructions
 */
trait CmoService{
  def withSoftwarePrefetch : Boolean
}

/**
 * Used by the TrapPlugin to flush the L1 data cache durring a fence.i instruction
 */
case class LsuL1InvalidationCmd() extends Bundle //Empty for now, as we flush the whole cache
case class LsuL1InvalidationBus() extends Bundle {
  val cmd = Stream(LsuL1InvalidationCmd())
}
trait LsuService{
  val invalidationRetainer = Retainer()
  val invalidationPorts = ArrayBuffer[LsuL1InvalidationBus]()
  def newInvalidationPort() = invalidationPorts.addRet(LsuL1InvalidationBus())
  def lsuCommitProbe : Flow[LsuCommitProbe] // Provide the LSU execution traces, allowing the hardware prefetching plugin to learn patterns
  def getBlockSize : Int
}

trait LsuL1Service{
  def withCoherency : Boolean // Memory Coherency
  val regions = Handle[ArrayBuffer[PmaRegion]]() // Provide the RISC-V PMA at hardware elaboration time, so the LSU can directly trap on bad memory accesses
}
