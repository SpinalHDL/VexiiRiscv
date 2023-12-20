package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Retainer

object PerformanceCounterService{
  val ICACHE_REFILL = 1
  val DCACHE_REFILL = 2
  val DCACHE_WRITEBACK = 3
  val BRANCH_MISS = 4
}

trait PerformanceCounterService {
  def createEventPort(id : Int) : Bool
  val elaborationLock = Retainer()
}

trait CommitService {
  def getCommitMask(hartId : Int) : Bits
}