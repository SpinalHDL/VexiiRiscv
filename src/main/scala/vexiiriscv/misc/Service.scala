package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Retainer

/*
        pmu {
		    compatible 			= "riscv,pmu";
		    riscv,event-to-mhpmevent =
 					 <0x5 0x0000 0x01>, /* BRANCH_INSTRUCTIONS -> Conditional branch instruction count */
					 <0x6 0x0000 0x02>, /* BRANCH_MISSES       -> Misprediction of conditional branches */
					 <0x10008 0x0000 0x10>,  /* L1I_READ_ACCESS  -> I-Cache access */
					 <0x10009 0x0000 0x11>,  /* L1I_READ_MISS    -> I-Cache miss */
					 <0x3 0x0000 0x18>, /* CACHE_REFERENCES    -> D-Cache access */
					 <0x4 0x0000 0x19>; /* CACHE_REFERENCES    -> D-Cache access */


	        riscv,event-to-mhpmcounters =
	                <0x00005 0x00006 0xFF8>,
	                <0x00003 0x00004 0xFF8>,
	                <0x10008 0x10009 0xFF8>;
          };


          riscv,raw-event-to-mhpmcounters =
              <0x0000 0x0012 0xffffffff 0xffffffff 0x00000ff8>,
              <0x0000 0x001a 0xffffffff 0xffffffff 0x00000ff8>;

 */

object PerformanceCounterService{
  val BRANCH_COUNT   = 0x01
  val BRANCH_MISS    = 0x02

  val ICACHE_ACCESS  = 0x10
  val ICACHE_MISS    = 0x11
  val ICACHE_WAITING = 0x12

  val DCACHE_ACCESS  = 0x18
  val DCACHE_MISS    = 0x19
  val DCACHE_WAITING = 0x1A
}

trait PerformanceCounterService {
  def createEventPort(id : Int) : Bool
  val elaborationLock = Retainer()
}

trait CommitService {
  def getCommitMask(hartId: Int): Bits
}

trait InflightService {
  def hasInflight(hartId: Int): Bool
}