package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Retainer

/*
        pmu {
		    compatible 			= "riscv,pmu";
		    riscv,event-to-mhpmevent =
 					 <0x1 0x0000 0x06>, /*  Cycle */
					 <0x2 0x0000 0x07>, /*  instructions */
 					 <0x5 0x0000 0x01>, /*  Conditional branch instruction count */
					 <0x6 0x0000 0x02>, /*  Misprediction of conditional branches */
 					 <0x8 0x0000 0x04>, /*  STALLED_CYCLES_FRONTEND */
 					 <0x9 0x0000 0x05>, /*  STALLED_CYCLES_BACKEND */
					 <0x10000 0x0000 0x18>, /*  D-Cache load access */
					 <0x10001 0x0000 0x19>, /*  D-Cache load miss */
					 <0x10008 0x0000 0x10>,  /* I-Cache access */
					 <0x10009 0x0000 0x11>;  /* I-Cache miss */


	        riscv,event-to-mhpmcounters =
	                <0x00001 0x00009 0xFF8>,
	                <0x10000 0x10009 0xFF8>;

		riscv,raw-event-to-mhpmcounters = <0x0000 0x0000 0xffffffff 0xffffff00 0x00000ff8>;
        };

 */

object PerformanceCounterService{
  val BRANCH_COUNT   = 0x01
  val BRANCH_MISS    = 0x02

  val STALLED_CYCLES_FRONTEND = 0x04 // => 8
  val STALLED_CYCLES_BACKEND = 0x05 // => 9

  val CYCLES = 0x06
  val INSTRUCTIONS = 0x07

  val ICACHE_ACCESS     = 0x10
  val ICACHE_MISS       = 0x11
  val ICACHE_WAITING    = 0x12
  val ICACHE_TLB_CYCLES = 0x13

  val DCACHE_LOAD_ACCESS = 0x18
  val DCACHE_LOAD_MISS   = 0x19
  val DCACHE_WAITING     = 0x1A
  val DCACHE_TLB_CYCLES  = 0x1B


  val DEV = 0x20
}

trait PerformanceCounterService {
  def createEventPort(id: Int): Bool
  def createEventPort(id: Int, drive : Bool): Bool = {
    val ret = createEventPort(id)
    ret := drive
    ret
  }
  val elaborationLock = Retainer()
}

trait CommitService {
  def getCommitMask(hartId: Int): Bits
}

trait InflightService {
  def hasInflight(hartId: Int): Bool
}