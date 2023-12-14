package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.CsrAccessPlugin
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._

import scala.collection.mutable.ArrayBuffer


object PrivilegedConfig{
  def full = PrivilegedConfig(
    withSupervisor = true,
    withUser       = true,
    withUserTrap   = false,
    withRdTime     = true,
    withDebug      = false,
    vendorId       = 0,
    archId         = 5, //As spike
    impId          = 0,
    hartId         = 0,
    debugTriggers  = 0
  )
}

case class PrivilegedConfig(withSupervisor : Boolean,
                            withUser: Boolean,
                            withUserTrap: Boolean,
                            withRdTime : Boolean,
                            withDebug: Boolean,
                            debugTriggers : Int,
                            vendorId: Int,
                            archId: Int,
                            impId: Int,
                            var hartId: Int) {

}


case class TrapSpec(bus : Flow[Trap], age : Int)
case class Trap(laneAgeWidth : Int, full : Boolean) extends Bundle{
  val tval = Reg(Bits(XLEN bits))
  val epc = Reg(Global.PC)
  val cause = Reg(Global.CAUSE)
  val laneAge = full generate UInt(laneAgeWidth bits)
  val hartId = full generate Global.HART_ID()

  def toRaw(): Trap = {
    val r = new Trap(laneAgeWidth, false)
    r.assignSomeByName(this)
    r
  }
}

trait CauseUser{
  def getCauseWidthMin() : Int
}

/**
 * fetch (page fault, access fault)
 * decode (illegal)
 * execute (miss aligned load/store/branch)
 */
trait TrapService{
  val trapLock = Lock()
  val traps = ArrayBuffer[TrapSpec]()
  def newTrap(age: Int, laneAgeWidth: Int): Flow[Trap] = {
    traps.addRet(TrapSpec(Flow(Trap(laneAgeWidth, true)), age)).bus
  }
}

class PrivilegedPlugin(p : PrivilegedConfig) extends FiberPlugin with TrapService{
  lazy val cap = host[CsrAccessPlugin]
  setupRetain(cap.csrLock)


  val logic = during build new Area{
    val causesWidthMins = host.list[CauseUser].map(_.getCauseWidthMin())
    Global.CAUSE_WIDTH.set((4 +: causesWidthMins).max)

    assert(Global.HART_COUNT.get == 1)
    val hartId = 0

    val withFs = RVF || p.withSupervisor
    val mstatus = new Area {
      val mie, mpie = RegInit(False)
      val mpp = RegInit(U"00")
      val fs = withFs generate RegInit(U"00")
      val sd = False
      if (RVF) ??? //setup.isFpuEnabled setWhen (fs =/= 0)
      if (withFs) sd setWhen (fs === 3)
    }
    val mtval = Reg(Bits(Riscv.XLEN bits)) init (0)

//    cap.readWrite(CSR.MCAUSE, XLEN - 1 -> cause.interrupt, 0 -> cause.code)
    cap.readWrite(CSR.MSTATUS, 11 -> mstatus.mpp, 7 -> mstatus.mpie, 3 -> mstatus.mie)
    cap.read(CSR.MSTATUS, XLEN - 1 -> mstatus.sd)
    if(withFs) cap.readWrite(CSR.MSTATUS, 13 -> mstatus.fs)
//    cap.read(CSR.MIP, 11 -> mip.meip, 7 -> mip.mtip, 3 -> mip.msip)
//    cap.readWrite(CSR.MIE, 11 -> mie.meie, 7 -> mie.mtie, 3 -> mie.msie)
    cap.readWrite(mtval, CSR.MTVAL)


//    val fsm = new StateMachine{

      trapLock.await()
      val pending = new Area{
        val requests = traps.map(e => new AgedArbiterUp(e.bus.valid && e.bus.hartId === hartId, e.bus.payload.toRaw(), e.age, e.age))
        val arbiter = new AgedArbiter(requests)

        val valid = RegInit(False) setWhen(arbiter.down.valid)
        val state = arbiter.down.toReg
      }
//      val RUNNING = makeInstantEntry()
//
//
//    }
    cap.csrLock.release()
  }
}
