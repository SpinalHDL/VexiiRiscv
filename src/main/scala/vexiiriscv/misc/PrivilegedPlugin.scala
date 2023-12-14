package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, ExecuteLanePlugin}
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._
import vexiiriscv._
import vexiiriscv.fetch.PcService
import vexiiriscv.schedule.Ages

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
  val tval = TVAL()
  val cause = CAUSE()
  val laneAge = full generate UInt(laneAgeWidth bits)
  val hartId = full generate HART_ID()

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
 * execute
 * - miss aligned load/store/branch
 * - page fault, access fault
 * - ecall, ebreak
 *
 * To do a trap request :
 * - Flush all youngers instructions
 * - Store cause / tval
 * - Rise trap flag in the pipe (this will disable side-effects)
 */
trait TrapService{
  val trapLock = Lock()
  val traps = ArrayBuffer[TrapSpec]()
  def newTrap(age: Int, laneAgeWidth: Int): Flow[Trap] = {
    traps.addRet(TrapSpec(Flow(Trap(laneAgeWidth, true)), age)).bus
  }
}

class PrivilegedPlugin(p : PrivilegedConfig, trapAt : Int) extends FiberPlugin with TrapService{
  lazy val cap = host[CsrAccessPlugin]
  lazy val pp = host[PipelineBuilderPlugin]
  lazy val pcs = host[PcService]
  setupRetain(cap.csrLock)
  buildBefore(pp.elaborationLock)
  buildBefore(pcs.elaborationLock)



  val logic = during build new Area{
    val causesWidthMins = host.list[CauseUser].map(_.getCauseWidthMin())
    CAUSE_WIDTH.set((4 +: causesWidthMins).max)

    assert(HART_COUNT.get == 1)

    val csrs = for(hartId <- 0 until HART_COUNT) yield new Area{
      val api = cap.hart(hartId)
      val withFs = RVF || p.withSupervisor
      val mstatus = new Area {
        val mie, mpie = RegInit(False)
        val mpp = RegInit(U"00")
        val fs = withFs generate RegInit(U"00")
        val sd = False
        if (RVF) ??? //setup.isFpuEnabled setWhen (fs =/= 0)
        if (withFs) sd setWhen (fs === 3)

        val mapper = api.onCsr(CSR.MSTATUS)
        mapper.readWrite(11 -> mpp, 7 -> mpie, 3 -> mie)
        mapper.read(XLEN - 1 -> sd)
        if (withFs) mapper.readWrite(13 -> fs)
      }
      val mcause = Reg(CAUSE) init (0)
      val mtval = Reg(TVAL) init (0)
      val mepc = Reg(PC) init (0)
      val mtvec = Reg(PC) init (0)
      api.onCsr(CSR.MTVAL).readWrite(mtval)
      api.onCsr(CSR.MEPC).readWrite(mepc)
      api.onCsr(CSR.MTVEC).readWrite(mtvec)

      //    cap.readWrite(CSR.MCAUSE, XLEN - 1 -> cause.interrupt, 0 -> cause.code)

      //    cap.read(CSR.MIP, 11 -> mip.meip, 7 -> mip.mtip, 3 -> mip.msip)
      //    cap.readWrite(CSR.MIE, 11 -> mie.meie, 7 -> mie.mtie, 3 -> mie.msie)
    }




    trapLock.await()
    val hartsTrap = for(hartId <- 0 until HART_COUNT) yield new Area{
      val pending = new Area {
        val requests = traps.map(e => new AgedArbiterUp(e.bus.valid && e.bus.hartId === hartId, e.bus.payload.toRaw(), e.age, e.age))
        val arbiter = new AgedArbiter(requests)
        val state = arbiter.down.toReg
        val pc = Reg(PC)
      }


      val trigger = new Area {
        val lanes = host.list[ExecuteLanePlugin] //TODO AREA filter the ones which may trap
        val oh = B(for(self <- lanes; sn = self.execute(trapAt).down) yield sn.isFiring && sn(TRAP))
        val valid = oh.orR
        val pc = OHMux.or(oh, lanes.map(_.execute(trapAt).down(PC)), true)
        when(valid){
          pending.pc := pc
        }

        val inflights = B(for(lane <- lanes; exId <- 0 to trapAt; ctrl = lane.execute(exId)) yield ctrl.isValid && ctrl(TRAP))
        val holdPort = pcs.newHoldPort(hartId)
        holdPort := inflights.orR
      }


      val pcPort = pcs.createJumpInterface(Ages.TRAP, 0, 0)
      pcPort.valid := trigger.valid
      pcPort.hartId := hartId
      pcPort.pc := csrs(hartId).mtvec
//      val fsm = new StateMachine {
//        val RUNNING = makeInstantEntry()
//
//
//      }
    }
    cap.csrLock.release()
  }
}
