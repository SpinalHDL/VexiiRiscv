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
  val code = CODE()
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
trait TrapService extends Area{
  val trapLock = Lock()
  val traps = ArrayBuffer[TrapSpec]()
  def newTrap(age: Int, laneAgeWidth: Int): Flow[Trap] = {
    traps.addRet(TrapSpec(Flow(Trap(laneAgeWidth, true)), age)).bus
  }

  def trapHandelingAt : Int
  val trapPendings = ArrayBuffer[Bits]()
  def newTrapPending() = trapPendings.addRet(Bits(Global.HART_COUNT bits))
}

case class TrapPending() extends Bundle{
  val hartId = Global.HART_ID()
}

class PrivilegedPlugin(p : PrivilegedConfig, trapAt : Int) extends FiberPlugin with TrapService{
  lazy val cap = host[CsrAccessPlugin]
  lazy val pp = host[PipelineBuilderPlugin]
  lazy val pcs = host[PcService]
  setupRetain(cap.csrLock)
  buildBefore(pp.elaborationLock)
  buildBefore(pcs.elaborationLock)


  override def trapHandelingAt: Int = trapAt

  val logic = during build new Area{
    val causesWidthMins = host.list[CauseUser].map(_.getCauseWidthMin())
    CAUSE_WIDTH.set((4 +: causesWidthMins).max)

    assert(HART_COUNT.get == 1)

    val io =new Area {
      val int = new Area {
        val m = new Area {
          val timer = in Bool()
          val software = in Bool()
          val external = in Bool()
        }
        val supervisor = p.withSupervisor generate new Area {
          val external = in Bool()
        }
        val user = p.withUserTrap generate new Area {
          val external = in Bool()
        }
      }
      val rdtime = in UInt (64 bits)
    }

    val csrs = for(hartId <- 0 until HART_COUNT) yield new Area{
      val api = cap.hart(hartId)
      val withFs = RVF || p.withSupervisor
      val privilege = RegInit(U"11")
      
      val m = new api.Csr(CSR.MSTATUS) {
        val status = new Area {
          val mie, mpie = RegInit(False)
          val mpp = RegInit(U"00")
          val fs = withFs generate RegInit(U"00")
          val sd = False
          if (RVF) ??? //setup.isFpuEnabled setWhen (fs =/= 0)
          if (withFs) sd setWhen (fs === 3)

          readWrite(11 -> mpp, 7 -> mpie, 3 -> mie)
          read(XLEN - 1 -> sd)
          if (withFs) readWrite(13 -> fs)
        }

        val cause = new api.Csr(CSR.MCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)

          readWrite(XLEN-1 -> interrupt, 0 -> code)
        }

        val ip =  new api.Csr(CSR.MIP) {
          val meip = RegNext(io.int.m.external) init (False)
          val mtip = RegNext(io.int.m.timer) init (False)
          val msip = RegNext(io.int.m.software) init (False)
          read(11 -> meip, 7 -> mtip, 3 -> msip)
        }

        val ie = new api.Csr(CSR.MIE) {
          val meie, mtie, msie = RegInit(False)
          readWrite(11 -> meie, 7 -> mtie, 3 -> msie)
        }


        val tval = Reg(TVAL) init (0)
        val epc = Reg(PC) init (0)
        val tvec = Reg(PC) init (0)

        api.onCsr(CSR.MTVAL).readWrite(tval)
        api.onCsr(CSR.MEPC).readWrite(epc)
        api.onCsr(CSR.MTVEC).readWrite(tvec)

      }



      //    cap.readWrite(CSR.MCAUSE, XLEN - 1 -> cause.interrupt, 0 -> cause.code)

      //    cap.read(CSR.MIP, 11 -> mip.meip, 7 -> mip.mtip, 3 -> mip.msip)
      //    cap.readWrite(CSR.MIE, 11 -> mie.meie, 7 -> mie.mtie, 3 -> mie.msie)
    }




    trapLock.await()
    val hartsTrap = for(hartId <- 0 until HART_COUNT) yield new Area{
      val csr = csrs(hartId)
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
      }


      val pcPort = pcs.createJumpInterface(Ages.TRAP, 0, 0)
      pcPort.valid := trigger.valid
      pcPort.hartId := hartId
      pcPort.pc := csrs(hartId).m.tvec
      val fsm = new StateMachine {
        val RUNNING = makeInstantEntry()
        val TRAP_TRIGGER = new State()

        val inflightTrap = trapPendings.map(_(hartId)).orR
        val holdPort = pcs.newHoldPort(hartId)
        holdPort := inflightTrap || !isActive(RUNNING)

        RUNNING.whenIsActive{
          when(trigger.valid){
            goto(TRAP_TRIGGER)
          }
        }

        TRAP_TRIGGER.whenIsActive{
          csr.m.epc := pending.pc
          csr.m.tval := pending.state.tval
          csr.m.cause.code := pending.state.code
          csr.m.cause.interrupt := False
          csr.m.status.mpp := csr.privilege
          goto(RUNNING)
        }
      }

      csr.privilege := 3 //TODO remove
    }
    cap.csrLock.release()
  }
}
