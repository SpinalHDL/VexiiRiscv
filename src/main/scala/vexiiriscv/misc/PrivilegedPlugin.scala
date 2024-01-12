package vexiiriscv.misc

import spinal.core.{Bool, _}
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamPlugin, CsrRamService, ExecuteLanePlugin}
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._
import vexiiriscv._
import vexiiriscv.fetch.{Fetch, PcService}
import vexiiriscv.schedule.Ages

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object PrivilegedParam{
  def base = PrivilegedParam(
    withSupervisor = false,
    withUser       = false,
    withUserTrap   = false,
    withRdTime     = true,
    withDebug      = false,
    vendorId       = 0,
    archId         = 5, //As spike
    impId          = 0,
    debugTriggers  = 0
  )
}

case class PrivilegedParam(var withSupervisor : Boolean,
                           var withUser: Boolean,
                           var withUserTrap: Boolean,
                           var withRdTime : Boolean,
                           var withDebug: Boolean,
                           var debugTriggers : Int,
                           var vendorId: Int,
                           var archId: Int,
                           var impId: Int){
  def check(): Unit = {
    assert(!(withSupervisor && !withUser))
  }
}

class PrivilegedPlugin(val p : PrivilegedParam, hartIds : Seq[Int]) extends FiberPlugin with CommitService{
  def implementSupervisor = p.withSupervisor
  def implementUser = p.withUser
  def implementUserTrap = p.withUserTrap

  def getPrivilege(hartId : UInt) : UInt = logic.harts.map(_.privilege).read(hartId)

  case class Delegator(var enable: Bool, privilege: Int)
  case class InterruptSpec(var cond: Bool, id: Int, privilege: Int, delegators: List[Delegator])
  case class ExceptionSpec(id: Int, delegators: List[Delegator])
  override def getCommitMask(hartId: Int): Bits = logic.harts(hartId).commitMask
  override def hasInflight(hartId: Int): Bool = logic.harts(hartId).hasInflight

  val misaIds = mutable.LinkedHashSet[Int]()
  def addMisa(id: Char): Unit = addMisa(id - 'A')
  def addMisa(id: Int) = {
    misaIds += id
  }

  def hart(id : Int) = logic.harts(id)

  val logic = during setup new Area {
    val cap = host[CsrAccessPlugin]
    val pp = host[PipelineBuilderPlugin]
    val pcs = host[PcService]
    val withRam = host.get[CsrRamService].nonEmpty
    val crs = withRam generate host[CsrRamService]
    val buildBefore = retains(List(cap.csrLock) ++ withRam.option(crs.csrLock))

    awaitBuild()
    p.check()

    addMisa('I')
    if (RVC) addMisa('C')
    if (RVF) addMisa('F')
    if (RVD) addMisa('D')
    if (RVA) addMisa('A')
    if (RVM) addMisa('M')
    if (p.withUser) addMisa('U')
    if (p.withSupervisor) addMisa('S')

    val causesWidthMins = host.list[CauseUser].map(_.getCauseWidthMin())
    CODE_WIDTH.set((4 +: causesWidthMins).max)

    assert(HART_COUNT.get == 1)

    val rdtime = in UInt (64 bits)
    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val xretAwayFromMachine = False
      val commitMask = Bits(host.list[ExecuteLanePlugin].size bits)
      val hasInflight = Bool()
      val int = new Area {
        val pending = False
        val m = new Area {
          val timer = Verilator.public(in Bool())
          val software = Verilator.public(in Bool())
          val external = Verilator.public(in Bool())
        }
        val s = p.withSupervisor generate new Area {
          val external = Verilator.public(in Bool())
        }
        val u = p.withUserTrap generate new Area {
          val external = Verilator.public(in Bool())
        }
      }
      val spec = new Area {
        val interrupt = ArrayBuffer[InterruptSpec]()
        val exception = ArrayBuffer[ExceptionSpec]()

        def addInterrupt(cond: Bool, id: Int, privilege: Int, delegators: List[Delegator]): Unit = {
          interrupt += InterruptSpec(cond, id, privilege, delegators)
        }
      }

      val api = cap.hart(hartId)
      val withFs = RVF || p.withSupervisor
      val privilege = RegInit(U"11")
      val withMachinePrivilege = privilege >= U"11"
      val withSupervisorPrivilege = privilege >= U"01"

      val m = new Area {
        api.read(U(p.vendorId), CSR.MVENDORID) // MRO Vendor ID.
        api.read(U(p.archId), CSR.MARCHID) // MRO Architecture ID.
        api.read(U(p.impId), CSR.MIMPID) // MRO Implementation ID.
        api.read(U(hartIds(hartId)), CSR.MHARTID) // MRO Hardware thread ID.Machine Trap Setup
        val misaExt = misaIds.map(1l << _).reduce(_ | _)
        val misaMxl = XLEN.get match {
          case 32 => BigInt(1) << XLEN.get - 2
          case 64 => BigInt(2) << XLEN.get - 2
        }
        val misa = misaMxl | misaExt
        api.read(U(misa, XLEN bits), CSR.MISA) // MRW ISA and extensions

        val status = new api.Csr(CSR.MSTATUS) {
          val mie, mpie = RegInit(False)
          val mpp = p.withUser.mux(RegInit(U"00"), U"11")
          val fs = withFs generate RegInit(U"00")
          val sd = False
          if (RVF) ??? //setup.isFpuEnabled setWhen (fs =/= 0)
          if (withFs) sd setWhen (fs === 3)

          readWrite(7 -> mpie, 3 -> mie)
          read(11 -> mpp)
          if (p.withUser) {
            onWrite(true) {
              switch(cap.onWriteBits(12 downto 11)) {
                is(3) { mpp := 3 }
                if (p.withSupervisor) is(1) { mpp := 1 }
                is(0) { mpp := 0 }
              }
            }
          }
          read(XLEN - 1 -> sd)
          if (withFs) readWrite(13 -> fs)
          if (p.withSupervisor && XLEN.get == 64) read(34 -> U"10")
        }

        val cause = new api.Csr(CSR.MCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)
          readWrite(XLEN - 1 -> interrupt, 0 -> code)
        }

        val ip = new api.Csr(CSR.MIP) {
          val meip = RegNext(int.m.external) init (False)
          val mtip = RegNext(int.m.timer) init (False)
          val msip = RegNext(int.m.software) init (False)
          read(11 -> meip, 7 -> mtip, 3 -> msip)
        }

        val ie = new api.Csr(CSR.MIE) {
          val meie, mtie, msie = RegInit(False)
          readWrite(11 -> meie, 7 -> mtie, 3 -> msie)
        }

        val edeleg = p.withSupervisor generate new api.Csr(CSR.MEDELEG) {
          val iam, bp, eu, es, ipf, lpf, spf = RegInit(False)
          val mapping = mutable.LinkedHashMap(0 -> iam, 3 -> bp, 8 -> eu, 9 -> es, 12 -> ipf, 13 -> lpf, 15 -> spf)
          for ((id, enable) <- mapping) readWrite(id -> enable)
        }
        val ideleg = p.withSupervisor generate new api.Csr(CSR.MIDELEG) {
          val st, se, ss = RegInit(False)
          readWrite(9 -> se, 5 -> st, 1 -> ss)
        }

        val tvec = crs.readWriteRam(CSR.MTVEC)
        val tval = crs.readWriteRam(CSR.MTVAL)
        val epc = crs.readWriteRam(CSR.MEPC)
        val scratch = crs.readWriteRam(CSR.MSCRATCH)

        spec.addInterrupt(ip.mtip && ie.mtie, id = 7, privilege = 3, delegators = Nil)
        spec.addInterrupt(ip.msip && ie.msie, id = 3, privilege = 3, delegators = Nil)
        spec.addInterrupt(ip.meip && ie.meie, id = 11, privilege = 3, delegators = Nil)
      }

      val s = p.withSupervisor generate new Area {
        val cause = new api.Csr(CSR.SCAUSE) {
          val interrupt = RegInit(False)
          val code = Reg(CODE) init (0)
          readWrite(XLEN - 1 -> interrupt, 0 -> code)
        }

        val status = new Area {
          val sie, spie = RegInit(False)
          val spp = RegInit(U"0")

          api.read(CSR.SSTATUS, XLEN - 1 -> m.status.sd)
          for (offset <- List(CSR.MSTATUS, CSR.SSTATUS)) {
            api.readWrite(offset, 8 -> spp, 5 -> spie, 1 -> sie)
          }
        }

        val ip = new Area {
          val seipSoft = RegInit(False)
          val seipInput = RegNext(int.s.external)
          val seipOr = seipSoft || seipInput
          val stip = RegInit(False)
          val ssip = RegInit(False)

          val seipMasked = seipOr && m.ideleg.se
          val stipMasked = stip && m.ideleg.st
          val ssipMasked = ssip && m.ideleg.ss
        }

        val ie = new Area {
          val seie, stie, ssie = RegInit(False)
        }

        val tvec = crs.readWriteRam(CSR.STVEC)
        val tval = crs.readWriteRam(CSR.STVAL)
        val epc = crs.readWriteRam(CSR.SEPC)
        val scratch = crs.readWriteRam(CSR.SSCRATCH)

        if (withFs) api.readWrite(CSR.SSTATUS, 13 -> m.status.fs)

        def mapMie(machineCsr: Int, supervisorCsr: Int, bitId: Int, reg: Bool, machineDeleg: Bool, sWrite: Boolean = true): Unit = {
          api.read(reg, machineCsr, bitId)
          api.write(reg, machineCsr, bitId)
          api.read(reg && machineDeleg, supervisorCsr, bitId)
          if (sWrite) api.writeWhen(reg, machineDeleg, supervisorCsr, bitId)
        }

        mapMie(CSR.MIE, CSR.SIE, 9, ie.seie, m.ideleg.se)
        mapMie(CSR.MIE, CSR.SIE, 5, ie.stie, m.ideleg.st)
        mapMie(CSR.MIE, CSR.SIE, 1, ie.ssie, m.ideleg.ss)

        api.read(ip.seipOr, CSR.MIP, 9)
        api.write(ip.seipSoft, CSR.MIP, 9)
        api.read(ip.seipOr && m.ideleg.se, CSR.SIP, 9)
        mapMie(CSR.MIP, CSR.SIP, 5, ip.stip, m.ideleg.st, sWrite = false)
        mapMie(CSR.MIP, CSR.SIP, 1, ip.ssip, m.ideleg.ss)
        api.readToWrite(ip.seipSoft, CSR.MIP, 9) //Avoid an external interrupt value to propagate to the soft external interrupt register.


        spec.addInterrupt(ip.ssip && ie.ssie, id = 1, privilege = 1, delegators = List(Delegator(m.ideleg.ss, 3)))
        spec.addInterrupt(ip.stip && ie.stie, id = 5, privilege = 1, delegators = List(Delegator(m.ideleg.st, 3)))
        spec.addInterrupt(ip.seipOr && ie.seie, id = 9, privilege = 1, delegators = List(Delegator(m.ideleg.se, 3)))

        for ((id, enable) <- m.edeleg.mapping) spec.exception += ExceptionSpec(id, List(Delegator(enable, 3)))

        if (XLEN.get == 64) {
          api.read(CSR.MSTATUS, 32 -> U"10")
          api.read(CSR.SSTATUS, 32 -> U"10")
        }
      }
    }

    val defaultTrap = new Area {
      val csrPrivilege = cap.onDecodeAddress(8, 2 bits)
      val csrReadOnly = cap.onDecodeAddress(10, 2 bits) === U"11"
      when(csrReadOnly && cap.onDecodeWrite || csrPrivilege > harts.reader(cap.onDecodeHartId)(_.privilege)) {
        cap.onDecodeException()
      }
    }

    val readAnyWriteLegal = new Area {
      val tvecFilter = CsrListFilter(List(CSR.MTVEC) ++ p.withSupervisor.option(CSR.STVEC))
      val epcFilter = CsrListFilter(List(CSR.MEPC) ++ p.withSupervisor.option(CSR.SEPC))
      cap.onWrite(tvecFilter, false) {
        cap.onWriteBits(0, 2 bits) := 0
      }
      cap.onWrite(epcFilter, false) {
        cap.onWriteBits(0, log2Up(Fetch.SLICE_BYTES) bits) := 0
      }
    }

    buildBefore.release()
  }
}





//        val tval = Reg(TVAL) init (0)
//        val epc = Reg(PC) init (0)
//        val tvec = Reg(PC) init (0)

//        api.onCsr(CSR.MTVAL).readWrite(tval)
//        api.onCsr(CSR.MEPC).readWrite(epc)
//        api.onCsr(CSR.MTVEC).readWrite(tvec)
