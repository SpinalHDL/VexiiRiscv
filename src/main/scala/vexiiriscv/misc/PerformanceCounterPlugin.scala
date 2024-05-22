package vexiiriscv.misc

import spinal.core._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamAllocation, CsrRamPlugin, CsrRamService}
import vexiiriscv.riscv.{CSR, Riscv}

import scala.collection.mutable.ArrayBuffer

/**
 * This plugin implement the performance counters in a very tricky way to save area
 * Only 7 bits registers are used for each counters, which are flushed into a CSR ram when their MSB is set
 */
class PerformanceCounterPlugin(var additionalCounterCount : Int,
                               var bufferWidth : Int = 8) extends FiberPlugin with PerformanceCounterService{
  def counterCount = 2 + additionalCounterCount

  case class Spec(id : Int, event : Bool)
  val specs = ArrayBuffer[Spec]()
  override def createEventPort(id: Int) = specs.addRet(Spec(id, Bool())).event

  val logic = during setup new Area{
    val csr = host[CsrAccessPlugin]
    val ram = host[CsrRamPlugin]
    val priv = host[PrivilegedPlugin]
    val tp = host[TrapPlugin]
    val csrRetainer = csr.csrLock()
    val ramCsrRetainer = ram.csrLock()
    val ramPortRetainer = ram.portLock()
    val trapLock = tp.trapLock()
    awaitBuild()

    assert(Global.HART_COUNT.get == 1)
    val withHigh = Riscv.XLEN.get == 32

    assert(Global.HART_COUNT.get == 1)
    val commitMask = Cat(host.list[CommitService].map(_.getCommitMask(0)))
    val ignoreNextCommit = RegInit(False) clearWhen (commitMask.orR)
    val commitCount = CountOne(commitMask) - U(ignoreNextCommit && commitMask.orR)

    var counterIdPtr = 0
    class Counter extends Area{
      val alloc = ram.ramAllocate(if (withHigh) 2 else 1)
      val value = Reg(UInt(bufferWidth bits)).init(0)
      val needFlush = value.msb
      val counterId = counterIdPtr
      counterIdPtr += 1

      val mcounteren = csr.readWrite(RegInit(False), CSR.MCOUNTEREN, counterId)
      val scounteren = priv.p.withSupervisor generate csr.readWrite(RegInit(False), CSR.SCOUNTEREN, counterId)
      val mcountinhibit = csr.readWrite(RegInit(False), CSR.MCOUNTINHIBIT, counterId)
    }
    case class Mapping(csrId : Int, alloc : CsrRamAllocation, offset : Int)
    val mappings = ArrayBuffer[Mapping]()
    val counters = new Area{
      val cycle = new Counter()
      counterIdPtr += 1 //skip time
      val instret = new Counter()
      val additionals = List.fill(additionalCounterCount)(new Counter)

      cycle.value := cycle.value + (!cycle.mcountinhibit).asUInt
      instret.value := instret.value + RegNext(commitCount.andMask(!instret.mcountinhibit)).init(0)

      mappings += Mapping(CSR.MCYCLE, cycle.alloc, 0)
      mappings += Mapping(CSR.UCYCLE, cycle.alloc, 0)
      mappings += Mapping(CSR.MINSTRET, instret.alloc, 0)
      mappings += Mapping(CSR.UINSTRET, instret.alloc, 0)
      for(i <- 0 until additionalCounterCount) {
        mappings += Mapping(CSR.MHPMCOUNTER3 + i, additionals(i).alloc, 0)
        mappings += Mapping(CSR.UHPMCOUNTER3 + i, additionals(i).alloc, 0)
      }
      if (withHigh) for(b <- mappings.toArray) mappings += Mapping(b.csrId+0x80, b.alloc, b.offset+1)

      for (m <- mappings) {
        ram.readWriteRam(m.csrId, m.alloc, m.offset)
      }
      val list = List(cycle, instret) ++ additionals
    }

    val dummyCsrs = for(hpmId <- 3+additionalCounterCount to 31;
                        csrId <- List(CSR.MHPMCOUNTER0, CSR.UHPMCOUNTER0, CSR.MHPMEVENT0) ++ (if(withHigh)List(CSR.MHPMCOUNTER0H, CSR.UHPMCOUNTER0H) else Nil)) yield csrId + hpmId
    csr.allowCsr(CsrListFilter(dummyCsrs)) //As read zero


    ramCsrRetainer.release()
    val readPort = ram.ramReadPort(CsrRamService.priority.COUNTER)
    val writePort = ram.ramWritePort(CsrRamService.priority.COUNTER)
    ramPortRetainer.release()

    elaborationLock.await()

    val csrFilter = CsrListFilter(mappings.map(m => m.csrId))

    val interrupt = new Area {
      val ip, ie = RegInit(False)
      csr.readWrite(CSR.MIP, 13 -> ip)
      csr.readWrite(CSR.MIE, 13 -> ie)
      val sup = priv.implementSupervisor generate new Area {
        val deleg = RegInit(False)
        csr.readWrite(CSR.MIDELEG, 13 -> deleg)
      }
      priv.logic.harts(0).spec.addInterrupt(
        ip && ie,
        id = 13,
        privilege = priv.implementSupervisor.mux(1, 3),
        delegators = priv.implementSupervisor.mux(List(Delegator(sup.deleg, 3)), Nil)
      )
    }

    val events = new Area {
      val selWidth = log2Up((specs.map(_.id) :+ 0).max + 1)
      val grouped = specs.groupByLinked(_.id)
      val sums = grouped.map{ case (id, specs) => id -> CountOne(specs.map(_.event)) }
      val widthMax = sums.map(_._2.getWidth).max
    }

    val hpm = for(id <- 0 until 3+additionalCounterCount) yield (id >= 3) generate new Area{
      val counter = counters.additionals(id-3)
      val eventId = Reg(UInt(events.selWidth bits)) init(0)
      val overflowEvent = False
      val OF   = RegInit(False) setWhen(overflowEvent)
      val MINH = RegInit(False)
      val SINH = RegInit(False)
      val UINH = RegInit(False)

      interrupt.ip.setWhen(overflowEvent && !OF)

      val incr    = if(events.sums.isEmpty) U(0) else events.sums.map(e => e._2.andMask(eventId === e._1).resize(events.widthMax)).toList.reduceBalancedTree(_ | _)
      val inhibit = CombInit(counter.mcountinhibit)
      when(!inhibit) {
        counter.value := counter.value + incr
      }
      csr.readWrite(CSR.MHPMEVENT0 + id, 0 -> eventId)
      val eb = CSR.MHPMEVENT0 + id
      val eo = Riscv.XLEN.get match {
        case 32 => 32
        case 64 => 0
      }
      val privValue = priv.getPrivilege(0)
      val ofRead = CombInit(OF)
      csr.read(CSR.SCOUNTOVF, id -> ofRead)
      ofRead clearWhen(!counter.mcounteren && !privValue(1))

      csr.readWrite(eb, 63-eo -> OF, 62-eo -> MINH)
      inhibit.setWhen(privValue === 3 && MINH)
      if (priv.p.withSupervisor) {
        csr.readWrite(eb, 61 - eo -> SINH)
        inhibit.setWhen(privValue === 1 && SINH)
        ofRead clearWhen(!counter.scounteren && !privValue(0))
      }
      if (priv.p.withUser) {
        csr.readWrite(eb, 60 - eo -> UINH)
        inhibit.setWhen(privValue === 0 && UINH)
      }
    }


    val fsm = new StateMachine{
      val IDLE, READ_LOW, CALC_LOW = new State
      val READ_HIGH, CALC_HIGH = withHigh generate new State
      val CSR_WRITE = new State
      setEntry(IDLE)


      val flusherCmd = Stream(new Bundle {
        val oh = Bits(counterCount bits)
      })
      val csrWriteCmd, csrReadCmd = Stream(new Bundle {
        val address = UInt(log2Up(counterCount+1) bits)
      })
      List(csrReadCmd, flusherCmd, csrWriteCmd).foreach(_.ready := False)


      ram.awaitMapping()
      val cmd = new Area {
        val flusher = Reg(Bool())
        val oh = Reg(Bits(counterCount bits))
        val address = counters.list.reader(oh)(c => U(c.alloc.at >> withHigh.toInt, ram.portAddressWidth - withHigh.toInt bits))
      }

      val done = isActive(IDLE)
      val ramReaded = Reg(UInt(Riscv.XLEN bits))
      val carry = Reg(Bool())
      val counterReaded = Reg(UInt(bufferWidth bits))

      readPort.valid := False
      readPort.address := (if(withHigh) cmd.address @@ carry else cmd.address)

      writePort.valid := False
      writePort.address := readPort.address
      writePort.data.assignDontCare()


      val calc = new Area{
        def low = ramReaded(ramReaded.high downto bufferWidth - 1) @@ counterReaded.resize(bufferWidth - 1)
        def high = ramReaded
        def blow = U(counterReaded.msb) << bufferWidth-1

        val a = if(withHigh) carry.mux(high, low) else low
        val b = if(withHigh) carry.mux(U(1), blow) else blow
        val sum = a +^ b
      }

      def doOverflow(): Unit = {
        hpm.drop(3).onMask(cmd.oh.drop(2)) { c =>
          c.overflowEvent := True
        }
      }

      val idleCsrAddress = csrReadCmd.valid.mux(csrReadCmd.address, csrWriteCmd.address)
      val holdCsrWrite = True
      when(holdCsrWrite){
        ram.holdCsrWrite()
      }
      IDLE whenIsActive{
        holdCsrWrite := False
        cmd.oh := B(for (c <- counters.list) yield idleCsrAddress === c.counterId)
        when(csrWriteCmd.valid) {
          goto(CSR_WRITE)
        }elsewhen(flusherCmd.valid){
          cmd.flusher := True
          cmd.oh := flusherCmd.oh
          flusherCmd.ready := True
          goto(READ_LOW)
        } elsewhen(csrReadCmd.valid){
          cmd.flusher := False
          csrReadCmd.ready := True
          goto(READ_LOW)
        }
        carry := False
      }

      CSR_WRITE whenIsActive {
        holdCsrWrite := False
        when(csr.bus.write.address(7) === False) {
          counters.list.onMask(cmd.oh) { c =>
            c.value := csr.bus.write.bits.asUInt.resized
            c.value.msb := False
          }
        }
        ignoreNextCommit setWhen (cmd.oh(1)) // && (if(withHigh) !csrWriteCmd.high else True)
        csrWriteCmd.ready := True
        goto(IDLE)
      }


      READ_LOW.whenIsActive{
        readPort.valid := True
        ramReaded := U(readPort.data)
        counterReaded := counters.list.reader(cmd.oh)(_.value)
        when(readPort.ready) {
          goto(CALC_LOW)
        }
      }

      CALC_LOW.whenIsActive {
        when(counterReaded.msb) {
          counters.list.onMask(cmd.oh)(_.value.msb := False)
        }
        writePort.valid := True
        writePort.data := B(calc.sum).resized
        when(writePort.ready) {
          goto(IDLE)
          if (withHigh) when(calc.sum.msb){
            carry := True
            goto(READ_HIGH)
          } else {
            when(calc.sum.msb){
              doOverflow()
            }
          }
        }
      }


      if(withHigh) READ_HIGH.whenIsActive{
        readPort.valid := True
        ramReaded := U(readPort.data)
        when(readPort.ready) {
          goto(CALC_HIGH)
        }

        CALC_HIGH.whenIsActive {
          writePort.valid := True
          writePort.data := B(calc.sum).resized
          when(writePort.ready) {
            when(calc.sum.msb) {
              doOverflow()
            }
            goto(IDLE)
          }
        }
      }

    }

    val flusher = new Area{
      val hits = B(counters.list.map(_.needFlush))
      val hit = hits.orR
      val oh = OHMasking.first(hits)

      fsm.flusherCmd.valid := hit
      fsm.flusherCmd.oh := oh
    }

    // Trap eventual bad accesses
    val csrDecode = new Area{
      val addr = csr.bus.decode.address(0, log2Up(counterCount + 1) bits)
      val mok = addr.muxListDc(counters.list.map(e => e.counterId -> e.mcounteren))
      val sok = priv.p.withSupervisor.mux(addr.muxListDc(counters.list.map(e => e.counterId -> e.scounteren)), True)
      val privOk = (priv.getPrivilege(csr.bus.decode.hartId) | U(mok ## sok)).andR
      csr.onDecode(csrFilter){ //TODO test
        when(csr.bus.decode.address(9 downto 8) === 0){
          when(csr.bus.decode.write || !privOk){
            csr.bus.decode.doException()
          }
        }
      }
    }

    val csrRead = new Area {
      val fired = RegInit(False) setWhen(fsm.csrReadCmd.fire)
      val requested = csr.bus.read.valid && csr.readingCsr(csrFilter)
      fsm.csrReadCmd.valid := requested && !fired
      fsm.csrReadCmd.address := csr.bus.read.address(0, log2Up(counterCount+1) bits)

      when(requested){
        when(!fired || !fsm.done){
          ram.holdCsrRead()
        }
      }
      when(csr.bus.read.moving){
        fired := False
      }
    }

    val csrWrite = new Area{
      val fired = RegInit(False) setWhen(fsm.csrWriteCmd.fire)
      fsm.csrWriteCmd.valid := False
      fsm.csrWriteCmd.address := csr.bus.write.address(0, log2Up(counterCount+1) bits)

      csr.onWrite(csrFilter, false){
        when(!fired){
          fsm.csrWriteCmd.valid := True
          when(!fsm.csrWriteCmd.ready){
            csr.bus.write.doHalt()
          }
        }
      }
      when(csr.bus.write.moving){
        fired := False
      }
    }
    csrRetainer.release()
    trapLock.release()
  }
}
