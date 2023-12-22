package vexiiriscv.misc

import spinal.core._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamAllocation, CsrRamPlugin, CsrRamService}
import vexiiriscv.riscv.{CSR, Riscv}

import scala.collection.mutable.ArrayBuffer


class PerformanceCounterPlugin(var additionalCounterCount : Int,
                               var bufferWidth : Int = 7) extends FiberPlugin with PerformanceCounterService{
  def counterCount = 2 + additionalCounterCount

  case class Spec(id : Int, event : Bool)
  val specs = ArrayBuffer[Spec]()
  override def createEventPort(id: Int) = specs.addRet(Spec(id, Bool())).event

  val logic = during setup new Area{
    val csr = host[CsrAccessPlugin]
    val ram = host[CsrRamPlugin]
    val priv = host[PrivilegedPlugin]
    val csrRetainer = retains(csr.csrLock, ram.csrLock)
    val ramPortRetainer = ram.portLock()
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

      val mcounteren = RegInit(False)
      val scounteren = RegInit(False)
    }
    case class Mapping(csrId : Int, alloc : CsrRamAllocation, offset : Int)
    val mappings = ArrayBuffer[Mapping]()
    val counters = new Area{
      val cycle = new Counter()
      counterIdPtr += 1 //skip time
      val instret = new Counter()
      val additionals = List.fill(additionalCounterCount)(new Counter)

      cycle.value := cycle.value + 1
      instret.value := instret.value + RegNext(commitCount).init(0)

      mappings += Mapping(CSR.MCYCLE, cycle.alloc, 0)
      mappings += Mapping(CSR.MINSTRET, instret.alloc, 0)
      for(i <- 0 until additionalCounterCount) mappings += Mapping(CSR.MHPMCOUNTER3+i, additionals(i).alloc, 0)
      if (withHigh) for(b <- mappings.toArray) mappings += Mapping(b.csrId+0x80, b.alloc, b.offset+1)

      for (m <- mappings) {
        ram.readWriteRam(m.csrId, m.alloc, m.offset)
      }
      val list = List(cycle, instret) ++ additionals
    }

    csrRetainer.release()
    val readPort = ram.ramReadPort(CsrRamService.priority.COUNTER)
    val writePort = ram.ramWritePort(CsrRamService.priority.COUNTER)
    ramPortRetainer.release()

    elaborationLock.await()

    val csrFilter = CsrListFilter(mappings.map(m => m.csrId))

//TODO
    csr.allowCsr(CSR.MCOUNTINHIBIT) //As read zero

//    val mcounteren = csr.readWrite(Reg(3 + Counter))
//    mcountinhibit
//    if (priv.implementSupervisor) csr.allowCsr(CSR.SCOUNTEREN)
//    csr.allowCsr(CSR.MCOUNTEREN)




    val events = new Area {
      val selWidth = log2Up((specs.map(_.id) :+ 0).max + 1)
      val grouped = specs.groupByLinked(_.id)
      val sums = grouped.map{ case (id, specs) => id -> CountOne(specs.map(_.event)) }
    }

    val hpm = for(id <- 0 until 3+additionalCounterCount) yield (id >= 3) generate new Area{
      val counter = counters.additionals(id-3)
      val eventId = Reg(UInt(events.selWidth bits)) init(0)
      val incr    = if(events.sums.isEmpty) U(0) else events.sums.map(e => e._2.andMask(eventId === e._1)).toList.reduceBalancedTree(_ | _)
      counter.value := counter.value + incr
      csr.readWrite(eventId, CSR.MHPMEVENT0 + id)
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

      val idleCsrAddress = csrReadCmd.valid.mux(csrReadCmd.address, csrWriteCmd.address)
      IDLE whenIsActive{
        cmd.oh := B(for (c <- counters.list) yield idleCsrAddress === c.counterId)
        when(flusherCmd.valid){
          cmd.flusher := True
          cmd.oh := flusherCmd.oh
          flusherCmd.ready := True
          goto(READ_LOW)
        } elsewhen(csrReadCmd.valid){
          cmd.flusher := False
          csrReadCmd.ready := True
          goto(READ_LOW)
        } elsewhen(csrWriteCmd.valid){
          goto(CSR_WRITE)
        }
        carry := False
      }

      CSR_WRITE whenIsActive {
        when(csr.onWriteAddress(7) === False) {
          counters.list.onMask(cmd.oh) { c =>
            c.value := csr.onWriteBits.asUInt.resized
            c.value.msb := False
          }
          ignoreNextCommit setWhen (cmd.oh(1)) // && (if(withHigh) !csrWriteCmd.high else True)

        }
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
          }
        }
      }


      if(withHigh) READ_HIGH.whenIsActive{
        readPort.valid := True
        ramReaded := U(readPort.data)
        when(readPort.ready) {
          goto(CALC_HIGH)
        }
      }

      CALC_HIGH.whenIsActive {
        writePort.valid := True
        writePort.data := B(calc.sum).resized
        when(writePort.ready) {
          goto(IDLE)
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

    val csrRead = new Area {
      val fired = RegInit(False) setWhen(fsm.csrReadCmd.fire)
      val requested = csr.isReading && csr.readingCsr(csrFilter)
      fsm.csrReadCmd.valid := requested && !fired
      fsm.csrReadCmd.address := csr.onReadAddress(0, log2Up(counterCount+1) bits)

      when(requested){
        when(!fired || !fsm.done){
          ram.holdCsrRead()
        }
      }
      when(csr.onReadMovingOff){
        fired := False
      }
    }

    val csrWrite = new Area{
      val fired = RegInit(False) setWhen(fsm.csrWriteCmd.fire)
      fsm.csrWriteCmd.valid := False
      fsm.csrWriteCmd.address := csr.onWriteAddress(0, log2Up(counterCount+1) bits)

      if(priv.implementUser) when(csr.onDecodeWrite && (csr.onDecodeAddress & 0xF60) === CSR.UCYCLE){
        csr.onDecodeTrap()
      }

      csr.onWrite(csrFilter, false){
        when(!fired){
          fsm.csrWriteCmd.valid := True
          when(!fsm.csrWriteCmd.ready){
            csr.onWriteHalt()
          }
        }
      }
      when(csr.onWriteMovingOff){
        fired := False
      }
    }
    csrRetainer.release()
  }
}
