package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.Plru
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.misc.Reservation
import vexiiriscv.riscv.Riscv

object LsuL1 extends AreaObject{
  // -> L1
  val SEL = Payload(Bool())
  val LOAD, AMO, SC, LR = Payload(Bool())
  val MIXED_ADDRESS = Payload(Global.MIXED_ADDRESS)
  val PHYSICAL_ADDRESS = Payload(Global.PHYSICAL_ADDRESS)
  val WRITE_DATA = Payload(Bits(Riscv.LSLEN bits))
  val WRITE_MASK = Payload(Bits(Riscv.LSLEN/8 bits))

  // L1 ->
  val READ_DATA = Payload(Bits(Riscv.LSLEN bits))
  val REDO, MISS, FAULT = Payload(Bool())
}

class LsuL1Plugin(val lane : ExecuteLaneService,
                  var memDataWidth: Int,
                  var cpuDataWidth: Int,
                  var refillCount: Int,
                  var writebackCount: Int,
                  var setCount: Int,
                  var wayCount: Int,
                  var lineSize: Int = 64,
                  var bankReadAt: Int = 0,
                  var wayReadAt: Int = 0,
                  var hitsAt: Int = 1,
                  var hitAt: Int = 1,
                  var bankMuxesAt: Int = 1,
                  var bankMuxAt: Int = 2,
                  var ctrlAt: Int = 2,
                  var hazardCheckWidth : Int = 12,
                  var hitsWithTranslationWays: Boolean = false,
                  var reducedBankWidth: Boolean = false,
                  var tagsReadAsync: Boolean = false,
                  var withCoherency: Boolean = false,
                  var probeIdWidth: Int = -1,
                  var ackIdWidth: Int = -1) extends FiberPlugin{

  def memParameter = LsuL1BusParameter(
    addressWidth = Global.PHYSICAL_WIDTH,
    dataWidth = memDataWidth,
    readIdCount = refillCount,
    writeIdCount = writebackCount,
    probeIdWidth = probeIdWidth,
    ackIdWidth = ackIdWidth,
    lineSize = lineSize,
    withReducedBandwidth = false,
    withCoherency = withCoherency
  )

  val logic = during setup new Area{
    import LsuL1._

    awaitBuild()

    val postTranslationWidth = Global.PHYSICAL_WIDTH

    val cacheSize = wayCount*setCount*lineSize
    val cpuWordWidth = cpuDataWidth
    val bytePerMemWord = memDataWidth / 8
    val bytePerFetchWord = cpuDataWidth / 8
    val waySize = cacheSize / wayCount
    val linePerWay = waySize / lineSize
    val memDataPerWay = waySize / bytePerMemWord
    val memData = HardType(Bits(memDataWidth bits))
    val memWordPerLine = lineSize / bytePerMemWord
    val tagWidth = postTranslationWidth - log2Up(waySize)

    val tagRange = postTranslationWidth - 1 downto log2Up(linePerWay * lineSize)
    val lineRange = tagRange.low - 1 downto log2Up(lineSize)
    val refillRange = tagRange.high downto lineRange.low
    val hazardCheckRange = hazardCheckWidth+lineRange.low-1 downto lineRange.low

    val bankCount = wayCount
    val bankWidth = if (!reducedBankWidth) memDataWidth else Math.max(cpuWordWidth, memDataWidth / wayCount)
    val bankByteSize = cacheSize / bankCount
    val bankWordCount = bankByteSize * 8 / bankWidth
    val bankWordToCpuWordRange = log2Up(bankWidth / 8) - 1 downto log2Up(bytePerFetchWord)
    val memToBankRatio = bankWidth * bankCount / memDataWidth
    val bankWord = HardType(Bits(bankWidth bits))
    val bankWordPerLine = lineSize * 8 / bankWidth

    assert(bankWidth <= memDataWidth)

    val bus = master(LsuL1Bus(memParameter))

    val ABORD = Payload(Bool())
    val WAYS_HAZARD = Payload(Bits(wayCount bits))
    val REDO_ON_DATA_HAZARD = Payload(Bool())
    val BANK_BUSY = Payload(Bits(bankCount bits))
    val BANK_BUSY_REMAPPED = Payload(Bits(bankCount bits))
    val REFILL_HITS_EARLY = Payload(Bits(refillCount bits))
    val REFILL_HITS = Payload(Bits(refillCount bits))
    val LOCKED, UNLOCKED = Payload(Bool())
    val NEED_UNIQUE = Payload(Bool())


    case class Tag() extends Bundle {
      val loaded = Bool()
      val address = UInt(tagWidth bits)
      val fault = Bool()
      val unique = withCoherency generate Bool()
      val dirty = Bool()
    }


//    val STATUS = Payload(Vec.fill(wayCount)(Status()))
    val BANKS_WORDS = Payload(Vec.fill(bankCount)(bankWord()))
    val WAYS_TAGS = Payload(Vec.fill(wayCount)(Tag()))
    val WAYS_HITS = Payload(Bits(wayCount bits))
    val WAYS_HIT = Payload(Bool())
    val IO = Payload(Bool())
    val REFILL_SLOT = Payload(Bits(refillCount bits))
    val REFILL_SLOT_FULL = Payload(Bool())
    val GENERATION, GENERATION_OK = Payload(Bool())
    val PREFETCH = Payload(Bool())
    val PROBE = Payload(Bool())
    val ALLOW_UNIQUE = Payload(Bool())
    val ALLOW_SHARED = Payload(Bool())
    val ALLOW_PROBE_DATA = Payload(Bool())
    val PROBE_ID = Payload(UInt(probeIdWidth bits))
    val FLUSH = Payload(Bool())
    val FLUSH_FREE = Payload(Bool())

    val BANKS_MUXES = Payload(Vec.fill(bankCount)(Bits(cpuWordWidth bits)))

    val tagsWriteArbiter = new Reservation()
    val bankWriteArbiter = new Reservation() //TODO
    val bankReadArbiter  = new Reservation()

    val refillCompletions = Bits(refillCount bits)
    val writebackBusy = Bool()

    val banks = for (id <- 0 until bankCount) yield new Area {
      val mem = Mem(Bits(bankWidth bits), bankWordCount)
      val write = mem.writePortWithMask(mem.getWidth / 8)
      val read = new Area{
        val cmd = Flow(mem.addressType).setIdle()
        val rsp = mem.readSync(cmd.payload, cmd.valid)
        KeepAttribute(rsp) //Ensure that it will not use 2 cycle latency ram block
      }
    }

    val waysWrite = new Area {
      val mask = B(0, wayCount bits)
      val address = UInt(log2Up(linePerWay) bits).assignDontCare()
      val tag = Tag().assignDontCare()

      //Used for hazard tracking in a pipelined way
      val maskLast = RegNext(mask)
      val addressLast = RegNext(address)
    }


    val ways = for (id <- 0 until wayCount) yield new Area {
      val mem = Mem.fill(linePerWay)(Tag())
      mem.write(waysWrite.address, waysWrite.tag, waysWrite.mask(id))
      val lsuRead = new Area {
        val cmd = Flow(mem.addressType)
        val rsp = if (tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
        KeepAttribute(rsp) //Ensure that it will not use 2 cycle latency ram block
      }
    }

    val PLRU = Payload(Plru.State(wayCount))
    val plru = new Area {
      val mem = Mem.fill(linePerWay)(Plru.State(wayCount))
      val write = mem.writePort
      val read = new Area {
        val cmd = Flow(mem.addressType)
        val rsp = if (tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
        KeepAttribute(rsp) //Ensure that it will not use 2 cycle latency ram block
      }
    }

    //TODO ensure that no invalidate happen when there is anything happening anywere else
    val invalidate = new Area {
      val counter = Reg(UInt(log2Up(linePerWay) + 1 bits)) init (0)
      val done = counter.msb
      val reservation = tagsWriteArbiter.create(0) //Warning assume no refill at the same time
      when(!done) {
        reservation.takeIt()
        assert(reservation.win)
        counter := counter + 1
        waysWrite.mask.setAll()
        waysWrite.address := counter.resized
        waysWrite.tag.loaded := False
      }

      val firstEver = RegInit(True) clearWhen (done)
      plru.write.valid := !done && firstEver
      plru.write.address := counter.resized
      plru.write.data.clearAll()
    }

    class PriorityArea(slots: Seq[(Bool, Bits)]) extends Area {
      val slotsWithId = slots.zipWithIndex.map(e => (e._1._1, e._1._2, e._2))
      val hits = B(slots.map(_._1))
      val hit = hits.orR
      val oh = hits & B(slotsWithId.map(slot => (B(slotsWithId.filter(_ != slot).map(other => hits(other._3))) & slot._2) === 0))
      val sel = OHToUInt(oh)
      val lock = RegNext(oh) init (0)
      when(lock.orR) {
        oh := lock
      }
    }


    val refill = new Area {
      val slots = for (refillId <- 0 until refillCount) yield new Area {
        val id = refillId
        val valid = RegInit(False)
        val address = Reg(UInt(postTranslationWidth bits))
        val way = Reg(UInt(log2Up(wayCount) bits))
        val cmdSent = Reg(Bool())
        val priority = Reg(Bits(refillCount - 1 bits)) //TODO Check it
        val unique = withCoherency generate Reg(Bool())
        val data = withCoherency generate Reg(Bool())
        val ackId = withCoherency generate Reg(UInt(ackIdWidth bits))
        val ackValid = withCoherency generate RegInit(False)

        // This counter ensure that load/store which started before the end of the refill memory transfer but ended after the end
        // of the memory transfer do see that there was a refill ongoing and that they need to retry
        val loaded = Reg(Bool())
        val loadedCounterMax = ctrlAt - Math.min(wayReadAt, bankReadAt)-1
        val loadedCounter = Reg(UInt(log2Up(loadedCounterMax + 1) bits))
        val loadedDone = loadedCounter === loadedCounterMax
        loadedCounter := loadedCounter + U(loaded && !loadedDone).resized
        valid clearWhen (loadedDone && withCoherency.mux(!ackValid, True))

        val free = !valid

        val victim = Reg(Bits(writebackCount bits))
        val writebackHazards = Reg(Bits(writebackCount bits)) //TODO Check it
      }

      //Ignore the way, allowing coherent BtoT to detect ongoing NtoB
      def isLineBusy(address: UInt) = slots.map(s => s.valid && s.address(hazardCheckRange) === address(hazardCheckRange)).orR

      val free = B(OHMasking.first(slots.map(_.free)))
      val full = slots.map(!_.free).andR

      val push = Flow(new Bundle {
        val address = UInt(postTranslationWidth bits)
        val way = UInt(log2Up(wayCount) bits)
        val victim = Bits(writebackCount bits)
        val unique = Bool()
        val data = Bool()
      }).setIdle()

      import spinal.core.sim._

      val pushCounter = Reg(UInt(32 bits)) init (0) simPublic()
      when(push.valid) {
        pushCounter := pushCounter + 1
      }

      for (slot <- slots) when(push.valid) {
        when(free(slot.id)) {
          slot.valid := True
          slot.address := push.address
          slot.way := push.way
          slot.cmdSent := False
          slot.priority.setAll()
          slot.loaded := False
          slot.loadedCounter := 0
          slot.victim := push.victim
          slot.writebackHazards := 0
          if (withCoherency) {
            slot.unique := push.unique
            slot.data := push.data
          }
        } otherwise {
          val freeFiltred = free.asBools.patch(slot.id, Nil, 1)
          (slot.priority.asBools, freeFiltred).zipped.foreach(_ clearWhen (_))
        }
      }

      val read = new Area {
        val arbiter = new PriorityArea(slots.map(s => (s.valid && !s.cmdSent && s.victim === 0 && s.writebackHazards === 0, s.priority)))

        val writebackHazards = Bits(writebackCount bits)
        val writebackHazard = writebackHazards.orR
        when(bus.read.cmd.fire || writebackHazard) {
          arbiter.lock := 0
        }

        val cmdAddress = slots.map(_.address(tagRange.high downto lineRange.low)).read(arbiter.sel) @@ U(0, lineRange.low bit)
        bus.read.cmd.valid := arbiter.hit && !writebackHazard
        bus.read.cmd.id := arbiter.sel
        bus.read.cmd.address := cmdAddress
        if (withCoherency) {
          bus.read.cmd.unique := slots.map(_.unique).read(arbiter.sel)
          bus.read.cmd.data := slots.map(_.data).read(arbiter.sel)
        }
        slots.onMask(arbiter.oh) { slot =>
          slot.writebackHazards := writebackHazards
          slot.cmdSent setWhen (bus.read.cmd.ready && !writebackHazard)
        }

        val rspAddress = slots.map(_.address).read(bus.read.rsp.id)
        val way = slots.map(_.way).read(bus.read.rsp.id)
        val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))
        val rspWithData = withCoherency.mux(bus.read.rsp.withData, True)
        if (withCoherency) assert(!(bus.read.rsp.valid && !rspWithData && slots.map(_.data).read(bus.read.rsp.id)), "Data cache asked for data but didn't recieved any :(")

        val bankWriteNotif = Bits(bankCount bits)
        val writeReservation = bankWriteArbiter.create(0)
        when(bus.read.rsp.valid) {
          writeReservation.takeIt()
          assert(writeReservation.win)
        }
        for ((bank, bankId) <- banks.zipWithIndex) {
          if (!reducedBankWidth) {
            bankWriteNotif(bankId) := bus.read.rsp.valid && rspWithData && way === bankId
            bank.write.valid := bankWriteNotif(bankId)
            bank.write.address := rspAddress(lineRange) @@ wordIndex
            bank.write.data := bus.read.rsp.data
          } else {
            val sel = U(bankId) - way
            val groupSel = way(log2Up(bankCount) - 1 downto log2Up(bankCount / memToBankRatio))
            val subSel = sel(log2Up(bankCount / memToBankRatio) - 1 downto 0)
            bankWriteNotif(bankId) := bus.read.rsp.valid && rspWithData && groupSel === (bankId >> log2Up(bankCount / memToBankRatio))
            bank.write.valid := bankWriteNotif(bankId)
            bank.write.address := rspAddress(lineRange) @@ wordIndex @@ (subSel)
            bank.write.data := bus.read.rsp.data.subdivideIn(bankCount / memToBankRatio slices)(subSel)
          }
          banks(bankId).write.mask := (default -> true)
        }

        val hadError = RegInit(False) setWhen (bus.read.rsp.valid && bus.read.rsp.error)
        val fire = False
        val reservation = tagsWriteArbiter.create(0)
        val faulty = hadError || bus.read.rsp.error

        refillCompletions := 0
        bus.read.rsp.ready := True
        when(bus.read.rsp.valid) {
          assert(reservation.win)
          when(rspWithData) {
            wordIndex := wordIndex + 1
          }
          when(wordIndex === wordIndex.maxValue || !rspWithData) {
            hadError := False
            fire := True
            if (!withCoherency) refillCompletions(bus.read.rsp.id) := True
            reservation.takeIt()
            waysWrite.mask(way) := True
            waysWrite.address := rspAddress(lineRange)
            waysWrite.tag.fault := faulty
            waysWrite.tag.address := rspAddress(tagRange)
            waysWrite.tag.loaded := True
            if (withCoherency) {
              waysWrite.tag.unique := bus.read.rsp.unique
            }
            slots.onSel(bus.read.rsp.id) { s =>
              s.loaded := True
              if (withCoherency) {
                s.ackValid := True
                s.ackId := bus.read.rsp.ackId
              }
            }
          }
        }
      }

      val ackSender = withCoherency generate new Area {
        val ack = cloneOf(bus.read.ack)
        val requests = slots.map(_.ackValid)
        val oh = OHMasking.first(requests)
        ack.valid := requests.orR
        ack.ackId := OhMux.or(oh, slots.map(_.ackId))
        when(ack.ready) {
          refillCompletions.asBools.onMask(oh)(_ := True)
          slots.onMask(oh)(_.ackValid := False)
        }

        val buffer = ack.m2sPipe()
        val counter = Reg(UInt(2 bits)) init (0)
        when(buffer.valid) {
          counter := counter + 1
        }
        bus.read.ack << buffer.haltWhen(counter =/= 3) //Give some time for the CPU to do forward progress
      }
    }

    val writeback = new Area {
      val slots = for (writebackId <- 0 until writebackCount) yield new Area {
        val id = writebackId
        val fire = False
        val valid = RegInit(False) clearWhen (fire)
        val address = Reg(UInt(postTranslationWidth bits))
        val way = Reg(UInt(log2Up(wayCount) bits))
        val priority = Reg(Bits(writebackCount - 1 bits)) //TODO Check it
        val readCmdDone = Reg(Bool())
        val victimBufferReady = Reg(Bool())
        val readRspDone = Reg(Bool())
        val writeCmdDone = Reg(Bool())

        val coherency = withCoherency generate new Area {
          val release = Reg(Bool())
          val dirty = Reg(Bool())
          val fromUnique = Reg(Bool())
          val toShared = Reg(Bool())
          val probeId = Reg(UInt(probeIdWidth bits))
        }

        val free = !valid

        refill.read.writebackHazards(id) := valid && address(refillRange) === refill.read.cmdAddress(refillRange)
        when(fire) {
          refill.slots.foreach(_.writebackHazards(id) := False)
        }
      }

      writebackBusy := slots.map(_.valid).orR

      def isLineBusy(address: UInt) = slots.map(s => s.valid && s.address(hazardCheckRange) === address(hazardCheckRange)).orR

      val free = B(OHMasking.first(slots.map(_.free)))
      val full = slots.map(!_.free).andR

      val push = Flow(new Bundle {
        val address = UInt(postTranslationWidth bits)
        val way = UInt(log2Up(wayCount) bits)

        //TtoB TtoN BtoN
        val dirty = withCoherency generate Bool()
        val fromUnique = withCoherency generate Bool()
        val toShared = withCoherency generate Bool()
        val release = withCoherency generate Bool()
        val probeId = withCoherency generate UInt(probeIdWidth bits)
      }).setIdle()

      for (slot <- slots) when(push.valid) {
        when(free(slot.id)) {
          slot.valid := True
          slot.address := push.address
          slot.way := push.way

          slot.writeCmdDone := False
          slot.priority.setAll()
          if (withCoherency) {
            slot.coherency.release := push.release
            slot.coherency.dirty := push.dirty
            slot.coherency.fromUnique := push.fromUnique
            slot.coherency.toShared := push.toShared
            slot.coherency.probeId := push.probeId
            slot.readCmdDone := !push.dirty
            slot.readRspDone := !push.dirty
            slot.victimBufferReady := !push.dirty
          } else {
            slot.readCmdDone := False
            slot.readRspDone := False
            slot.victimBufferReady := False
          }
        } otherwise {
          val freeFiltred = free.asBools.patch(slot.id, Nil, 1)
          (slot.priority.asBools, freeFiltred).zipped.foreach(_ clearWhen (_))
        }
      }

      val victimBuffer = Mem.fill(writebackCount * memWordPerLine)(Bits(memDataWidth bits))
      val read = new Area {
        val arbiter = new PriorityArea(slots.map(s => (s.valid && !s.readCmdDone, s.priority)))

        val address = slots.map(_.address).read(arbiter.sel)
        val way = slots.map(_.way).read(arbiter.sel)
        val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

        val slotRead = Flow(new Bundle {
          val id = UInt(log2Up(writebackCount) bits)
          val last = Bool()
          val wordIndex = UInt(log2Up(memWordPerLine) bits)
          val way = UInt(log2Up(wayCount) bits)
        })
        slotRead.valid := arbiter.hit
        slotRead.id := arbiter.sel
        slotRead.wordIndex := wordIndex
        slotRead.way := way
        slotRead.last := wordIndex === wordIndex.maxValue
        wordIndex := wordIndex + U(slotRead.valid)
        when(slotRead.valid && slotRead.last) {
          slots.onMask(arbiter.oh) {
            _.readCmdDone := True
          }
          arbiter.lock := 0
        }
        when(slotRead.fire) {
          for (slot <- refill.slots) slot.victim(slotRead.id) := False
        }

        val brr = bankReadArbiter.create(0)
        when(slotRead.valid) {
          brr.takeIt()
          assert(brr.win)
        }
        for ((bank, bankId) <- banks.zipWithIndex) {
          if (!reducedBankWidth) {
            when(slotRead.valid && way === bankId) {
              bank.read.cmd.valid := True
              bank.read.cmd.payload := address(lineRange) @@ wordIndex
            }
          } else {
            val sel = U(bankId) - way
            val groupSel = way(log2Up(bankCount) - 1 downto log2Up(bankCount / memToBankRatio))
            val subSel = sel(log2Up(bankCount / memToBankRatio) - 1 downto 0)
            when(arbiter.hit && groupSel === (bankId >> log2Up(bankCount / memToBankRatio))) {
              bank.read.cmd.valid := True
              bank.read.cmd.payload := address(lineRange) @@ wordIndex @@ (subSel)
            }
          }
        }

        val slotReadLast = slotRead.stage()
        val readedData = Bits(memDataWidth bits)

        if (!reducedBankWidth) {
          readedData := banks.map(_.read.rsp).read(slotReadLast.way)
        } else {
          for ((slice, sliceId) <- readedData.subdivideIn(bankWidth bits).zipWithIndex) {
            ???
          }
        }


        when(slotReadLast.valid) {
          victimBuffer.write(slotReadLast.id @@ slotReadLast.wordIndex, readedData)
          whenIndexed(slots, slotReadLast.id) {
            _.victimBufferReady := True
          }
          when(slotReadLast.last) {
            whenIndexed(slots, slotReadLast.id) {
              _.readRspDone := True
            }
          }
        }
      }

      val write = new Area {
        val arbiter = new PriorityArea(slots.map(s => (s.valid && s.victimBufferReady && !s.writeCmdDone, s.priority)))
        val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))
        val last = wordIndex === wordIndex.maxValue

        val bufferRead = Stream(new Bundle {
          val id = UInt(log2Up(writebackCount) bits)
          val address = UInt(postTranslationWidth bits)
          val last = Bool()
          val coherency = withCoherency generate new Bundle {
            val release = Bool()
            val dirty = Bool()
            val fromUnique = Bool()
            val toShared = Bool()
            val probeId = UInt(probeIdWidth bits)
          }
        })
        bufferRead.valid := arbiter.hit
        bufferRead.id := arbiter.sel
        bufferRead.last := last
        bufferRead.address := slots.map(_.address).read(arbiter.sel)
        val c = withCoherency generate new Area {
          last setWhen (!bufferRead.coherency.dirty)
          bufferRead.coherency.release := slots.map(_.coherency.release).read(arbiter.sel)
          bufferRead.coherency.dirty := slots.map(_.coherency.dirty).read(arbiter.sel)
          bufferRead.coherency.fromUnique := slots.map(_.coherency.fromUnique).read(arbiter.sel)
          bufferRead.coherency.toShared := slots.map(_.coherency.toShared).read(arbiter.sel)
          bufferRead.coherency.probeId := slots.map(_.coherency.probeId).read(arbiter.sel)
        }
        wordIndex := wordIndex + U(bufferRead.fire && withCoherency.mux(bufferRead.coherency.dirty, True))
        when(bufferRead.fire && last) {
          slots.onMask(arbiter.oh)(_.writeCmdDone := True)
          arbiter.lock := 0
        }

        val cmd = bufferRead.stage()
        val word = victimBuffer.readSync(bufferRead.id @@ wordIndex, bufferRead.ready)
        bus.write.cmd.arbitrationFrom(cmd)
        bus.write.cmd.address := cmd.address
        bus.write.cmd.data := word
        bus.write.cmd.id := cmd.id
        bus.write.cmd.last := cmd.last
        if (withCoherency) {
          bus.write.cmd.coherent.release := cmd.coherency.release
          bus.write.cmd.coherent.dirty := cmd.coherency.dirty
          bus.write.cmd.coherent.fromUnique := cmd.coherency.fromUnique
          bus.write.cmd.coherent.toShared := cmd.coherency.toShared
          bus.write.cmd.coherent.probeId := cmd.coherency.probeId
          when(cmd.fire && cmd.last && !cmd.coherency.release) {
            slots.onSel(cmd.id) { s =>
              s.fire := True
            }
          }
        }

        when(bus.write.rsp.valid) {
          whenIndexed(slots, bus.write.rsp.id) { s =>
            s.fire := True
          }
        }
      }
    }


    def isLineBusy(address: UInt) = refill.isLineBusy(address) || writeback.isLineBusy(address)
//    def waysHazard(stages: Seq[Stage], address: Stageable[UInt]): Unit = {
//      for (s <- stages) {
//        s.overloaded(WAYS_HAZARD) := s(WAYS_HAZARD) | waysWrite.maskLast.andMask(waysWrite.addressLast === s(address)(lineRange))
//      }
//    }

    val ls = new Area {
      val rb0 = new lane.Execute(bankReadAt){
        val readAddress = MIXED_ADDRESS(lineRange.high downto log2Up(bankWidth / 8))
        val reservation = bankReadArbiter.create(1)
        for ((bank, bankId) <- banks.zipWithIndex) {
          BANK_BUSY(bankId) := bank.write.valid && bank.write.address === readAddress //Write to read hazard
          when(SEL){
            when(reservation.win){
              reservation.takeIt()
              bank.read.cmd.valid := !lane.isFreezed()
              bank.read.cmd.payload := readAddress
            } otherwise {
              lane.freezeIt()
            }
          }
        }
      }

      val rb1 = new lane.Execute(bankReadAt+1){
        def wayToBank(way: Int): UInt = {
          val wayId = U(way, log2Up(wayCount) bits)
          if (!reducedBankWidth) return wayId
          (wayId >> log2Up(bankCount / memToBankRatio)) @@ ((wayId + (MIXED_ADDRESS(log2Up(bankWidth / 8), log2Up(bankCount) bits))).resize(log2Up(bankCount / memToBankRatio)))
        }

        for ((bank, bankId) <- banks.zipWithIndex) {
          BANKS_WORDS(bankId) := banks(bankId).read.rsp
          BANK_BUSY_REMAPPED(bankId) := BANK_BUSY(wayToBank(bankId))
        }
      }

      val bm = new lane.Execute(bankMuxesAt){
        for ((bank, bankId) <- banks.zipWithIndex) {
          BANKS_MUXES(bankId) := BANKS_WORDS(bankId).subdivideIn(cpuWordWidth bits).read(MIXED_ADDRESS(bankWordToCpuWordRange))
        }
      }

      val bankMuxStd = !reducedBankWidth generate new lane.Execute(bankMuxAt){
        READ_DATA := OhMux.or(WAYS_HITS, BANKS_MUXES)
      }

      val bankMuxReduced = reducedBankWidth generate new lane.Execute(bankMuxAt){
        val wayId = OHToUInt(WAYS_HITS)
        val bankId = (wayId >> log2Up(bankCount / memToBankRatio)) @@ ((wayId + (MIXED_ADDRESS(log2Up(bankWidth / 8), log2Up(bankCount) bits))).resize(log2Up(bankCount / memToBankRatio)))
        READ_DATA := BANKS_MUXES.read(bankId) //MuxOH(WAYS_HITS, BANKS_MUXES)
      }


//      translatedStage(PHYSICAL_ADDRESS) := io.load.translated.physical
//      translatedStage(ABORD) := io.load.translated.abord
//
//      readTagsStage(PLRU) := plru.ram.readAsync(readTagsStage(MIXED_ADDRESS)(lineRange))

      val rt0 = new lane.Execute(wayReadAt){
        plru.read.cmd.valid := !lane.isFreezed()
        plru.read.cmd.payload := MIXED_ADDRESS(lineRange)
        val PLRU_BYPASS_VALID = insert(plru.write.valid && plru.write.address === plru.read.cmd.payload)
        val PLRU_BYPASS_VALUE = insert(plru.write.data)

        for ((way, wayId) <- ways.zipWithIndex){
          way.lsuRead.cmd.valid := !lane.isFreezed()
          way.lsuRead.cmd.payload := MIXED_ADDRESS(lineRange)
        }
      }

      val rt1 = new lane.Execute(wayReadAt + 1 - tagsReadAsync.toInt){
        this(PLRU) := plru.read.rsp
        val plruBypass = tagsReadAsync generate new Area{
          when(rt0.PLRU_BYPASS_VALID){
            this(PLRU) := rt0.PLRU_BYPASS_VALUE
          }
        }

        for ((way, wayId) <- ways.zipWithIndex) {
          WAYS_TAGS(wayId) := ways(wayId).lsuRead.rsp
        }
      }
      val hs  = new lane.Execute(hitsAt){
        for ((way, wayId) <- ways.zipWithIndex) {
          WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === PHYSICAL_ADDRESS(tagRange)
        }
      }
      val h = new lane.Execute(hitAt) {
        WAYS_HIT := B(WAYS_HITS).orR
      }

      val preCtrl = new lane.Execute(ctrlAt){
        NEED_UNIQUE := !LOAD || LR
        ABORD := False //TODO
        WAYS_HAZARD := 0 //TODO
        LOCKED := False //TODO
      }

//      val rcl = new lane.Execute(ctrlAt){
//        REFILL_HITS := B(refill.slots.map(r => r.valid && r.address(hazardCheckRange) === PHYSICAL_ADDRESS(hazardCheckRange)))
//      }

      val ctrl = new lane.Execute(ctrlAt) {
        val plruLogic = new Area {
          val core = new Plru(wayCount, false)
          core.io.context.state := PLRU
          core.io.update.id.assignDontCare()
          when(SEL) {
            plru.write.address := MIXED_ADDRESS(lineRange)
            plru.write.data := core.io.update.state
          }
        }

        val reservation = tagsWriteArbiter.create(2)
        val refillWay = CombInit(plruLogic.core.io.evict.id)
//        val refillWay = CombInit(wayRandom.value)
        val refillWayNeedWriteback = WAYS_TAGS.map(w => w.loaded && withCoherency.mux(True, w.dirty)).read(refillWay)
//        val refillHit = REFILL_HITS.orR
//        val refillLoaded = (B(refill.slots.map(_.loaded)) & REFILL_HITS).orR
        val lineBusy = isLineBusy(PHYSICAL_ADDRESS)
        val bankBusy = (BANK_BUSY_REMAPPED & WAYS_HITS) =/= 0
        val waysHitHazard = (WAYS_HITS & WAYS_HAZARD).orR
        val hitUnique = withCoherency.mux((WAYS_HITS & WAYS_TAGS.map(_.unique).asBits).orR, True)
        val uniqueMiss = NEED_UNIQUE && !hitUnique
        val wasDirty = (B(WAYS_TAGS.map(_.dirty)) & WAYS_HITS).orR
        val refillWayWasDirty = WAYS_TAGS.map(w => w.loaded && w.dirty).read(refillWay)

        //TODO !!! refill / writeback slots valid can go false faster than the main pipeline !freeze => need fix
        REDO := !WAYS_HIT || waysHitHazard || bankBusy || lineBusy || LOCKED || uniqueMiss
        MISS := !WAYS_HIT && !waysHitHazard && !lineBusy && !LOCKED
        FAULT := (WAYS_HITS & WAYS_TAGS.map(_.fault).asBits).orR
        val canRefill = !refill.full && !lineBusy && reservation.win && !(refillWayNeedWriteback && writeback.full) && !WAYS_HAZARD(refillWay)
        val askRefill = MISS && canRefill
        val askUpgrade = !MISS && canRefill && uniqueMiss
        val startRefill = isValid && askRefill
        val startUpgrade = isValid && askUpgrade
        val wayId = OHToUInt(WAYS_HITS)

        when(ABORD) {
          REDO := False
          MISS := False
          askUpgrade := False
        }

        when(startRefill || startUpgrade) {
          reservation.takeIt()

          refill.push.valid := True
          refill.push.address := PHYSICAL_ADDRESS
          refill.push.unique := NEED_UNIQUE
          refill.push.data := askRefill
        }

        when(askUpgrade) {
          refill.push.way := wayId
          refill.push.victim := 0
        } otherwise {
          refill.push.way := refillWay
          refill.push.victim := writeback.free.andMask(refillWayNeedWriteback && refillWayWasDirty)
        }

        when(startRefill) {
          waysWrite.mask(refillWay) := True
          waysWrite.address := MIXED_ADDRESS(lineRange)
          waysWrite.tag.loaded := True
          waysWrite.tag.address := PHYSICAL_ADDRESS(tagRange)
          waysWrite.tag.fault := False
          waysWrite.tag.dirty := False
          if (withCoherency) waysWrite.tag.unique := True

          writeback.push.valid := refillWayNeedWriteback
          writeback.push.address := (WAYS_TAGS(refillWay).address @@ MIXED_ADDRESS(lineRange)) << lineRange.low
          writeback.push.way := refillWay
          if (withCoherency) {
            writeback.push.dirty := wasDirty
            writeback.push.fromUnique := WAYS_TAGS(refillWay).unique
            writeback.push.toShared := False
            writeback.push.release := True
          }

          plru.write.valid := True
          plruLogic.core.io.update.id := refillWay
        }

        when(isValid && !REDO && !MISS) {
          plru.write.valid := True
          plruLogic.core.io.update.id := wayId
        }

//        REFILL_SLOT_FULL := MISS && !refillHit && refill.full
//        REFILL_SLOT := REFILL_HITS.andMask(!refillLoaded) | refill.free.andMask(askRefill)
      }
    }

    tagsWriteArbiter.build()
    bankWriteArbiter.build()
    bankReadArbiter.build()
  }
}
