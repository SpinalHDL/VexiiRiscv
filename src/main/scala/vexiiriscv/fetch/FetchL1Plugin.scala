package vexiiriscv.fetch

import spinal.lib.misc.plugin.FiberPlugin
import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4ReadOnly}
import spinal.lib.bus.tilelink
import spinal.lib.bus.amba4.axilite.{AxiLite4Config, AxiLite4ReadOnly}
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbParameter, BmbSourceParameter}
import spinal.lib.bus.tilelink.{M2sSupport, SizeRange}
import spinal.lib.misc.Plru
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService, PmaLoad, PmaLogic, PmaPort}
import vexiiriscv.misc._
import vexiiriscv._
import vexiiriscv.Global._
import Fetch._
import spinal.core.fiber.{Handle, Retainer}
import spinal.lib.system.tag.PmaRegion
import vexiiriscv.execute.lsu.LsuCommitProbe
import vexiiriscv.riscv.CSR
import vexiiriscv.schedule.ReschedulePlugin

import scala.collection.mutable.ArrayBuffer

case class FetchL1InvalidationCmd() extends Bundle //Empty for now
case class FetchL1InvalidationBus() extends Bundle {
  val cmd = Stream(FetchL1InvalidationCmd())
}

trait FetchL1Service{
  val invalidationRetainer = Retainer()
  val invalidationPorts = ArrayBuffer[FetchL1InvalidationBus]()
  def newInvalidationPort() = invalidationPorts.addRet(FetchL1InvalidationBus())
  def fetchProbe : FetchProbe
}

case class LsuL1InvalidationCmd() extends Bundle //Empty for now
case class LsuL1InvalidationBus() extends Bundle {
  val cmd = Stream(LsuL1InvalidationCmd())
}
trait LsuService{
  val invalidationRetainer = Retainer()
  val invalidationPorts = ArrayBuffer[LsuL1InvalidationBus]()
  def newInvalidationPort() = invalidationPorts.addRet(LsuL1InvalidationBus())
  def lsuCommitProbe : Flow[LsuCommitProbe]
  def getBlockSize : Int
}

trait LsuL1Service{
  def withCoherency : Boolean
  val regions = Handle[ArrayBuffer[PmaRegion]]()
}



class FetchL1Plugin(var translationStorageParameter: Any,
                    var translationPortParameter: Any,
                    var memDataWidth : Int,
                    var fetchDataWidth : Int,
                    var setCount: Int,
                    var wayCount: Int,
                    var refillCount: Int = 1,
                    var lineSize: Int = 64,
                    var readAt: Int = 0,
                    var hitsAt: Int = 1,
                    var hitAt: Int = 1,
                    var bankMuxesAt: Int = 1,
                    var bankMuxAt: Int = 2,
                    var ctrlAt: Int = 2,
                    var hitsWithTranslationWays: Boolean = false,
                    var reducedBankWidth: Boolean = false,
                    var tagsReadAsync: Boolean = false,
                    var bootMemClear : Boolean) extends FiberPlugin with FetchL1Service with InitService {

  def getBusParameter() = FetchL1BusParam(
    physicalWidth = PHYSICAL_WIDTH,
    dataWidth = memDataWidth,
    lineSize = lineSize,
    refillCount = refillCount,
    withBackPresure = false
  )


  override def fetchProbe: FetchProbe = during build FetchProbe()

  override def initHold(): Bool = logic.invalidate.firstEver || bootMemClear.mux(logic.initializer.busy, False)

  val logic = during setup new Area{
    val pp = host[FetchPipelinePlugin]
    val pcp = host[PcService]
    val rp = host[ReschedulePlugin]
    val ts = host[TrapService]
    val ats = host[AddressTranslationService]
    val pcs = host.get[PerformanceCounterService]
    val prefetcher = host.get[PrefetcherPlugin].map(_.io)
    val buildBefore = retains(pp.elaborationLock, ats.portsLock)
    val atsStorageLock = retains(ats.storageLock)
    val setupLock = retains(List(ts.trapLock, pcp.elaborationLock, rp.elaborationLock) ++ pcs.map(_.elaborationLock).toList)
    awaitBuild()

    Fetch.WORD_WIDTH.set(fetchDataWidth)

    val bus = master(FetchL1Bus(
      getBusParameter()
    ))

    val translationStorage = ats.newStorage(translationStorageParameter, PerformanceCounterService.ICACHE_TLB_CYCLES)
    atsStorageLock.release()

    val age = pp.getAge(ctrlAt, false)
    val trapPort = ts.newTrap(pp.getAge(ctrlAt), 0)

    val events = pcs.map(p => new Area {
      val access  = p.createEventPort(PerformanceCounterService.ICACHE_ACCESS)
      val miss    = p.createEventPort(PerformanceCounterService.ICACHE_MISS)
      val waiting = p.createEventPort(PerformanceCounterService.ICACHE_WAITING)
//      val dev0 = p.createEventPort(PerformanceCounterService.DEV)
//      val dev1 = p.createEventPort(PerformanceCounterService.DEV+1)
    })

    setupLock.release()

    val cacheSize = wayCount*setCount*lineSize
    val cpuWordWidth = fetchDataWidth
    val bytePerMemWord = memDataWidth / 8
    val bytePerFetchWord = cpuWordWidth / 8
    val waySize = cacheSize / wayCount
    val linePerWay = waySize / lineSize
    val memDataPerWay = waySize / bytePerMemWord
    val memData = HardType(Bits(memDataWidth bits))
    val memWordPerLine = lineSize / bytePerMemWord
    val tagWidth = PHYSICAL_WIDTH - log2Up(waySize)
    val tagRange = PHYSICAL_WIDTH - 1 downto log2Up(linePerWay * lineSize)
    val lineRange = tagRange.low - 1 downto log2Up(lineSize)
    val wordRange = log2Up(lineSize) - 1 downto log2Up(bytePerMemWord)
    val bankCount = wayCount
    val bankWidth = if (!reducedBankWidth) memDataWidth else Math.max(cpuWordWidth, memDataWidth / wayCount)
    val bankByteSize = cacheSize / bankCount
    val bankWordCount = bankByteSize * 8 / bankWidth
    val bankWordToCpuWordRange = log2Up(bankWidth / 8) - 1 downto log2Up(bytePerFetchWord)
    val memToBankRatio = bankWidth * bankCount / memDataWidth
    val bankWord = HardType(Bits(bankWidth bits))


    case class Tag() extends Bundle {
      val loaded = Bool()
      val error = Bool()
      val address = UInt(tagWidth bits)
    }

    val BANKS_WORDS = Payload(Vec.fill(bankCount)(bankWord()))
    val WAYS_TAGS = Payload(Vec.fill(wayCount)(Tag()))
    val WAYS_HITS = Payload(Vec.fill(wayCount)(Bool()))
    val WAYS_HIT = Payload(Bool())
    val HAZARD = Payload(Bool())
    val PREFETCH = Payload(Bool())
    val MIXED_PC = Payload(Global.PC)
    def MIXED_PC_SOLVED = prefetcher.nonEmpty.mux(MIXED_PC, WORD_PC)

    val BANKS_MUXES = Payload(Vec.fill(bankCount)(Bits(cpuWordWidth bits)))


    val banks = for (id <- 0 until bankCount) yield new Area {
      val mem = Mem(Bits(bankWidth bits), bankWordCount)
      if(bootMemClear) mem.randBoot()
      val write = mem.writePort
      val read = new Area {
        val cmd = Flow(mem.addressType)
        val rsp = mem.readSync(cmd.payload, cmd.valid)
        pp.fetch(readAt + 1)(BANKS_WORDS)(id) := rsp
        KeepAttribute(rsp)
      }
    }
    val waysWrite = new Area {
      val mask = Bits(wayCount bits)
      val address = UInt(log2Up(linePerWay) bits)
      val tag = Tag()

      mask := 0
      address.assignDontCare()
      tag.assignDontCare()
    }
    val ways = for (id <- 0 until wayCount) yield new Area {
      val mem = Mem.fill(linePerWay)(Tag())
      if(bootMemClear) mem.randBoot()
      mem.write(waysWrite.address, waysWrite.tag, waysWrite.mask(id))
      val read = new Area {
        val cmd = Flow(mem.addressType)
        val rsp = if (tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
        pp.fetch(readAt + (!tagsReadAsync).toInt)(WAYS_TAGS)(id) := rsp
        KeepAttribute(rsp)
      }
    }

    val PLRU_READ, PLRU_BYPASSED = Payload(Plru.State(wayCount))
    val x = pp.fetch(0)
    val plru = new Area {
      val mem = Mem.fill(linePerWay)(Plru.State(wayCount))
      val write = mem.writePort
      val read = new Area {
        val cmd = Flow(mem.addressType)
        val rsp = if (tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
        pp.fetch(readAt + (!tagsReadAsync).toInt)(PLRU_READ) := rsp
        KeepAttribute(rsp)
      }
    }

    val invalidate = new Area {
      invalidationRetainer.await()
      val cmd = Event
      cmd.valid := invalidationPorts.map(_.cmd.valid).orR

      val canStart = True
      val counter = Reg(UInt(log2Up(linePerWay) + 1 bits)) init (0)
      val counterIncr = counter + 1
      val done = counter.msb
      val last = counterIncr.msb
      val firstEver = RegInit(True) clearWhen (done)
      when(!done) {
        counter := counterIncr
        waysWrite.mask := (default -> true)
        waysWrite.address := counter.resized
        waysWrite.tag.loaded := False
      }

      when(done && cmd.valid && canStart) {
        counter := 0
      }

      invalidationPorts.foreach(_.cmd.ready := False)
      when(!done){
        pp.fetch(readAt).haltIt()
        when(last){
          invalidationPorts.foreach(_.cmd.ready := True)
        }
      }
    }

    val refill = new Area {
      val start = new Area {
        val valid = Bool()
        val address = UInt(PHYSICAL_WIDTH bits)
        val wayToAllocate = UInt(log2Up(wayCount) bits)
        val isIo = Bool()
        val hartId = HART_ID()
      }

      val slots = for(slotId <- 0 until refillCount) yield new Area{
        val id = slotId
        val valid = RegInit(False)
        val cmdSent = RegInit(True)
        val address = KeepAttribute(Reg(UInt(PHYSICAL_WIDTH bits)))
        val isIo = Reg(Bool())
        val hartId = Reg(HART_ID())
        val wayToAllocate = Reg(UInt(log2Up(wayCount) bits))
        val priority = Reg(Bits(refillCount bits))
        val askCmd = valid && !cmdSent
      }

      for(self <- slots; other <- slots; if self != other){
        self.priority(other.id) clearWhen(!other.valid)
      }

      import spinal.core.sim._
      val pushCounter = Reg(UInt(32 bits)) init (0) simPublic()

      val freeOh = OHMasking.first(slots.map(!_.valid))

      val hazard = slots.map(s => s.valid && s.address(lineRange) === start.address(lineRange)).orR
      when(start.valid && invalidate.done && !hazard) {
        slots.onMask(freeOh){ s =>
          s.valid := True
          s.address := start.address
          s.hartId := start.hartId
          s.isIo := start.isIo
          s.wayToAllocate := start.wayToAllocate
          s.cmdSent := False
          s.priority := B(slots.map(_.valid))
        }
        pushCounter := pushCounter + 1
      }



//      val oldDo = !valid && start.valid && invalidate.done
//      val oldAddress = RegNextWhen(start.address, oldDo)
//      val oldHit = ((oldAddress + lineSize) ^ start.address) >> log2Up(lineSize) === 0
//      events.get.dev0 := oldDo
//      events.get.dev1 := oldDo && oldHit


      invalidate.canStart clearWhen (slots.map(_.valid).orR || start.valid)

      val onCmd = new Area{
        val oh = B(for((self, slotId) <- slots.zipWithIndex) yield {
          self.askCmd && slots.filter(_ != self).map(s => !s.askCmd || !s.priority(slotId)).andR
        })
        val reader = slots.reader(oh, bypassIfSingle = true)
        bus.cmd.valid := oh.orR
        bus.cmd.address := reader(_.address(tagRange.high downto lineRange.low)) @@ U(0, lineRange.low bit)
        bus.cmd.io := reader(_.isIo)
        bus.cmd.id := OHToUInt(oh)
        when(bus.cmd.ready) {
          slots.onMask(oh) { s =>
            s.cmdSent := True
          }
        }
      }


      val onRsp = new Area{
        val rspIdReg = RegNextWhen(bus.rsp.id, bus.rsp.valid)
        val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

        val holdHarts = waysWrite.mask.orR || slots.map(s => s.valid && s.address(lineRange) === pp.fetch(readAt)(WORD_PC)(lineRange) && !(rspIdReg === s.id && wordIndex > pp.fetch(readAt)(WORD_PC)(wordRange))).orR
        pp.fetch(readAt).haltWhen(holdHarts)

        val firstCycle = RegInit(True) clearWhen (bus.rsp.fire)
        val reader = slots.reader(bus.rsp.id)
        val wayToAllocate = reader(_.wayToAllocate)
        val address = reader(_.address)
        when(invalidate.done) {
          waysWrite.mask(wayToAllocate) setWhen (bus.rsp.valid && (firstCycle || bus.rsp.error))
          waysWrite.address := address(lineRange)
          waysWrite.tag.loaded := True
          waysWrite.tag.error := bus.rsp.valid && bus.rsp.error
          waysWrite.tag.address := address(tagRange)
        }


        for ((bank, bankId) <- banks.zipWithIndex) {
          if (!reducedBankWidth) {
            bank.write.valid := bus.rsp.valid && wayToAllocate === bankId
            bank.write.address := address(lineRange) @@ wordIndex
            bank.write.data := bus.rsp.data
          } else {
            val sel = U(bankId) - wayToAllocate
            val groupSel = wayToAllocate(log2Up(bankCount) - 1 downto log2Up(bankCount / memToBankRatio))
            val subSel = sel(log2Up(bankCount / memToBankRatio) - 1 downto 0)
            bank.write.valid := bus.rsp.valid && groupSel === (bankId >> log2Up(bankCount / memToBankRatio))
            bank.write.address := address(lineRange) @@ wordIndex @@ (subSel)
            bank.write.data := bus.rsp.data.subdivideIn(bankCount / memToBankRatio slices)(subSel)
          }
        }

        bus.rsp.ready := True
        when(bus.rsp.valid) {
          wordIndex := (wordIndex + 1).resized
          when(wordIndex === wordIndex.maxValue) {
            firstCycle := True
            slots.onSel(bus.rsp.id)(_.valid := False)
          }
        }
      }
    }

    val translationPort = ats.newTranslationPort(
      nodes = Seq(pp.fetch(readAt).down, pp.fetch(readAt+1).down),
      rawAddress = MIXED_PC_SOLVED,
      forcePhysical = pp.fetch(readAt).insert(False),
      usage = AddressTranslationPortUsage.FETCH,
      portSpec = translationPortParameter,
      storageSpec = translationStorage
    )
    val tpk = translationPort.keys

    val cmd = new pp.Fetch(readAt) {
      val ra1 = pp.fetch(readAt + 1).up
      val doIt = ra1.ready || (!ra1.valid && prefetcher.nonEmpty.mux(!ra1(PREFETCH), True)) // Better timings than up.isReady, ra1.cancel not necessary as cancel do not collapse bubbles
      for ((bank, bankId) <- banks.zipWithIndex) {
        bank.read.cmd.valid := doIt
        bank.read.cmd.payload := WORD_PC(lineRange.high downto log2Up(bankWidth / 8))
      }

      for((way, wayId) <- ways.zipWithIndex) {
        way.read.cmd.valid := doIt
        way.read.cmd.payload := MIXED_PC_SOLVED(lineRange)
      }

      prefetcher match {
        case Some(prefetcher) => {
          prefetcher.cmd.ready := down.isReady
          PREFETCH := prefetcher.cmd.valid
          MIXED_PC_SOLVED := PREFETCH ? prefetcher.cmd.pc | WORD_PC
          pp.fetch(readAt).haltWhen(PREFETCH)

          for (ctrlId <- readAt + 1 to ctrlAt) {
            pp.fetch(ctrlId).up(PREFETCH).setAsReg.init(False)
          }
        }
        case None =>
      }

      plru.read.cmd.valid := doIt
      plru.read.cmd.payload := MIXED_PC_SOLVED(lineRange)

      val PLRU_BYPASS_VALID = insert(plru.write.valid && plru.write.address === plru.read.cmd.payload)
      val PLRU_BYPASS_DATA = insert(plru.write.data)

      val TAGS_UPDATE = insert(waysWrite.mask.orR)
      val TAGS_UPDATE_ADDRESS = insert(waysWrite.address)
    }

    val plruBypass = new pp.Fetch(readAt + 1){
      this (PLRU_BYPASSED) := PLRU_READ
      if(!tagsReadAsync) when(cmd.PLRU_BYPASS_VALID) {
        this (PLRU_BYPASSED) := cmd.PLRU_BYPASS_DATA
      }
    }

    val muxes = new pp.Fetch(bankMuxesAt) {
      for ((bank, bankId) <- banks.zipWithIndex) {
        BANKS_MUXES(bankId) := BANKS_WORDS(bankId).subdivideIn(cpuWordWidth bits).read(WORD_PC(bankWordToCpuWordRange))
      }
    }

    val bankMux = new pp.Fetch(bankMuxAt) {
      if(!reducedBankWidth) WORD := OhMux.or(WAYS_HITS, BANKS_MUXES)
      val reduced = reducedBankWidth generate new Area{
        val wayId = OHToUInt(WAYS_HITS)
        val bankId = (wayId >> log2Up(bankCount / memToBankRatio)) @@ ((wayId + (WORD_PC(log2Up(bankWidth / 8), log2Up(bankCount) bits))).resize(log2Up(bankCount / memToBankRatio)))
        WORD := BANKS_MUXES.read(bankId) //MuxOH(WAYS_HITS, BANKS_MUXES)
      }
    }

    val hazard = new pp.Fetch(readAt+1) {
      import cmd._
      val pageRange = 11 downto wordRange.high + 1
      HAZARD := TAGS_UPDATE && TAGS_UPDATE_ADDRESS === MIXED_PC_SOLVED(lineRange) // || REFILL_VALID && REFILL_ADDRESS(pageRange) === WORD_PC(pageRange) && REFILL_WORD <= WORD_PC(wordRange)
    }

    val hits = new pp.Fetch(hitsAt){
      val withDirectHits = !hitsWithTranslationWays || translationPort.wayCount == 0
      val w = for((way, wayId) <- ways.zipWithIndex) yield new Area{
        if (withDirectHits) WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === tpk.TRANSLATED(tagRange)
        val indirect = if(!withDirectHits) new Area{
          val wayTlbHits = (0 until translationPort.wayCount) map (tlbWayId => WAYS_TAGS(wayId).address === tpk.WAYS_PHYSICAL(tlbWayId)(tagRange) && tpk.WAYS_OH(tlbWayId))
          val translatedHits = wayTlbHits.orR
          val bypassHits = WAYS_TAGS(wayId).address === MIXED_PC_SOLVED >> tagRange.low
          WAYS_HITS(wayId) := (tpk.BYPASS_TRANSLATION ? bypassHits | translatedHits) & WAYS_TAGS(wayId).loaded
        }
      }
    }

    val hit = new pp.Fetch(hitAt) {
      WAYS_HIT := B(WAYS_HITS).orR
    }

    val ctrl = new pp.Fetch(ctrlAt){
      val pmaPort = new PmaPort(Global.PHYSICAL_WIDTH, List(lineSize), List(PmaLoad))
      pmaPort.cmd.address := tpk.TRANSLATED

      val plruLogic = new Area {
        val core = new Plru(wayCount, false)
        core.io.context.state := PLRU_BYPASSED
        core.io.update.id := OHToUInt(WAYS_HITS)

        val buffer = cloneOf(plru.write)
        buffer >-> plru.write
        buffer.valid := up.isValid && up.isReady
        buffer.address := MIXED_PC_SOLVED(lineRange)
        buffer.data := core.io.update.state

        refill.start.wayToAllocate := core.io.evict.id
//          refill.start.wayToAllocate := refill.randomWay
      }

      val dataAccessFault = this(WAYS_TAGS).reader(WAYS_HITS)(_.error) && !HAZARD

      //trapSent is required, as the CPU will continue to fetch stuff as long as the trap request do not reach decode stages
      assert(Global.HART_COUNT.get == 1) //Would require proper clearWhen(up.isCancel) and trapSent per hart
      val trapSent = RegInit(False) setWhen (trapPort.valid) clearWhen (up.isCancel)

      trapPort.valid := False
      trapPort.tval := Fetch.WORD_PC.asBits
      trapPort.hartId := Global.HART_ID
      trapPort.exception.assignDontCare()
      trapPort.code.assignDontCare()
      trapPort.arg.allowOverride() := 0

      val allowRefill = !WAYS_HIT && !HAZARD
      when(!WAYS_HIT || HAZARD) {
        trapPort.valid := True
        trapPort.exception := False
        trapPort.code := TrapReason.REDO
      }

      when(dataAccessFault || pmaPort.rsp.fault) {
        allowRefill := False
        trapPort.valid := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT
      }

      when(tpk.PAGE_FAULT || !tpk.ALLOW_EXECUTE) {
        allowRefill := False
        trapPort.valid := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_PAGE_FAULT
      }

      when(tpk.ACCESS_FAULT) {
        allowRefill := False
        trapPort.valid := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT
      }

      trapPort.arg(0, 2 bits) := TrapArg.FETCH
      trapPort.arg(2, ats.getStorageIdWidth() bits) := ats.getStorageId(translationStorage)
      when(tpk.REFILL) {
        allowRefill := False
        trapPort.valid := True
        trapPort.exception := False
        trapPort.code := TrapReason.MMU_REFILL
      }
      when(tpk.HAZARD) {
        allowRefill := False
        trapPort.valid := True
        trapPort.exception := False
        trapPort.code := TrapReason.REDO
      }
      when(Fetch.PC_FAULT) {
        allowRefill := False
        trapPort.valid := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT
        when(!tpk.BYPASS_TRANSLATION){
          trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_PAGE_FAULT
        }
      }


      refill.start.valid := allowRefill && !trapSent
      refill.start.address := tpk.TRANSLATED
      refill.start.hartId := HART_ID
      refill.start.isIo := pmaPort.rsp.io

      TRAP := trapPort.valid || trapSent

      when(!isValid || trapSent){
        trapPort.valid := False
      }

      when(!isValid && prefetcher.nonEmpty.mux(!PREFETCH, True)) {
        refill.start.valid := False
      }

      val firstCycle = RegInit(True) clearWhen(isValid) setWhen(!isValid || isReady || up.isCanceling)
      prefetcher.map{ p =>
        p.probe.valid := isValid && firstCycle
        p.probe.pc := WORD_PC
        p.probe.refill := refill.start.valid
      }

      val onEvents = events.map( e => new Area {
        val waiting = RegInit(False) clearWhen (isValid && !TRAP) setWhen (e.miss)
        e.access := up.isMoving
        e.miss   := up.isMoving && allowRefill
        e.waiting := waiting
      })
    }

    assert(!(refill.slots.map(_.valid).orR && !invalidate.done))

    when(!invalidate.done) {
      plru.write.valid := True
      plru.write.address := invalidate.counter.resized
      plru.write.data.clearAll()
    }

    val initializer = bootMemClear generate new Area {
      val counter = Reg(UInt(log2Up(banks(0).mem.wordCount) + 1 bits)) init (0)
      val busy = !counter.msb
      when(busy) {
        counter := counter + 1
        for (bank <- banks; port = bank.write) {
          port.valid := True
          port.address := counter.resized
          port.data := 0
        }
      }
    }

    buildBefore.release()
  }

  val regions = Handle[ArrayBuffer[PmaRegion]]()
  val pmaBuilder = during build new PmaLogic(logic.ctrl.pmaPort, regions.filter(_.isExecutable))
}

