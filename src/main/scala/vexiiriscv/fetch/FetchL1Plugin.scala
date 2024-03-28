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
}

case class LsuL1InvalidationCmd() extends Bundle //Empty for now
case class LsuL1InvalidationBus() extends Bundle {
  val cmd = Stream(LsuL1InvalidationCmd())
}
trait LsuService{
  val invalidationRetainer = Retainer()
  val invalidationPorts = ArrayBuffer[LsuL1InvalidationBus]()
  def newInvalidationPort() = invalidationPorts.addRet(LsuL1InvalidationBus())
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
                    var lineSize: Int = 64,
                    var readAt: Int = 0,
                    var hitsAt: Int = 1,
                    var hitAt: Int = 1,
                    var bankMuxesAt: Int = 1,
                    var bankMuxAt: Int = 2,
                    var ctrlAt: Int = 2,
                    var hitsWithTranslationWays: Boolean = false,
                    var reducedBankWidth: Boolean = false,
                    var tagsReadAsync: Boolean = false) extends FiberPlugin with FetchL1Service with InitService {

  def getBusParameter() = FetchL1BusParam(
    physicalWidth = PHYSICAL_WIDTH,
    dataWidth = memDataWidth,
    lineSize = lineSize,
    withBackPresure = false
  )


  override def initHold(): Bool = logic.invalidate.firstEver

  val logic = during setup new Area{
    val pp = host[FetchPipelinePlugin]
    val pcp = host[PcService]
    val rp = host[ReschedulePlugin]
    val ts = host[TrapService]
    val ats = host[AddressTranslationService]
    val buildBefore = retains(pp.elaborationLock, ats.portsLock)
    val atsStorageLock = retains(ats.storageLock)
    val setupLock = retains(ts.trapLock, pcp.elaborationLock, rp.elaborationLock)
    awaitBuild()

    Fetch.WORD_WIDTH.set(fetchDataWidth)

    val bus = master(FetchL1Bus(
      getBusParameter()
    ))

    val translationStorage = ats.newStorage(translationStorageParameter)
    atsStorageLock.release()

    val age = pp.getAge(ctrlAt, false)
    val trapPort = ts.newTrap(pp.getAge(ctrlAt), 0)
    val holdPorts = (0 until HART_COUNT).map(pcp.newHoldPort(_))
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

    val BANKS_MUXES = Payload(Vec.fill(bankCount)(Bits(cpuWordWidth bits)))


    val banks = for (id <- 0 until bankCount) yield new Area {
      val mem = Mem(Bits(bankWidth bits), bankWordCount)
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

      val fire = False
      val valid = RegInit(False) clearWhen (fire)
      val firstCycle = RegNext(False)
      val address = KeepAttribute(Reg(UInt(PHYSICAL_WIDTH bits)))
      val isIo = Reg(Bool())
      val hartId = Reg(HART_ID())
      val hadError = RegInit(False)
      val wayToAllocate = Reg(UInt(log2Up(wayCount) bits))

      import spinal.core.sim._

      val pushCounter = Reg(UInt(32 bits)) init (0) simPublic()

      when(!valid) {
        when(start.valid && invalidate.done) {
          valid := True
          pushCounter := pushCounter + 1
          firstCycle := True
        }
        address := start.address
        hartId := start.hartId
        isIo := start.isIo
        wayToAllocate := start.wayToAllocate
      }


      invalidate.canStart clearWhen (valid || start.valid)

      val cmdSent = RegInit(False) setWhen (bus.cmd.fire) clearWhen (fire)
      bus.cmd.valid := valid && !cmdSent
      bus.cmd.address := address(tagRange.high downto lineRange.low) @@ U(0, lineRange.low bit)
      bus.cmd.io := isIo

      val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

      for ((port, i) <- holdPorts.zipWithIndex) {
        port := valid && hartId === i && wordIndex <= address(wordRange)
      }

      when(invalidate.done) {
        waysWrite.mask(wayToAllocate) setWhen (firstCycle || bus.rsp.valid && bus.rsp.error)
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
        hadError.setWhen(bus.rsp.error)
        when(wordIndex === wordIndex.maxValue) {
          fire := True
        }
      }

      hadError clearWhen (fire)
    }


    val translationPort = ats.newTranslationPort(
      nodes = Seq(pp.fetch(readAt).down, pp.fetch(readAt+1).down),
      rawAddress = Fetch.WORD_PC,
      forcePhysical = pp.fetch(readAt).insert(False),
      usage = AddressTranslationPortUsage.FETCH,
      portSpec = translationPortParameter,
      storageSpec = translationStorage
    )
    val tpk = translationPort.keys

    val cmd = new pp.Fetch(readAt) {
      val ra1 = pp.fetch(readAt + 1).up
      val doIt = ra1.ready || !ra1.valid // Better timings than up.isReady, ra1.cancel not necessary as cancel do not collapse bubbles
      for ((bank, bankId) <- banks.zipWithIndex) {
        bank.read.cmd.valid := doIt
        bank.read.cmd.payload := WORD_PC(lineRange.high downto log2Up(bankWidth / 8))
      }

      for((way, wayId) <- ways.zipWithIndex) {
        way.read.cmd.valid := doIt
        way.read.cmd.payload := WORD_PC(lineRange)
      }

      plru.read.cmd.valid := doIt
      plru.read.cmd.payload := WORD_PC(lineRange)

      val PLRU_BYPASS_VALID = insert(plru.write.valid && plru.write.address === plru.read.cmd.payload)
      val PLRU_BYPASS_DATA = insert(plru.write.data)

      val REFILL_VALID = insert(refill.valid)
      val REFILL_ADDRESS = insert(refill.address)
      val REFILL_WORD = insert(refill.wordIndex)
      val TAGS_UPDATE = insert(waysWrite.mask.orR)
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
      HAZARD := TAGS_UPDATE || REFILL_VALID && REFILL_ADDRESS(pageRange) === WORD_PC(pageRange) && REFILL_WORD <= WORD_PC(wordRange)
    }

    val hits = new pp.Fetch(hitsAt){
      val withDirectHits = !hitsWithTranslationWays || translationPort.wayCount == 0
      val w = for((way, wayId) <- ways.zipWithIndex) yield new Area{
        if (withDirectHits) WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === tpk.TRANSLATED(tagRange)
        val indirect = if(!withDirectHits) new Area{
          val wayTlbHits = (0 until translationPort.wayCount) map (tlbWayId => WAYS_TAGS(wayId).address === tpk.WAYS_PHYSICAL(tlbWayId)(tagRange) && tpk.WAYS_OH(tlbWayId))
          val translatedHits = wayTlbHits.orR
          val bypassHits = WAYS_TAGS(wayId).address === WORD_PC >> tagRange.low
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

        plru.write.valid := False
        plru.write.address := WORD_PC(lineRange)
        plru.write.data := core.io.update.state

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

      val allowRefill = !WAYS_HIT && !HAZARD && !trapSent

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
      when(tpk.REDO) {
        trapPort.valid := True
        trapPort.exception := False
        trapPort.code := TrapReason.MMU_REFILL
      }

      refill.start.valid := allowRefill && !trapSent
      refill.start.address := tpk.TRANSLATED
      refill.start.hartId := HART_ID
      refill.start.isIo := pmaPort.rsp.io

      TRAP := trapPort.valid || trapSent

      when(!isValid){
        refill.start.valid := False
      }
      when(!isValid || trapSent){
        trapPort.valid := False
      }

      when(up.isValid && up.isReady){
        plru.write.valid := True
      }
    }

    assert(!(refill.valid && !invalidate.done))

    when(!invalidate.done) {
      plru.write.valid := True
      plru.write.address := invalidate.counter.resized
      plru.write.data.clearAll()
    }

    buildBefore.release()
  }

  val regions = Handle[ArrayBuffer[PmaRegion]]()
  val pmaBuilder = during build new PmaLogic(logic.ctrl.pmaPort, regions.filter(_.isExecutable))
}

