package vexiiriscv.fetch

import spinal.core._
import spinal.core.fiber.Handle
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.DebugId
import spinal.lib.system.tag.{MappedTransfers, PmaRegion}
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService, PmaLoad, PmaLogic, PmaPort}
import vexiiriscv.misc.{PerformanceCounterService, TrapArg, TrapReason, TrapService}
import vexiiriscv.riscv.CSR

import scala.collection.mutable.ArrayBuffer

object FetchCachelessPlugin{
  val ID_WIDTH = blocking[Int]
  val ID = blocking[Int]
}

//TODO avoid cmd fork on unmapped memory space
class FetchCachelessPlugin(var wordWidth : Int,
                           var translationStorageParameter: Any,
                           var translationPortParameter: Any,
                           var addressAt: Int = 0,
                           var pmaAt: Int = 0,
                           var forkAt : Int = 0,
                           var joinAt : Int = 1,
                           var cmdForkPersistence : Boolean = true) extends FiberPlugin{
  val regions = Handle[ArrayBuffer[PmaRegion]]()

  val logic = during setup new Area{
    val pp = host[FetchPipelinePlugin]
    val ts = host[TrapService]
    val ats = host[AddressTranslationService]
    val buildBefore = retains(pp.elaborationLock, ats.portsLock)
    val atsStorageLock = retains(ats.storageLock)
    val trapLock =  ts.trapLock()
    awaitBuild()

    Fetch.WORD_WIDTH.set(wordWidth)

    val translationStorage = ats.newStorage(translationStorageParameter, PerformanceCounterService.ICACHE_TLB_CYCLES)
    atsStorageLock.release()

    val trapPort = ts.newTrap(pp.getAge(joinAt), 0)
    trapLock.release()

    val idCount = joinAt - forkAt + 1
    val p = CachelessBusParam(PHYSICAL_WIDTH, Fetch.WORD_WIDTH, idCount, cmdForkPersistence)
    val bus = master(CachelessBus(p))

    val BUFFER_ID = Payload(UInt(log2Up(idCount) bits))

    val buffer = new Area{
      val reserveId = Counter(idCount)
      val inflight = Vec.fill(idCount)(RegInit(False))
      val words = Mem.fill(idCount)(CachelessRsp(p, false))
      val write = words.writePort()
      val reservedHits = for (ctrlId <- forkAt+1 to joinAt; ctrl = pp.fetch(ctrlId)) yield {
        ctrl.isValid && ctrl(BUFFER_ID) === reserveId
      }
      val full = CombInit(reservedHits.orR || inflight.read(reserveId)) //TODO that's one cycle late, can use sort of ahead value

      val inflightSpawn = Bool()
      when(inflightSpawn) {
        inflight(reserveId) := True
      }

      write.valid := False
      write.address := bus.rsp.id
      write.data.assignSomeByName(bus.rsp.payload)
      when(bus.rsp.valid) {
        write.valid := True
        inflight(bus.rsp.id) := False
      }
    }

    val onAddress = new pp.Fetch(addressAt) {
      val translationPort = ats.newTranslationPort(
        nodes = Seq(down),
        rawAddress = Fetch.WORD_PC,
        forcePhysical = insert(False),
        usage = AddressTranslationPortUsage.FETCH,
        portSpec = translationPortParameter,
        storageSpec = translationStorage
      )
    }
    val tpk = onAddress.translationPort.keys

    val onPma = new pp.Fetch(pmaAt) {
      val port = new PmaPort(Global.PHYSICAL_WIDTH, List(Fetch.WORD_WIDTH / 8), List(PmaLoad))
      port.cmd.address := tpk.TRANSLATED
      val RSP = insert(port.rsp)
    }

    val fork = new pp.Fetch(forkAt){
      val fresh = (forkAt == 0).option(host[PcPlugin].forcedSpawn())
      val forked = forkStream(fresh)
      val halted = forked.haltWhen(buffer.full)
      val translated = cloneOf(bus.cmd)
      translated.arbitrationFrom(halted)
      translated.id := buffer.reserveId
      translated.address := tpk.TRANSLATED
      translated.address(Fetch.SLICE_RANGE) := 0
      val persistent = translated.pipelined(s2m = cmdForkPersistence)
      bus.cmd << persistent

      buffer.inflightSpawn := translated.fire

      if (cmdForkPersistence) {
        bus.cmd.assertPersistence()
      }

      BUFFER_ID := buffer.reserveId

      val PMA_FAULT = insert(onPma.RSP.fault)
      when(tpk.HAZARD || tpk.REFILL || PMA_FAULT) {
        halted.valid := False
      }otherwise {
        when(up.isMoving) {
          buffer.reserveId.increment()
        }
      }
    }

    val join = new pp.Fetch(joinAt){
      val haltIt = buffer.inflight.read(BUFFER_ID)
      val rsp = CombInit(buffer.words.readAsync(BUFFER_ID))
      // Implement bus rsp bypass into the pipeline (without using the buffer)
      // TODO this one can be optional
      when(bus.rsp.valid && bus.rsp.id === BUFFER_ID){
        haltIt := False
        rsp.assignSomeByName(bus.rsp.payload)
      }
      Fetch.WORD := rsp.word

      //trapSent is required, as the CPU will continue to fetch stuff as long as the trap request do not reach decode stages
      assert(Global.HART_COUNT.get == 1) //Would require proper clearWhen(up.isCancel) and trapSent per hart
      val trapSent = RegInit(False) setWhen(trapPort.valid) clearWhen(up.isCancel)

      TRAP := False
      trapPort.valid := TRAP && !trapSent
      trapPort.tval := Fetch.WORD_PC.asBits
      trapPort.hartId := Global.HART_ID
      trapPort.exception.assignDontCare()
      trapPort.code.assignDontCare()
      trapPort.arg.allowOverride() := 0

      when(rsp.error || fork.PMA_FAULT){
        TRAP := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT
      }

      when(tpk.PAGE_FAULT || !tpk.ALLOW_EXECUTE) {
        TRAP := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_PAGE_FAULT
      }

      when(tpk.ACCESS_FAULT) {
        TRAP := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT
      }

      trapPort.arg(0, 2 bits) := TrapArg.FETCH
      trapPort.arg(2, ats.getStorageIdWidth() bits) := ats.getStorageId(translationStorage)
      when(tpk.REFILL) {
        TRAP := True
        trapPort.exception := False
        trapPort.code := TrapReason.MMU_REFILL
      }
      when(tpk.HAZARD) {
        TRAP := True
        trapPort.exception := False
        trapPort.code := TrapReason.REDO
      }

      TRAP.clearWhen(!isValid || haltIt)

      haltWhen(haltIt)
    }
    buildBefore.release()
  }

  val pmaBuilder = during build new PmaLogic(logic.onPma.port, regions.filter(_.isExecutable))
}


//      val persist = (forkAt > 0 && cmdForkPersistence) generate new Area{
//        val started = RegNext(translated.isStall) init(False)
//        translated.valid setWhen(started)
//        pp.setPersistence(forkAt)
//      }