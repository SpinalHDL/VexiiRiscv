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
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationReq, AddressTranslationService, PmaLoad, PmaLogic, PmaPort, PmpService}
import vexiiriscv.misc.{PerformanceCounterService, TrapArg, TrapReason, TrapService}
import vexiiriscv.riscv.CSR

import scala.collection.mutable.ArrayBuffer

object FetchCachelessPlugin{
  val ID_WIDTH = blocking[Int]
  val ID = blocking[Int]
}

/**
 * Implement the instruction fetch bus without L1 cache.
 * The main particularity of this implementation is that it support out of order memory busses
 */
class FetchCachelessPlugin(var wordWidth : Int,
                           var translationStorageParameter: Any,
                           var translationPortParameter: Any,
                           var pmpPortParameter : Any,
                           var addressAt: Int = 0,
                           var pmaAt: Int = 0,
                           var forkAt : Int = 0,
                           var joinAt : Int = 1,
                           var cmdForkPersistence : Boolean = true) extends FiberPlugin{
  val regions = Handle[ArrayBuffer[PmaRegion]]()

  val logic = during setup new Area{
    // * Plugins interlocking *
    val pp = host[FetchPipelinePlugin]
    val ps = host[PmpService]
    val ts = host[TrapService]
    val ats = host[AddressTranslationService]
    val buildBefore = retains(pp.elaborationLock, ats.portsLock, ps.portsLock)
    val atsStorageLock = retains(ats.storageLock)
    val trapLock =  ts.trapLock()
    awaitBuild()

    // * Plugins interfaces *
    Fetch.WORD_WIDTH.set(wordWidth)

    val translationStorage = ats.newStorage(translationStorageParameter, PerformanceCounterService.ICACHE_TLB_CYCLES)
    atsStorageLock.release()

    val trapPort = ts.newTrap(pp.getAge(joinAt), 0)
    trapLock.release()

    // * Hardware generation *
    val idCount = joinAt - forkAt + 1
    val p = CachelessBusParam(PHYSICAL_WIDTH, Fetch.WORD_WIDTH, idCount, cmdForkPersistence)
    val bus = master(CachelessBus(p))

    val BUFFER_ID = Payload(UInt(log2Up(idCount) bits))

    // Storage and reorder of the memory bus responses
    val buffer = new Area {
      val reserveId = Counter(idCount)
      val inflight = Vec.fill(idCount)(RegInit(False))
      val words = Mem.fill(idCount)(CachelessRsp(p, false))
      val write = words.writePort()
      val reservedHits = for (ctrlId <- forkAt+1 to joinAt; ctrl = pp.fetch(ctrlId)) yield {
        ctrl.isValid && ctrl(BUFFER_ID) === reserveId
      }
      val full = CombInit(reservedHits.orR || inflight.read(reserveId)) // If this create timings issues, that's one cycle late, can use sort of ahead value

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
      val request = AddressTranslationReq(
        PRE_ADDRESS    = Fetch.WORD_PC,
        LOAD           = insert(False),
        STORE          = insert(False),
        EXECUTE        = insert(True),
        FORCE_PHYSICAL = insert(False)
      )
      val translationPort = ats.newTranslationPort(
        nodes = Seq(down),
        req = request,
        usage = AddressTranslationPortUsage.FETCH,
        portSpec = translationPortParameter,
        storageSpec = translationStorage
      )
    }
    val tpk = onAddress.translationPort.keys

    val pmpPort = ps.createPmpPort(
      nodes = List.tabulate(joinAt+1)(pp.fetch(_).down),
      physicalAddress = tpk.TRANSLATED,
      forceCheck = _ => False,
      read = _ => False,
      write = _ => False,
      execute = _ => True,
      portSpec = pmpPortParameter,
      storageSpec = null
    )

    val onPma = new pp.Fetch(pmaAt) {
      val port = new PmaPort(Global.PHYSICAL_WIDTH, List(Fetch.WORD_WIDTH / 8), List(PmaLoad))
      port.cmd.address := tpk.TRANSLATED
      val RSP = insert(port.rsp)
    }

    // Logic to fork the fetch pipeline toward the memory bus cmd
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

    // Logic to join the memory bus rsp to the pipeline and the buffer
    val join = new pp.Fetch(joinAt){
      val haltIt = buffer.inflight.read(BUFFER_ID)
      val rsp = CombInit(buffer.words.readAsync(BUFFER_ID))
      // Implement bus rsp bypass into the pipeline (without using the buffer).
      // To save area an option could be added to disable that.
      when(bus.rsp.valid && bus.rsp.id === BUFFER_ID){
        haltIt := False
        rsp.assignSomeByName(bus.rsp.payload)
      }
      Fetch.WORD := rsp.word

      // trapSent is required, as the CPU will continue to fetch stuff as long as
      // the trap request do not reach decode stages.
      assert(Global.HART_COUNT.get == 1) // Would require proper clearWhen(up.isCancel) and trapSent per hart
      val trapSent = RegInit(False) setWhen(trapPort.valid) clearWhen(up.isCancel)

      TRAP := False
      trapPort.valid := TRAP && !trapSent
      trapPort.tval := Fetch.WORD_PC.asBits
      trapPort.hartId := Global.HART_ID
      trapPort.exception.assignDontCare()
      trapPort.code.assignDontCare()
      trapPort.arg.allowOverride() := 0

      when(rsp.error || fork.PMA_FAULT || pmpPort.ACCESS_FAULT){
        TRAP := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT
      }

      when(tpk.PAGE_FAULT) {
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
