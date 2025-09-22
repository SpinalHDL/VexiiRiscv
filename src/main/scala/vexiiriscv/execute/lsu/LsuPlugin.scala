package vexiiriscv.execute.lsu

import spinal.core._
import spinal.core.fiber.Handle
import spinal.core.sim.SimDataPimper
import spinal.lib
import spinal.lib._
import spinal.lib.bus.tilelink.M2sTransfers
import spinal.lib.bus.tilelink.coherent.{FlushBus, FlushParam}
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.system.tag.PmaRegion
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.decode.Decode.UOP
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService, DBusAccessService, PmaLoad, PmaLogic, PmaPort, PmaStore, PmpService}
import vexiiriscv.misc.{AddressToMask, LsuTriggerService, PerformanceCounterService, PrivilegedPlugin, TrapArg, TrapReason, TrapService}
import vexiiriscv.riscv.Riscv.{FLEN, LSLEN, XLEN}
import vexiiriscv.riscv._
import vexiiriscv.schedule.{DispatchPlugin, ScheduleService}
import vexiiriscv.{Global, riscv}
import vexiiriscv.execute._
import vexiiriscv.execute.lsu.AguPlugin._
import vexiiriscv.execute.lsu.LsuL1.{HAZARD}

import scala.collection.mutable.ArrayBuffer

object LsuL1CmdOpcode extends SpinalEnum{
  val LSU, ACCESS, STORE_BUFFER, FLUSH, PREFETCH = newElement()
}

case class LsuL1Cmd() extends Bundle {
  val op = LsuL1CmdOpcode()
  val address = LsuL1.MIXED_ADDRESS()
  val size = SIZE()
  val load, store, atomic = Bool()
  val clean, invalidate = Bool()
  val storeId = Decode.STORE_ID()
}

case class StoreBufferOp() extends Bundle {
  val address = Global.PHYSICAL_ADDRESS()
  val data = LsuL1.WRITE_DATA()
  val size = LsuL1.SIZE()
  val storeId = Decode.STORE_ID()
}


/**
 * The LsuPlugin does many things :
 * - Handle the AGU (Address Generation Unit)
 * - Interface with the L1 cache
 * - Handle the MMU integration
 * - Implement the PMA checks(Physical Memory Access)
 * - Implement the IO accesses
 * - Implement the AMO/LR/SC instructions
 * - Implement a store buffer
 * - Implement the prefetching request
 * - Implement RISC-V debug triggers related to memory load/store
 *
 * So, it has a lot of different functionalities which sometimes are tightly coupled together in order to reduce area and increase FMax
 * which can make it challenging to read.
 */
class LsuPlugin(var layer : LaneLayer,
                var withRva : Boolean,
                var translationStorageParameter: Any,
                var translationPortParameter: Any,
                var pmpPortParameter : Any,
                var softwarePrefetch: Boolean,
                var withCbm: Boolean,
                var withLlcFlush : Boolean = false,
                var addressAt: Int = 0,
                var triggerAt : Int = 1,
                var pmaAt : Int = 1,
                var ctrlAt: Int = 2,
                var wbAt : Int = 2,
                var storeRs2At : Int = 0, //Note that currently, it only apply for integer store (not float store)
                var storeBufferSlots : Int = 0,
                var storeBufferOps : Int = 0) extends FiberPlugin with DBusAccessService with LsuCachelessBusProvider with LsuService with CmoService{
  if(withLlcFlush) assert(withCbm)
  def withL1Cmb = withCbm && !withLlcFlush

  override def accessRefillCount: Int = 0
  override def accessWake: Bits = B(0)

  override def withSoftwarePrefetch: Boolean = softwarePrefetch
  override def getLsuCachelessBus(): LsuCachelessBus = logic.bus
  override def lsuCommitProbe: Flow[LsuCommitProbe] = logic.commitProbe
  override def getBlockSize: Int = LsuL1.LINE_BYTES.get

  // bus refer to the IO memory bus
  def busParam = LsuCachelessBusParam(
    addressWidth = Global.PHYSICAL_WIDTH,
    dataWidth = Riscv.LSLEN,
    hartIdWidth = Global.HART_ID_WIDTH,
    uopIdWidth = Decode.UOP_ID_WIDTH,
    withAmo = false, // AMO are only supported on the cached memory space
    pendingMax = 1
  )

  val tagWidth = 6
  val SB_PTR = Payload(UInt(log2Up(storeBufferOps) + 1 bits))
  // Interface with the store buffer
  case class StoreBufferPush() extends Bundle {
    val slotOh = Bits(storeBufferSlots bits) // Specifies which address slots if being concerned
    val tag = Bits(tagWidth bits)
    val op = StoreBufferOp()
  }
  case class StoreBufferPop() extends Bundle {
    val ptr = SB_PTR()
    val op = StoreBufferOp()
  }

  val logic = during setup new Area{
    assert(!(storeBufferSlots != 0 ^ storeBufferOps != 0))
    val withStoreBuffer = storeBufferSlots != 0

    // * Get reference to the concened plugins, as well as retains from of them from elaborating *
    val elp = host.find[ExecuteLanePlugin](_ == layer.lane)
    val ifp = host.find[IntFormatPlugin](_.lane == layer.lane)
    val srcp = host.find[SrcPlugin](_.layer == layer)
    val ats = host[AddressTranslationService]
    val ps = host[PmpService]
    val ts = host[TrapService]
    val ss = host[ScheduleService]
    val ds = host[DecoderService]
    val pp = host[PrivilegedPlugin]
    val cap = host[CsrAccessPlugin]
    val pcs = host.get[PerformanceCounterService]
    val hp = host.get[PrefetcherPlugin]
    val fpwbp = host.findOption[WriteBackPlugin](p => p.lane == layer.lane && p.rf == FloatRegFile)
    val buildBefore = retains(elp.pipelineLock, ats.portsLock, ps.portsLock)
    val earlyLock = retains(List(ats.storageLock) ++ pcs.map(_.elaborationLock).toList)
    val retainer = retains(List(elp.uopLock, srcp.elaborationLock, ifp.elaborationLock, ts.trapLock, ss.elaborationLock, cap.csrLock, ds.elaborationLock) ++ fpwbp.map(_.elaborationLock))
    awaitBuild()
    Riscv.RVA.set(withRva)

    // * Instanciate a few hardware interfaces *
    val translationStorage = ats.newStorage(translationStorageParameter, PerformanceCounterService.DCACHE_TLB_CYCLES)
    val fpwb = fpwbp.map(_.createPort(wbAt))

    val events = pcs.map(p => new Area {
      val waiting = p.createEventPort(PerformanceCounterService.DCACHE_WAITING)
      waiting := False
    })

    earlyLock.release()

    val trapPort = ts.newTrap(layer.lane.getExecuteAge(ctrlAt), Execute.LANE_AGE_WIDTH)
    val flushPort = ss.newFlushPort(layer.lane.getExecuteAge(ctrlAt), laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
    val frontend = new AguFrontend(layer, host, withRvcbm = withCbm)
    val commitProbe = Flow(LsuCommitProbe()) // Used by the hardware prefetching plugin to learn about the software behaviour

    // Extends the instruction specifications done by the AGU with sign extentions and flush behaviour
    val iwb = ifp.access(wbAt)
    val amos = Riscv.RVA.get.option(frontend.amos.uops).toList.flatten
    for(load <- frontend.writingRf ++ amos){
      val op = layer(load)
      op.mayFlushUpTo(ctrlAt) // page fault / trap / L1 hazard / store queue hazard / ...
      op.dontFlushFrom(ctrlAt+1) //The +1 make the assumption that if a flush happen it is the first cycle in ctrlAt. Also, io access wait one cycle before starting

      Rvi.loadSpec.get(load) match {
        case Some(spec) =>
          ifp.addMicroOp(iwb, op)
          spec.signed match {
            case false => ifp.zeroExtend(iwb, op, spec.width)
            case true => ifp.signExtend(iwb, op, spec.width)
          }
        case None =>
      }
    }

    for(cbm <- frontend.cbms){
      val op = layer(cbm)
      op.mayFlushUpTo(ctrlAt)
      op.dontFlushFrom(ctrlAt+1)
      op.setCompletion(wbAt)
    }

    fpwbp.foreach(_.addMicroOp(fpwb.get, layer, frontend.writeRfFloat))
    for(fp <- frontend.writeRfFloat) {
      val spec = layer(fp)
      spec.setCompletion(wbAt)
    }

    for(store <- frontend.writingMem ++ amos){
      val op = layer(store)
      val isInt = store.resources.exists {
        case RfResource(IntRegFile, RS2) => true
        case _ => false
      }
      op.mayFlushUpTo(ctrlAt)
      op.dontFlushFrom(ctrlAt+1)
      op.addRsSpec(RS2, isInt.mux(storeRs2At, 0)) // Only int late usage works, not the float ones (scheduler reasons)
    }

    val FENCE = Payload(Bool())
    val LSU_PREFETCH = Payload(Bool())

    frontend.uopList.foreach(layer(_).addDecoding(FENCE -> False, LSU_PREFETCH -> False))
    layer.add(Rvi.FENCE).mayFlushUpTo(ctrlAt).setCompletion(ctrlAt).addDecoding(frontend.dec(SEL -> True, FENCE -> True, LSU_PREFETCH -> False))
    elp.setDecodingDefault(FENCE, False)

    for(uop <- frontend.writingMem if layer(uop).completion.isEmpty) layer(uop).setCompletion(ctrlAt)

    val spf = softwarePrefetch generate new Area{
      val pr = layer.add(Rvi.PREFETCH_R)
      val pw = layer.add(Rvi.PREFETCH_W)
      for(op <- List(pr,pw)) {
        op.setCompletion(ctrlAt)
        op.addDecoding(frontend.dec(SEL -> True, LOAD -> True, STORE -> Bool(op == pw), FENCE -> False, LSU_PREFETCH -> True))
        frontend.srcPlugin.specify(op, List(SrcKeys.Op.ADD, SrcKeys.SRC1.RF, SrcKeys.SRC2.S))
      }
    }

    retainer.release()

    val cbmCsr = withCbm generate new Area{
      val invalIntoClean = False
      def xenvcfg(priv : Int) = new Area{
        val at = 0x00A + priv * 0x100
        if(priv == 3) cap.allowCsr(at + 0x10) //Allow menvcfgh
        val privLower = pp.getPrivilege(0) < priv
        val cbie = RegInit(B"00")
        val cbcfe = RegInit(B"0")
        invalIntoClean.setWhen(privLower && cbie === 1)

        cap.readWrite(at, 4 -> cbie, 6 -> cbcfe)
        ds.addIllegalCheck{ ctrlLane =>
          privLower && (ctrlLane(INVALIDATE) && cbie === 0 || ctrlLane(CLEAN) && cbcfe === 0  && privLower)
        }
      }

      ds.addMicroOpDecodingDefault(CLEAN, False)
      ds.addMicroOpDecodingDefault(INVALIDATE, False)
      ds.addMicroOpDecoding(Rvi.CBM_CLEAN, CLEAN, True)
      ds.addMicroOpDecoding(Rvi.CBM_FLUSH, CLEAN, True)
      ds.addMicroOpDecoding(Rvi.CBM_INVALIDATE, INVALIDATE, True)

      val menvcfg = xenvcfg(3)
      val senvcfg = pp.implementSupervisor generate xenvcfg(1)
    }

    val injectCtrl = elp.ctrl(0)
    val inject = new injectCtrl.Area {
      SIZE := Decode.UOP(13 downto 12).asUInt
    }

    val bus = master(LsuCachelessBus(busParam)).simPublic()
    val llcBus = withLlcFlush generate master(FlushBus(FlushParam(Global.PHYSICAL_WIDTH, 0)))

    accessRetainer.await()
    val l1 = LsuL1
    val FROM_ACCESS = Payload(Bool())
    val FROM_LSU = Payload(Bool())
    val FROM_WB = Payload(Bool())
    val FORCE_PHYSICAL = Payload(Bool())
    val FROM_PREFETCH = Payload(Bool())
    val MMU_FAILURE, MMU_PAGE_FAULT = Payload(Bool())

    // Area which can be used to wait until the L1 finish the refills which triggered the wait.
    class L1Waiter extends Area {
      val refill = Reg(l1.WAIT_REFILL)
      val valid = RegInit(False) clearWhen ((refill & ~l1.REFILL_BUSY).orR)

      def capture(node : NodeBaseApi) = {
        import node._
        when(l1.WAIT_REFILL.orR) {
          this.valid := True
          this.refill := l1.WAIT_REFILL
        }
      }
    }

    def p2t(that : UInt) = B(that(tagWidth, log2Up(LsuL1.LINE_BYTES) bits))


    val storeBuffer = withStoreBuffer generate new Area {
      assert(isPow2(storeBufferOps))

      val push = Flow(StoreBufferPush()) // Used when a store do a cache miss, but the cpu commits the instruction
      val pop = Stream(StoreBufferPop()) // To retry a failed store

      val ops = new Area {
        val mem = Mem.fill(storeBufferOps)(StoreBufferOp())
        val pushPtr, popPtr, freePtr = Reg(SB_PTR) init (0)
        val full = (pushPtr ^ freePtr ^ storeBufferOps) === 0 // Assumes storeBufferOps is a power of 2
        val occupancy = pushPtr - freePtr

        // Implement the pipeline which generate store retry commands (the pop interface)
        val pip = new StagePipeline(){
          val inserter = new Area(0){
            valid := popPtr =/= pushPtr
            SB_PTR := popPtr
            val doRead = isFiring
            popPtr := popPtr + U(doRead)
          }
          val read = new Area(1){
            val OPS = insert(mem.readSync(popPtr.resized, inserter.doRead))
          }
          val extracter = new Area(2){
            arbitrateTo(pop)
            pop.op := read.OPS
            pop.ptr := SB_PTR
          }
        }
      }

      val tagRange = tagWidth + log2Up(LsuL1.LINE_BYTES) -1 downto log2Up(LsuL1.LINE_BYTES)
      val TAG = Payload(Bits(tagWidth bits))
      // Store which memory block addresses (cache line big) are currently in the store buffer
      val slots = for(slotId <- 0 until storeBufferSlots) yield new Area {
        val valid = RegInit(False)
        val ptr = Reg(SB_PTR)
        val tag = Reg(TAG)
      }
      val slotsFree = slots.map(!_.valid).orR
      val slotsFreeFirst = B(OHMasking.firstV2(B(slots.map(!_.valid))))

      // Populate the store buffer
      when(push.fire){
        ops.mem.write(ops.pushPtr.resized, push.op)
        ops.pushPtr := ops.pushPtr + U(push.fire)
        slots.onMask(push.slotOh){slot =>
          slot.valid := True
          slot.ptr   := ops.pushPtr
          slot.tag   := push.tag
        }
      }

      // Implement some hardware capable of holding the CPU, this is used when the store buffer is starting to be too full
      // to give priority to the store buffer drain instead of being interrupted by the software usage of the LSU
      val holdHart = new Area{
        val wordPerLine = LsuL1.LINE_BYTES*8/XLEN
        val threshold = Math.max(storeBufferOps - wordPerLine, storeBufferOps/2)
        val waitIt = RegInit(False) clearWhen (slotsFree && ops.occupancy <= threshold)
        host[DispatchPlugin].haltDispatchWhen(waitIt)
        events.foreach(_.waiting setWhen(waitIt))
      }

      val waitL1 = new L1Waiter() // Used when the store buffer is waiting of a cache line refill
      val empty = slots.map(!_.valid).andR
    }

    invalidationRetainer.await()
    // Can flush the whole data cache when no memory coherency is implemented. This is used by the fence.i instruction.
    val flusher = !l1.coherency generate new StateMachine {
      val IDLE = makeInstantEntry()
      val SB_DRAIN = withStoreBuffer generate new State()
      val CMD, COMPLETION = new State()
      val arbiter = StreamArbiterFactory().transactionLock.lowerFirst.buildOn(invalidationPorts.map(_.cmd))
      val cmdCounter = Reg(UInt(log2Up(l1.SETS) + 1 bits))
      val inflight = (addressAt+1 to ctrlAt).map(elp.execute).map(e => e(l1.SEL) && e(l1.FLUSH)).orR
      val waiter = Reg(cloneOf(l1.WRITEBACK_BUSY.get))

      IDLE.whenIsActive{
        cmdCounter := 0
        when(arbiter.io.output.valid) {
          goto(withStoreBuffer.mux(SB_DRAIN, CMD))
        }
      }
      if(withStoreBuffer) SB_DRAIN.whenIsActive {
        when(storeBuffer.empty){
          goto(CMD)
        }
      }
      CMD.whenIsActive{
        when(cmdCounter.msb && !inflight) {
          waiter := l1.WRITEBACK_BUSY
          goto(COMPLETION)
        }
      }
      arbiter.io.output.ready := False
      COMPLETION.whenIsActive{
        waiter := waiter & l1.WRITEBACK_BUSY
        when(!waiter.orR){
          arbiter.io.output.ready := True
          goto(IDLE)
        }
      }
    }

    // When memory coherency is implemented and we execute a fence.i instruction, we still need to ensure that the store buffer
    // is drained before letting the instruction cache fetch stuff.
    val coherentFlusher = l1.coherency.get generate new Area{
      invalidationPorts.foreach(_.cmd.ready := withStoreBuffer.mux(storeBuffer.empty, True))
    }

    // Handle the interface to the RISC-V debug hardware triggers
    val onTrigger = new elp.Execute(triggerAt){
      val bus = host[LsuTriggerService].getLsuTriggerBus
      bus.hartId  := Global.HART_ID
      bus.load    := l1.LOAD
      bus.store   := l1.STORE
      bus.virtual := l1.MIXED_ADDRESS
      bus.size    := l1.SIZE

      val HITS = insert(bus.hits)
      val HIT = insert(bus.hits.orR)
    }

    // Collect the different request and interface them with the L1 cache as well as the MMU
    val onAddress0 = new elp.Execute(addressAt){
      FORCE_PHYSICAL := FROM_ACCESS || FROM_WB
      val translationPort = ats.newTranslationPort(
        nodes = Seq(elp.execute(addressAt).down, elp.execute(addressAt+1).down),
        rawAddress = l1.MIXED_ADDRESS,
        forcePhysical = FORCE_PHYSICAL,
        usage = AddressTranslationPortUsage.LOAD_STORE,
        portSpec = translationPortParameter,
        storageSpec = translationStorage
      )

      val ports = ArrayBuffer[Stream[LsuL1Cmd]]()

      // Accesses coming explicitly from the software
      val ls = new Area {
        val prefetchOp = Decode.UOP(24 downto 20)
        val port = ports.addRet(Stream(LsuL1Cmd()))
        port.valid := isValid && SEL
        port.address := srcp.ADD_SUB.asUInt.resized
        port.size := SIZE
        port.load := LOAD
        port.store := STORE
        port.atomic := ATOMIC
        port.clean := withCbm.mux(CLEAN || INVALIDATE && cbmCsr.invalIntoClean, False)
        port.invalidate := withCbm.mux(INVALIDATE, False)
        port.op := LsuL1CmdOpcode.LSU
        if(softwarePrefetch) when(LSU_PREFETCH) { port.op := LsuL1CmdOpcode.PREFETCH }

        val storeId = Reg(Decode.STORE_ID) init (0)
        storeId := storeId + U(port.fire)
        port.storeId := storeId
      }

      // Accesses comming from the MMU (in practice)
      val access = dbusAccesses.nonEmpty generate new Area {
        assert(dbusAccesses.size == 1)
        val waiter = new L1Waiter
        val sbWaiter = withStoreBuffer.mux(RegInit(False) clearWhen(storeBuffer.empty), False)
        val cmd = dbusAccesses.head.cmd
        val port = ports.addRet(Stream(LsuL1Cmd()))
        port.arbitrationFrom(cmd.haltWhen(waiter.valid || sbWaiter))
        port.address := cmd.address.resized
        port.size := cmd.size
        port.load := True
        port.store := False
        port.atomic := False
        port.clean := False
        port.invalidate := False
        port.op := LsuL1CmdOpcode.ACCESS
        port.storeId := 0
      }

      // Accesses comming from the TrapPlugin (ex : fence.i)
      val flush = (flusher != null) generate new Area {
        val port = ports.addRet(Stream(LsuL1Cmd()))
        port.valid := flusher.isActive(flusher.CMD) && !flusher.cmdCounter.msb
        port.address := (flusher.cmdCounter << log2Up(l1.LINE_BYTES)).resized
        port.size := 0
        port.load := False
        port.store := False
        port.atomic := False
        port.clean := False
        port.invalidate := False
        port.op := LsuL1CmdOpcode.FLUSH
        port.storeId := 0
        when(port.fire) {
          flusher.cmdCounter := flusher.cmdCounter + 1
        }
      }

      // Accesses comming from the store buffer which attempts to retry a failed store
      val sb = withStoreBuffer generate new Area {
        val isHead = storeBuffer.pop.ptr === storeBuffer.ops.freePtr
        val flush = storeBuffer.waitL1.valid && !isHead
        val port = ports.addRet(Stream(LsuL1Cmd()))
        port.valid := storeBuffer.pop.valid && !storeBuffer.waitL1.valid && !flush
        port.address := storeBuffer.pop.op.address.resized
        port.size := storeBuffer.pop.op.size
        port.load := False
        port.store := True
        port.atomic := False
        port.clean := False
        port.invalidate := False
        port.op := LsuL1CmdOpcode.STORE_BUFFER
        storeBuffer.pop.ready := port.ready || flush
        port.storeId := storeBuffer.pop.op.storeId
      }

      // Accesses comming from the hardware prefetcher
      val fromHp = hp.nonEmpty generate new Area {
        val feed = hp.get.io.get
        val port = ports.addRet(Stream(LsuL1Cmd()))
        port.arbitrationFrom(feed)
        port.op := LsuL1CmdOpcode.PREFETCH
        port.address := feed.address
        port.store := feed.unique
        port.size := 0
        port.load := False
        port.atomic := False
        port.clean := False
        port.invalidate := False
        port.storeId := 0
      }

      // Let's arbitrate all those request and connect the atrbitred output to the pipeline / L1
      val arbiter = StreamArbiterFactory().noLock.lowerFirst.buildOn(ports)
      arbiter.io.output.ready := !elp.isFreezed()
      l1.SEL := arbiter.io.output.valid
      if(withLlcFlush) l1.SEL clearWhen(arbiter.io.output.clean || arbiter.io.output.invalidate) //Avoid enabling the L1 cache on flush
      l1.MIXED_ADDRESS := arbiter.io.output.address
      l1.MASK := AddressToMask(arbiter.io.output.address, arbiter.io.output.size, Riscv.LSLEN / 8)
      l1.SIZE := arbiter.io.output.size
      l1.LOAD := arbiter.io.output.load
      l1.ATOMIC := arbiter.io.output.atomic
      l1.STORE := arbiter.io.output.store
      l1.CLEAN := arbiter.io.output.clean
      l1.INVALID := arbiter.io.output.invalidate
      l1.PREFETCH := arbiter.io.output.op === LsuL1CmdOpcode.PREFETCH
      l1.FLUSH := arbiter.io.output.op === LsuL1CmdOpcode.FLUSH
      Decode.STORE_ID := arbiter.io.output.storeId
      FROM_ACCESS := arbiter.io.output.op === LsuL1CmdOpcode.ACCESS
      FROM_WB := arbiter.io.output.op === LsuL1CmdOpcode.STORE_BUFFER
      FROM_LSU := arbiter.io.output.op === LsuL1CmdOpcode.LSU
      FROM_PREFETCH := arbiter.io.output.op === LsuL1CmdOpcode.PREFETCH
      if(withStoreBuffer) SB_PTR := storeBuffer.pop.ptr
      val SB_DATA = withStoreBuffer generate insert(storeBuffer.pop.op.data)
      val STORE_BUFFER_EMPTY = withStoreBuffer generate insert(storeBuffer.empty)
    }

    val tpk = onAddress0.translationPort.keys

    val pmpPort = ps.createPmpPort(
      nodes = List.tabulate(ctrlAt+1)(elp.execute(_).down),
      physicalAddress = tpk.TRANSLATED,
      forceCheck = _(FROM_ACCESS),
      read = _(l1.LOAD),
      write = _(l1.STORE),
      execute = _ => False,
      portSpec = pmpPortParameter,
      storageSpec = null
    )

    val onAddress1 = new elp.Execute(addressAt+1) {
      l1.PHYSICAL_ADDRESS := tpk.TRANSLATED
    }

    for(eid <- addressAt + 1 to ctrlAt) {
      val e = elp.execute(eid)
      e.up(l1.SEL).setAsReg().init(False)
      when(e(FROM_LSU) && !e.isValid) {
        e.bypass(l1.SEL) := False
      }
    }

    // Pre compute a few things to reduce the combinatorial path presure on the ctrl stage.
    val preCtrl = new elp.Execute(ctrlAt-1){
      val MISS_ALIGNED = insert((1 to log2Up(LSLEN / 8)).map(i => l1.SIZE === i && l1.MIXED_ADDRESS(i - 1 downto 0) =/= 0).orR)
      if(withCbm) MISS_ALIGNED clearWhen(l1.CLEAN || l1.INVALID)
      val IS_AMO = insert(SEL && l1.ATOMIC && l1.STORE && l1.LOAD)
    }

    val onPma = new elp.Execute(pmaAt){
      val cached = new PmaPort(Global.PHYSICAL_WIDTH, List(l1.LINE_BYTES), List(PmaLoad, PmaStore))
      val io = new PmaPort(Global.PHYSICAL_WIDTH, (0 to log2Up(Riscv.LSLEN / 8)).map(1 << _), List(PmaLoad, PmaStore))
      cached.cmd.address := tpk.TRANSLATED
      cached.cmd.op(0) := l1.STORE
      io.cmd.address := tpk.TRANSLATED
      io.cmd.size := l1.SIZE.asBits
      io.cmd.op(0) := l1.STORE

      val CACHED_RSP = insert(cached.rsp)
      val IO_RSP = insert(io.rsp)
      IO_RSP.fault.setWhen(l1.ATOMIC || FROM_ACCESS)
      val IO = insert(CACHED_RSP.fault && !IO_RSP.fault && !FENCE && !FROM_PREFETCH)
      val addressExtension = ats.getSignExtension(AddressTranslationPortUsage.LOAD_STORE, srcp.ADD_SUB.asUInt)
      val FROM_LSU_MSB_FAILED = insert(FROM_LSU && srcp.ADD_SUB.dropLow(Global.MIXED_WIDTH).asBools.map(_ =/= addressExtension).orR)
      MMU_PAGE_FAULT := tpk.PAGE_FAULT || STORE.mux(!tpk.ALLOW_WRITE, !tpk.ALLOW_READ)
      MMU_FAILURE := MMU_PAGE_FAULT || tpk.ACCESS_FAULT || tpk.REFILL || tpk.HAZARD || FROM_LSU_MSB_FAILED
    }

    // The ctrl stage will take all the decisions, handle AMO/LR/SC and IO accesses as well
    // A loooot of things are happening here.
    val onCtrl = new elp.Execute(ctrlAt) {
      val lsuTrap = False

      // Data which should be written to the memory, before being aligned to the memory address
      val writeData = Bits(Riscv.LSLEN bits)
      writeData.assignDontCare()
      writeData(0, XLEN bits) := up(elp(IntRegFile, riscv.RS2))
      if(Riscv.withFpu) when(FLOAT){
        writeData(0, FLEN bits) := up(elp(FloatRegFile, riscv.RS2))
      }

      val scMiss = Bool() // (Store Conditional miss)

      // Little state machine which handle IO accesses. Pipelined on all sides to avoid putting pressure on the FMax
      val io = new Area {
        // Give one cycle delay, allowing trap to happen before the IO access is emitted.
        val tooEarly = RegNext(True) clearWhen(elp.isFreezed()) init(False)
         
        val allowIt = RegNext(False) setWhen(!lsuTrap && !isCancel && FROM_LSU && !l1.CLEAN && !l1.INVALID) init(False)
        val doIt = isValid && l1.SEL && onPma.IO
        val doItReg = RegNext(doIt) init(False)

        val cmdSent = RegInit(False) setWhen (bus.cmd.fire) clearWhen (!elp.isFreezed())
        bus.cmd.valid := doItReg && !cmdSent && allowIt && !tooEarly
        bus.cmd.write := l1.STORE
        bus.cmd.address := l1.PHYSICAL_ADDRESS
        bus.cmd.data := l1.WRITE_DATA
        bus.cmd.size := l1.SIZE
        bus.cmd.mask := l1.MASK
        bus.cmd.io := True
        bus.cmd.fromHart := True
        bus.cmd.hartId := Global.HART_ID
        bus.cmd.uopId := Decode.UOP_ID

        val rsp = bus.rsp.toStream.halfPipe()
        rsp.ready := !elp.isFreezed()

        val freezeIt = doIt && (tooEarly || !rsp.valid && allowIt)
        elp.freezeWhen(freezeIt)

        l1.ackUnlock setWhen (cmdSent) //Ensure we don't create external lockup
      }

      // Extract the data from the memory read, into the wished register file format.
      val loadData = new Area {
        val input = io.cmdSent.mux[Bits](io.rsp.data, l1.READ_DATA)
        val splitted = input.subdivideIn(8 bits)
        val shifted = Bits(LSLEN bits)
        val wordBytes = LSLEN / 8

        // Generate minimal mux to move from a wide aligned memory read to the register file shifter representation
        for (i <- 0 until wordBytes) {
          val srcSize = 1 << (log2Up(wordBytes) - log2Up(i + 1))
          val srcZipped = splitted.zipWithIndex.filter { case (v, b) => b % (wordBytes / srcSize) == i }
          val src = srcZipped.map(_._1)
          val range = log2Up(wordBytes) - 1 downto log2Up(wordBytes) - log2Up(srcSize)
          val sel = l1.MIXED_ADDRESS(range)
          shifted(i * 8, 8 bits) := src.read(sel)
        }
        val RESULT = insert(shifted)
      }

      // Translate the register file data into the memory friendly format.
      // So for instance, let's say the want to write 0xABCD on a given 16 bits location on a 64 bits memory bus,
      // then hardware will generate 0xABCD_ABCD_ABCD_ABCD, allowing the write to happen with every aligned byte mask possible.
      val storeData = new Area {
        val mapping = for(size <- 0 to log2Up(Riscv.LSLEN / 8)) yield {
          val w = (1 << size) * 8
          size -> writeData(0, w bits).#*(Riscv.LSLEN / w)
        }
        l1.WRITE_DATA := l1.SIZE.muxListDc(mapping)
      }

      val SC_MISS = insert(scMiss) //insert(withRva.mux(io.doIt.mux[Bool](io.rsp.scMiss, scMiss), False))

      if (!Riscv.RVA) {
        scMiss := False
        l1.lockPort.valid := False
        l1.lockPort.address := 0
      }

      // Implements a little state machine,the AMO ALU and the reservation logic (LR/SC)
      val rva = Riscv.RVA.get generate new Area {
        val srcBuffer = RegNext[Bits](loadData.RESULT.resize(XLEN bits))
        val alu = new AtomicAlu(
          op = UOP(29, 3 bits),
          swap = UOP(27),
          mem = srcBuffer,
          rf = up(elp(IntRegFile, riscv.RS2)),
          isWord = l1.SIZE === 2
        )
        val aluBuffer = RegNext(alu.result)

        when(preCtrl.IS_AMO) {
          writeData(aluBuffer.bitsRange) := aluBuffer
        }

        val delay = History(!elp.isFreezed(), 1 to 2)
        val freezeIt = isValid && preCtrl.IS_AMO && delay.orR
        elp.freezeWhen(freezeIt) //Note that if the refill is faster than 2 cycle, it may create issues

        assert(Global.HART_COUNT.get == 1)
        val lrsc = new Area {
          val capture = False // True when the software ask a reservation (load reserve)
          val reserved = RegInit(False)
          val address = Reg(l1.PHYSICAL_ADDRESS)

          when(!elp.isFreezed() && isValid && FROM_LSU && l1.SEL && !lsuTrap && !onPma.IO) {
            when(l1.STORE){
              reserved := False // Kill the reservation on any store
            } elsewhen(apply(l1.ATOMIC)) {
              capture := True // Load Reserve
            }
          }
          scMiss := !reserved
          l1.lockPort.valid := reserved
          l1.lockPort.address := address

          val age = Reg(UInt(6 bits)) //Will make the reservation die after 2**(bits-1) cycles
          when(!age.msb && !elp.isFreezed()){
            // It only age when the pipeline isn't freezed for 2 reasons :
            // - Else we may lose the reservation at the same time we are processing a store condition => transient cases
            // - If a LR is followed by a divide instruction (for example), this will slow down things and ensure we have a chance to pass
            age := age + 1
          }

          when(age.msb || io.cmdSent){ // io.cmdSent ensure we do not create external deadlock
            reserved := False
            age := 0
          }
          when(capture && (reserved || age >= 8)){ //age >= 8 Ensure we don't prevent forward progression of other agents
            reserved  := !reserved //Toggling the reservation is a way to ensure reservation changes are notified by the LsuL1 (not great)
            address :=  l1.PHYSICAL_ADDRESS
            age := 0
          }
        }
      }

      // Compute a few flags / condition for the store buffer handling
      val wb = withStoreBuffer generate new Area {
        when(FROM_WB) {
          l1.WRITE_DATA := onAddress0.SB_DATA
        }
        val tag = p2t(LsuL1.PHYSICAL_ADDRESS)
        val hits = B(storeBuffer.slots.map(s => s.valid && s.tag === tag)) // Check if the current instruction collide with the store queue slots.
        val hit = hits.orR
        val compatibleOp = FROM_LSU && l1.STORE && !l1.ATOMIC && !onPma.IO // The current instruction may eventually be pushed to the store queue
        val notFull = !storeBuffer.ops.full && (storeBuffer.slotsFree || hit)
        val allowed = notFull && compatibleOp // The current store can be pushed to the store queue
        val slotOh = hits | storeBuffer.slotsFreeFirst.andMask(!hit) // Find which store buffer slot to assign to the current store instruction
        val loadHazard = l1.LOAD && hit // Store buffer to load hazard detected. Vexii doesn't implement store buffer to load bypass.
        val selfHazard = FROM_WB && SB_PTR =/= storeBuffer.ops.freePtr // The store retry isn't the one at the head of the store buffer.
      }

      // Handle all the different trap cases
      val traps = new Area {
        flushPort.valid := False
        flushPort.hartId := Global.HART_ID
        flushPort.uopId := Decode.UOP_ID
        flushPort.laneAge := Execute.LANE_AGE
        flushPort.self := False

        trapPort.valid := False
        trapPort.hartId := Global.HART_ID
        trapPort.laneAge := Execute.LANE_AGE
        trapPort.tval := l1.MIXED_ADDRESS.asBits.resized //PC RESIZED
        trapPort.exception.assignDontCare()
        trapPort.code.assignDontCare()
        trapPort.arg.allowOverride() := 0

        val accessFault = (onPma.CACHED_RSP.fault).mux[Bool](io.rsp.valid && io.rsp.error, l1.FAULT) || pmpPort.ACCESS_FAULT
        when(accessFault) {
          lsuTrap := True
          trapPort.exception := True
          trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
          trapPort.code(1) setWhen (l1.STORE)
        }

        val l1Failed = l1.SEL && (!onPma.CACHED_RSP.fault && (l1.HAZARD || (l1.MISS || l1.MISS_UNIQUE) && (l1.LOAD || l1.STORE)))
        when(withStoreBuffer.mux((l1Failed || wb.hit) && !wb.allowed, l1Failed)) {
          lsuTrap := True
          trapPort.exception := False
          trapPort.code := TrapReason.REDO
        }

        if(withL1Cmb) when(l1.CBM_REDO) {
          lsuTrap := True
          trapPort.exception := False
          trapPort.code := TrapReason.REDO
        }

        val pmaFault = onPma.CACHED_RSP.fault && onPma.IO_RSP.fault
        when(pmaFault) {
          lsuTrap := True
          trapPort.exception := True
          trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
          trapPort.code(1) setWhen (l1.STORE)
        }

        when(MMU_PAGE_FAULT) {
          lsuTrap := True
          trapPort.exception := True
          trapPort.code := CSR.MCAUSE_ENUM.LOAD_PAGE_FAULT
          trapPort.code(1) setWhen (l1.STORE)
        }

        when(tpk.ACCESS_FAULT) {
          lsuTrap := True
          trapPort.exception := True
          trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
          trapPort.code(1) setWhen (l1.STORE)
        }

        trapPort.arg(0, 2 bits) := l1.STORE.mux(B(TrapArg.STORE, 2 bits), B(TrapArg.LOAD, 2 bits))
        trapPort.arg(2, ats.getStorageIdWidth() bits) := ats.getStorageId(translationStorage)
        when(tpk.REFILL) { // Could be ignored for llc flush
          lsuTrap := True
          trapPort.exception := False
          trapPort.code := TrapReason.MMU_REFILL
        }
        when(tpk.HAZARD) {
          lsuTrap := True
          trapPort.exception := False
          trapPort.code := TrapReason.REDO
        }
        when(onPma.FROM_LSU_MSB_FAILED) {
          lsuTrap := True
          trapPort.exception := True
          trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
          trapPort.code(1) setWhen (STORE)
          trapPort.code(3) setWhen (!tpk.BYPASS_TRANSLATION)
        }
        when(preCtrl.MISS_ALIGNED) {
          lsuTrap := True
          trapPort.exception := True
          trapPort.code := l1.STORE.mux[Bits](CSR.MCAUSE_ENUM.STORE_MISALIGNED, CSR.MCAUSE_ENUM.LOAD_MISALIGNED).andMask(preCtrl.MISS_ALIGNED).resized
        }

        val triggerId = B(OHToUInt(onTrigger.HITS))
        when(onTrigger.HIT) {
          lsuTrap := True
          trapPort.exception := False
          trapPort.code := TrapReason.DEBUG_TRIGGER
          trapPort.tval(triggerId.bitsRange) := B(OHToUInt(onTrigger.HITS))
        }
      }

      if(withStoreBuffer) {
        storeBuffer.push.valid := False
        storeBuffer.push.slotOh := wb.slotOh
        storeBuffer.push.tag := wb.tag
        storeBuffer.push.op.address := l1.PHYSICAL_ADDRESS
        storeBuffer.push.op.data := LsuL1.WRITE_DATA
        storeBuffer.push.op.size := LsuL1.SIZE
        storeBuffer.push.op.storeId := Decode.STORE_ID
      }

      when(FENCE || FROM_PREFETCH){
        lsuTrap := False
      }

      // memory fences while the store buffer isn't drained are handled by retrying the fence later.
      val fenceTrap = new Area{
        val enable = False
        val doIt = (l1.ATOMIC || FENCE) && enable
        val doItReg = RegInit(False) setWhen(doIt) clearWhen(!elp.isFreezed())

        when(doIt || doItReg) {
          lsuTrap := True
          trapPort.exception := False
          trapPort.code := TrapReason.REDO
        }

        if(withStoreBuffer) enable.setWhen((!storeBuffer.empty || !onAddress0.STORE_BUFFER_EMPTY))
      }

      val onLlcFlush = withLlcFlush generate new Area{
        val llcFlushBuffer = cloneOf(llcBus.cmd)
        llcBus.cmd <-/< llcFlushBuffer

        val flushPendingMax = 16
        assert(isPow2(flushPendingMax))

        val flushTokens = Reg(UInt(log2Up(flushPendingMax + 1) bits)) init(flushPendingMax)
        val flushFull = flushTokens === 0
        val flushEmpty = flushTokens.msb
        val flushEmptyBuffer = RegNextWhen(flushTokens.msb && !llcFlushBuffer.fire, isReady) init(False)
        flushTokens := flushTokens - U(llcFlushBuffer.fire) + U(llcBus.rsp.fire)

        llcFlushBuffer.valid := isValid && SEL && !lsuTrap && !isCancel && (l1.CLEAN || l1.INVALID)
        llcFlushBuffer.address := l1.PHYSICAL_ADDRESS

        val redo = (!llcFlushBuffer.ready || flushFull) && (l1.CLEAN || l1.INVALID)
        when(redo) {
          lsuTrap := True
          trapPort.exception := False
          trapPort.code := TrapReason.REDO
        }

        llcBus.rsp.ready := True

        when(l1.ATOMIC || FENCE || l1.LOAD){
          fenceTrap.doIt.setWhen(!flushEmptyBuffer)
        }
      }

      val cmbTrap = withL1Cmb generate new Area{
        val cmbTrigger = RegNextWhen(isValid && SEL && (l1.CLEAN || l1.INVALID), !elp.isFreezed()) init(False)
        val pendingWritebacks = Reg(LsuL1.WRITEBACK_BUSY.get) init(0)
        pendingWritebacks := (pendingWritebacks & LsuL1.WRITEBACK_BUSY.orMask(elp.isFreezed())) | LsuL1.WRITEBACK_BUSY.andMask(cmbTrigger)
        val pending = cmbTrigger || pendingWritebacks.orR

        val valid = (l1.ATOMIC || FENCE || l1.LOAD) && pending
        when(valid) {
          fenceTrap.doIt := True
        }
      }

      when(isValid && SEL) {
        val t = when(lsuTrap) {
          trapPort.valid := True
          flushPort.valid := True
          bypass(Global.TRAP) := True
          bypass(Global.COMMIT) := False
        }
        if(withStoreBuffer) {
          t.elsewhen((traps.l1Failed || wb.hit) && wb.allowed && down.isFiring){
            storeBuffer.push.valid   := True
          }
          when(wb.compatibleOp && !wb.notFull) {
            storeBuffer.holdHart.waitIt := True
          }
        }
      }

      val mmuNeeded = FROM_LSU || FROM_PREFETCH

      // Prevents side effects on the L1 in various cases.
      val abords, skipsWrite = ArrayBuffer[Bool]()
      abords += l1.HAZARD
      abords += l1.FLUSH_HAZARD
      abords += !l1.FLUSH && onPma.CACHED_RSP.fault
      abords += FROM_LSU && (!isValid || isCancel || FENCE)
      abords += mmuNeeded && MMU_FAILURE
      abords += FROM_LSU && (fenceTrap.doIt || fenceTrap.doItReg)
      if(withStoreBuffer && withL1Cmb) abords += (l1.CLEAN || l1.INVALID) && wb.hit
      if(withStoreBuffer) abords += wb.loadHazard || wb.selfHazard

      skipsWrite += l1.MISS || l1.MISS_UNIQUE
      skipsWrite += l1.FAULT
      skipsWrite += preCtrl.MISS_ALIGNED
      skipsWrite += FROM_LSU && (onTrigger.HIT || pmpPort.ACCESS_FAULT)
      skipsWrite += FROM_PREFETCH
      if(Riscv.RVA) skipsWrite += l1.ATOMIC && !l1.LOAD && scMiss
      if (withStoreBuffer) skipsWrite += wb.selfHazard || !FROM_WB && wb.hit

      l1.ABORD := abords.orR
      l1.SKIP_WRITE := skipsWrite.orR

      if(flusher != null) when(l1.SEL && l1.FLUSH && (l1.FLUSH_HIT || l1.HAZARD || l1.FLUSH_HAZARD)){
        flusher.cmdCounter := l1.MIXED_ADDRESS(log2Up(l1.LINE_BYTES), log2Up(l1.SETS) bits).resized
      }

      // Handle store buffer retry
      if(withStoreBuffer) when(l1.SEL && FROM_WB && !elp.isFreezed() && !wb.selfHazard){
        when(traps.l1Failed) { // Seems like the L1 cache had a miss
          storeBuffer.ops.popPtr := storeBuffer.ops.freePtr
          when(!l1.HAZARD) {
            storeBuffer.waitL1.capture(down) // Seems like we need to wait on the L1 refill
          }
        } otherwise { // All went well :D
          storeBuffer.ops.freePtr := storeBuffer.ops.freePtr + 1
          for (slot <- storeBuffer.slots) when(slot.ptr === storeBuffer.ops.freePtr) {
            slot.valid := False
          }
        }
      }

      // Feed L1 load data to the dBusAccess (to the MMU page walker)
      val access = dbusAccesses.nonEmpty generate new Area {
        assert(dbusAccesses.size == 1)
        val rsp = dbusAccesses.head.rsp
        rsp.valid    := l1.SEL && FROM_ACCESS && !elp.isFreezed()
        rsp.data     := loadData.RESULT.resized // loadData.RESULT instead of l1.READ_DATA (because it rv32fd)
        rsp.error    := l1.FAULT || pmpPort.ACCESS_FAULT
        rsp.redo     := traps.l1Failed
        rsp.waitSlot := 0
        rsp.waitAny  := False //TODO
        if(withStoreBuffer) when(wb.hit){
          rsp.redo := True
          onAddress0.access.sbWaiter setWhen(rsp.valid)
        }
        when(traps.pmaFault){
          rsp.error := True
          rsp.redo := False
        }
        when(rsp.fire && rsp.redo) {
          onAddress0.access.waiter.capture(down)
        }
      }

      val hartRegulation = new L1Waiter{
        host[DispatchPlugin].haltDispatchWhen(valid)
        when(isValid && SEL && !FROM_PREFETCH && !onPma.IO && !FENCE && withStoreBuffer.mux(l1.LOAD, True) && (l1.HAZARD || l1.MISS || l1.MISS_UNIQUE)){
          capture(down)
        }
        events.foreach(_.waiting setWhen(valid))
      }

      // Drive the commitProbe bus
      val commitProbeReq = down.isFiring && SEL && FROM_LSU
      val commitProbeToken = RegNextWhen(lsuTrap, commitProbeReq) init(False) // Avoid to spam on consicutive failure
      commitProbe.valid := down.isFiring && SEL.mux[Bool](FROM_LSU && (!lsuTrap || !commitProbeToken) && (l1.LOAD || l1.STORE || l1.PREFETCH), FROM_PREFETCH && HAZARD) // && !l1.REFILL_HIT
      commitProbe.address := l1.MIXED_ADDRESS
      commitProbe.load := l1.LOAD
      commitProbe.store := l1.STORE
      commitProbe.trap := lsuTrap
      commitProbe.miss := l1.MISS && !l1.HAZARD && !MMU_FAILURE
      commitProbe.io := onPma.IO
      commitProbe.prefetchFailed := FROM_PREFETCH
      commitProbe.pc := Global.PC
    }

    // Drive the write back interface
    val onWb = new elp.Execute(wbAt){
      iwb.valid := SEL && !FLOAT
      iwb.payload := onCtrl.loadData.RESULT.resized

      if (withRva) when(l1.ATOMIC && !l1.LOAD) {
        iwb.payload(0) := onCtrl.SC_MISS
        iwb.payload(7 downto 1) := 0  // other bits set to 0 by using `LoadSpec(8, ...)` for the instruction
      }

      fpwb.foreach{p =>
        p.valid := SEL && FLOAT
        p.payload := onCtrl.loadData.RESULT.resized
        if(Riscv.RVD) when(SIZE === 2) {
          p.payload(63 downto 32).setAll()
        }
      }

      val storeFire      = down.isFiring && AguPlugin.SEL && l1.STORE && !onPma.IO && !FROM_PREFETCH
      val storeBroadcast = down.isReady && l1.SEL && l1.STORE && !l1.ABORD && !l1.SKIP_WRITE && !l1.MISS && !l1.MISS_UNIQUE && !l1.HAZARD
    }

    if(withStoreBuffer) storeBuffer.ops.pip.build()
    buildBefore.release()
  }


  // Generate the PMA (Physical Memory access) checkers
  // - One for the accesses which can go to the L1
  // - One for the access which can go to the IO bus
  // If an access could go to both, then L1 win.
  val ioRegions = Handle[ArrayBuffer[PmaRegion]]()
  val pmaBuilder = during build new Area{
    val l1Regions = ArrayBuffer[PmaRegion]()
    for(r <- host[LsuL1Service].regions if r.isMain){
      r.transfers match {
        case t: M2sTransfers if t.get.contains(LsuL1.LINE_BYTES) && (t.putFull.contains(LsuL1.LINE_BYTES) || t.putFull.none) =>
          l1Regions += r
        case t: M2sTransfers if t.withBCE =>
          l1Regions += r
        case t: M2sTransfers => {
          println(t)
          ???
        }
      }
    }
    val l1 = new PmaLogic(logic.onPma.cached, l1Regions)
    val io = new PmaLogic(logic.onPma.io, ioRegions)
  }
}
