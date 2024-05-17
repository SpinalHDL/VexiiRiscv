package vexiiriscv.execute.lsu

import spinal.core._
import spinal.core.fiber.Handle
import spinal.core.sim.SimDataPimper
import spinal.lib
import spinal.lib._
import spinal.lib.bus.tilelink.M2sTransfers
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.system.tag.PmaRegion
import vexiiriscv.decode.Decode
import vexiiriscv.decode.Decode.UOP
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService, DBusAccessService, PmaLoad, PmaLogic, PmaPort, PmaStore}
import vexiiriscv.misc.{AddressToMask, LsuTriggerService, PerformanceCounterService, TrapArg, TrapReason, TrapService}
import vexiiriscv.riscv.Riscv.{LSLEN, XLEN}
import vexiiriscv.riscv._
import vexiiriscv.schedule.{DispatchPlugin, ScheduleService}
import vexiiriscv.{Global, riscv}
import vexiiriscv.execute._
import vexiiriscv.execute.lsu.AguPlugin._
import vexiiriscv.fetch.{LsuL1Service, LsuService}

import scala.collection.mutable.ArrayBuffer

case class LsuL1Cmd() extends Bundle {
  val address = LsuL1.MIXED_ADDRESS()
  val size = SIZE()
  val load, store, atomic = Bool()
  val fromFlush = Bool()
  val fromAccess = Bool()
  val fromStoreBuffer = Bool()
  val storeId = Decode.STORE_ID()
}

case class StoreBufferOp() extends Bundle {
  val address = Global.PHYSICAL_ADDRESS()
  val data = LsuL1.WRITE_DATA()
  val size = LsuL1.SIZE()
  val storeId = Decode.STORE_ID()
}

class LsuPlugin(var layer : LaneLayer,
                var withRva : Boolean,
                var translationStorageParameter: Any,
                var translationPortParameter: Any,
                var addressAt: Int = 0,
                var triggerAt : Int = 1,
                var ctrlAt: Int = 2,
                var wbAt : Int = 2,
                var storeRs2At : Int = 0,
                var storeBufferSlots : Int = 0,
                var storeBufferOps : Int = 0) extends FiberPlugin with DBusAccessService with LsuCachelessBusProvider with LsuService{

  override def accessRefillCount: Int = 0
  override def accessWake: Bits = B(0)

  override def getLsuCachelessBus(): LsuCachelessBus = logic.bus

  def busParam = LsuCachelessBusParam(
    addressWidth = Global.PHYSICAL_WIDTH,
    dataWidth = Riscv.LSLEN,
    hartIdWidth = Global.HART_ID_WIDTH,
    uopIdWidth = Decode.UOP_ID_WIDTH,
    withAmo = false, //TODO
    pendingMax = 1
  )

  val tagWidth = 6
  val SB_PTR = Payload(UInt(log2Up(storeBufferOps) + 1 bits))
  case class StoreBufferPush() extends Bundle {
    val slotOh = Bits(storeBufferSlots bits)
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
    val elp = host.find[ExecuteLanePlugin](_ == layer.el)
    val ifp = host.find[IntFormatPlugin](_.lane == layer.el)
    val srcp = host.find[SrcPlugin](_.layer == layer)
    val ats = host[AddressTranslationService]
    val ts = host[TrapService]
    val ss = host[ScheduleService]
    val pcs = host.get[PerformanceCounterService]
    val fpwbp = host.findOption[WriteBackPlugin](p => p.lane == layer.el && p.rf == FloatRegFile)
    val buildBefore = retains(elp.pipelineLock, ats.portsLock)
    val earlyLock = retains(List(ats.storageLock) ++ pcs.map(_.elaborationLock).toList)
    val retainer = retains(List(elp.uopLock, srcp.elaborationLock, ifp.elaborationLock, ts.trapLock, ss.elaborationLock) ++ fpwbp.map(_.elaborationLock))
    awaitBuild()
    Riscv.RVA.set(withRva)

    val translationStorage = ats.newStorage(translationStorageParameter)
    val fpwb = fpwbp.map(_.createPort(wbAt))

    val events = pcs.map(p => new Area {
      val waiting = p.createEventPort(PerformanceCounterService.DCACHE_WAITING)
      waiting := False
    })

    earlyLock.release()

    val trapPort = ts.newTrap(layer.el.getExecuteAge(ctrlAt), Execute.LANE_AGE_WIDTH)
    val flushPort = ss.newFlushPort(layer.el.getExecuteAge(ctrlAt), laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
    val frontend = new AguFrontend(layer, host)

    // IntFormatPlugin specification
    val iwb = ifp.access(wbAt)
    val amos = Riscv.RVA.get.option(frontend.amos.uops).toList.flatten
    for(load <- frontend.writingRf ++ amos){
      val op = layer(load)
      op.mayFlushUpTo(ctrlAt) // page fault / trap
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

    fpwbp.foreach(_.addMicroOp(fpwb.get, layer, frontend.writeRfFloat))
    for(fp <- frontend.writeRfFloat) {
      val spec = layer(fp)
      spec.setCompletion(wbAt)
    }

    for(store <- frontend.writingMem ++ amos){
      val op = layer(store)
      op.mayFlushUpTo(ctrlAt)
      op.dontFlushFrom(ctrlAt+1)
      op.addRsSpec(RS2, storeRs2At)
    }

    val FENCE = Payload(Bool())
    frontend.uopList.foreach(layer(_).addDecoding(FENCE -> False))
    layer.add(Rvi.FENCE).setCompletion(ctrlAt).addDecoding(SEL -> True, LOAD -> False, STORE -> False, ATOMIC -> False, FLOAT -> False, FENCE -> True)
    elp.setDecodingDefault(FENCE, False)


    for(uop <- frontend.writingMem if layer(uop).completion.isEmpty) layer(uop).setCompletion(ctrlAt)

    retainer.release()

    val injectCtrl = elp.ctrl(0)
    val inject = new injectCtrl.Area {
      SIZE := Decode.UOP(13 downto 12).asUInt
    }

    val bus = master(LsuCachelessBus(busParam)).simPublic()

    accessRetainer.await()
    val l1 = LsuL1
    val FROM_ACCESS = Payload(Bool())
    val FROM_LSU = Payload(Bool())
    val FROM_WB = Payload(Bool())
    val FORCE_PHYSICAL = Payload(Bool())

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

      val push = Flow(StoreBufferPush())
      val pop = Stream(StoreBufferPop())

      val ops = new Area {
        val mem = Mem.fill(storeBufferOps)(StoreBufferOp())
        val pushPtr, popPtr, freePtr = Reg(SB_PTR) init (0)
        val full = (pushPtr ^ freePtr ^ storeBufferOps) === 0
        val occupancy = pushPtr - freePtr

        val stages = 2
        val ctrls = List.fill(stages)(CtrlLink())
        val regs = for((from, to) <- (ctrls, ctrls.tail).zipped) yield StageLink(from.down, to.up)

        ctrls(0).up.valid := popPtr =/= pushPtr
        ctrls(0).up(SB_PTR) := popPtr
        val doRead = ctrls(0).up.isFiring
        popPtr := popPtr + U(doRead)
        val OPS = ctrls(1).insert(mem.readSync(popPtr.resized, doRead))
        ctrls.last.down.driveTo(pop){(p, n) =>
          p.op := n(OPS)
          p.ptr := n(SB_PTR)
        }

        Builder(ctrls ++ regs)
      }

      val tagRange = tagWidth + log2Up(LsuL1.LINE_BYTES) -1 downto log2Up(LsuL1.LINE_BYTES)
      val TAG = Payload(Bits(tagWidth bits))
      val slots = for(slotId <- 0 until storeBufferSlots) yield new Area {
        val valid = RegInit(False)
        val ptr = Reg(SB_PTR)
        val tag = Reg(TAG)
      }
      val slotsFree = slots.map(!_.valid).orR
      val slotsFreeFirst = B(OHMasking.firstV2(B(slots.map(!_.valid))))

      when(push.fire){
        ops.mem.write(ops.pushPtr.resized, push.op)
        ops.pushPtr := ops.pushPtr + U(push.fire)
        slots.onMask(push.slotOh){slot =>
          slot.valid := True
          slot.ptr   := ops.pushPtr
          slot.tag   := push.tag
        }
      }

      val holdHart = new Area{
        val wordPerLine = LsuL1.LINE_BYTES*8/XLEN
        val threshold = Math.max(storeBufferOps - wordPerLine, storeBufferOps/2)
        val waitIt = RegInit(False) clearWhen (slotsFree && ops.occupancy <= threshold)
        host[DispatchPlugin].haltDispatchWhen(waitIt)
        events.foreach(_.waiting setWhen(waitIt))
      }

      val waitL1 = new L1Waiter()
      val empty = slots.map(!_.valid).andR
    }

    invalidationRetainer.await()
    val flusher = !l1.coherency generate new StateMachine {
      val IDLE = makeInstantEntry()
      val SB_DRAIN = withStoreBuffer generate new State()
      val CMD, COMPLETION = new State()
      val arbiter = StreamArbiterFactory().transactionLock.lowerFirst.buildOn(invalidationPorts.map(_.cmd))
      val cmdCounter = Reg(UInt(log2Up(l1.SETS) + 1 bits))
      val inflight = (addressAt+1 to ctrlAt).map(elp.execute).map(e => e(l1.SEL) && e(l1.FLUSH)).orR

      val waiter = Reg(l1.WRITEBACK_BUSY.get)

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

    val coherentFlusher = l1.coherency.get generate new Area{
      invalidationPorts.foreach(_.cmd.ready := withStoreBuffer.mux(storeBuffer.empty, True))
    }

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

      val ls = new Area {
        val port = ports.addRet(Stream(LsuL1Cmd()))
        port.valid := isValid && SEL
        port.address := srcp.ADD_SUB.asUInt.resized  //TODO Overflow  ?
        port.size := SIZE
        port.load := LOAD
        port.store := STORE
        port.atomic := ATOMIC
        port.fromFlush := False
        port.fromAccess := False
        port.fromStoreBuffer := False

        val storeId = Reg(Decode.STORE_ID) init (0)
        storeId := storeId + U(port.fire)
        port.storeId := storeId
      }

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
        port.fromFlush := False
        port.fromAccess := True
        port.fromStoreBuffer := False
        port.storeId := 0
      }

      val flush = (flusher != null) generate new Area {
        val port = ports.addRet(Stream(LsuL1Cmd()))
        port.valid := flusher.isActive(flusher.CMD) && !flusher.cmdCounter.msb
        port.address := (flusher.cmdCounter << log2Up(l1.LINE_BYTES)).resized
        port.size := 0
        port.load := False
        port.store := False
        port.atomic := False
        port.fromFlush := True
        port.fromAccess := False
        port.fromStoreBuffer := False
        port.storeId := 0
        when(port.fire) {
          flusher.cmdCounter := flusher.cmdCounter + 1
        }
      }

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
        port.fromFlush := False
        port.fromAccess := False
        port.fromStoreBuffer := True
        storeBuffer.pop.ready := port.ready || flush
        port.storeId := storeBuffer.pop.op.storeId
      }

      val arbiter = StreamArbiterFactory().noLock.lowerFirst.buildOn(ports)
      arbiter.io.output.ready := !elp.isFreezed()
      l1.SEL := arbiter.io.output.valid
      l1.MIXED_ADDRESS := arbiter.io.output.address
      l1.MASK := AddressToMask(arbiter.io.output.address, arbiter.io.output.size, Riscv.LSLEN / 8)
      l1.SIZE := arbiter.io.output.size
      l1.LOAD := arbiter.io.output.load
      l1.ATOMIC := arbiter.io.output.atomic
      l1.STORE := arbiter.io.output.store
      l1.FLUSH := arbiter.io.output.fromFlush
      Decode.STORE_ID := arbiter.io.output.storeId
      FROM_ACCESS := arbiter.io.output.fromAccess
      FROM_WB := arbiter.io.output.fromStoreBuffer
      FROM_LSU := !(arbiter.io.output.fromFlush || arbiter.io.output.fromAccess || arbiter.io.output.fromStoreBuffer)
      if(withStoreBuffer) SB_PTR := storeBuffer.pop.ptr
      val SB_DATA = withStoreBuffer generate insert(storeBuffer.pop.op.data)
      val STORE_BUFFER_EMPTY = withStoreBuffer generate insert(storeBuffer.empty)
    }

    val tpk = onAddress0.translationPort.keys



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

    val preCtrl = new elp.Execute(ctrlAt-1){
      val MISS_ALIGNED = insert((1 to log2Up(LSLEN / 8)).map(i => l1.SIZE === i && l1.MIXED_ADDRESS(i - 1 downto 0) =/= 0).orR)
    }

    val onCtrl = new elp.Execute(ctrlAt) {
      val lsuTrap = False
      val mmuPageFault = tpk.PAGE_FAULT || STORE.mux(!tpk.ALLOW_WRITE, !tpk.ALLOW_READ)

      val pmaL1 = new PmaPort(Global.PHYSICAL_WIDTH, List(l1.LINE_BYTES), List(PmaLoad, PmaStore))
      val pmaIo = new PmaPort(Global.PHYSICAL_WIDTH, (0 to log2Up(Riscv.LSLEN / 8)).map(1 << _), List(PmaLoad, PmaStore))
      pmaL1.cmd.address := tpk.TRANSLATED
      pmaL1.cmd.op(0) := l1.STORE
      pmaIo.cmd.address := tpk.TRANSLATED
      pmaIo.cmd.size := l1.SIZE.asBits
      pmaIo.cmd.op(0) := l1.STORE

      val withAddress = !FENCE
      val IO = insert(pmaL1.rsp.fault && !pmaIo.rsp.fault && withAddress)

      val writeData = CombInit[Bits](elp(IntRegFile, riscv.RS2))
      if(Riscv.withFpu) when(FLOAT){
        val value = elp(FloatRegFile, riscv.RS2)
        writeData(value.bitsRange) := value
      }

      val scMiss = Bool()

      val io = new Area {
        val tooEarly = RegNext(True) clearWhen(elp.isFreezed()) init(False)
        val allowIt = RegNext(False) setWhen(!lsuTrap && !isCancel) init(False)
        val doIt = isValid && l1.SEL && IO
        val doItReg = RegNext(doIt) init(False)

        val cmdSent = RegInit(False) setWhen (bus.cmd.fire) clearWhen (!elp.isFreezed())
        bus.cmd.valid := doItReg && !cmdSent && allowIt && !tooEarly
        bus.cmd.write := l1.STORE
        bus.cmd.address := l1.PHYSICAL_ADDRESS //TODO Overflow on TRANSLATED itself ?
        bus.cmd.data := l1.WRITE_DATA
        bus.cmd.size := l1.SIZE
        bus.cmd.mask := l1.MASK
        bus.cmd.io := True
        bus.cmd.fromHart := True
        bus.cmd.hartId := Global.HART_ID
        bus.cmd.uopId := Decode.UOP_ID
//        if (withRva) {
//          bus.cmd.amoEnable := l1.ATOMIC
//          bus.cmd.amoOp := UOP(31 downto 27)
//        }

        val rsp = bus.rsp.toStream.halfPipe()
        rsp.ready := !elp.isFreezed()

        val freezeIt = doIt && (tooEarly || !rsp.valid && allowIt)
        elp.freezeWhen(freezeIt)

        l1.ackUnlock setWhen (cmdSent) //Ensure we don't create external lockup
      }


      val rspData = io.cmdSent.mux[Bits](io.rsp.data, l1.READ_DATA)
      val rspSplits = rspData.subdivideIn(8 bits)
      val rspShifted = Bits(LSLEN bits)
      val wordBytes = LSLEN / 8

      //Generate minimal mux to move from a wide aligned memory read to the register file shifter representation
      for (i <- 0 until wordBytes) {
        val srcSize = 1 << (log2Up(wordBytes) - log2Up(i + 1))
        val srcZipped = rspSplits.zipWithIndex.filter { case (v, b) => b % (wordBytes / srcSize) == i }
        val src = srcZipped.map(_._1)
        val range = log2Up(wordBytes) - 1 downto log2Up(wordBytes) - log2Up(srcSize)
        val sel = srcp.ADD_SUB(range).asUInt
        rspShifted(i * 8, 8 bits) := src.read(sel)
      }

      val READ_SHIFTED = insert(rspShifted)
      val SC_MISS = insert(scMiss)//insert(withRva.mux(io.doIt.mux[Bool](io.rsp.scMiss, scMiss), False))


      if (!Riscv.RVA.get) {
        scMiss := False
      }
      val rva = Riscv.RVA.get generate new Area {
        val srcBuffer = RegNext[Bits](READ_SHIFTED)
        val alu = new AtomicAlu(
          op = UOP(29, 3 bits),
          swap = UOP(27),
          mem = srcBuffer,
          rf = elp(IntRegFile, riscv.RS2),
          isWord = l1.SIZE === 2
        )
        val aluBuffer = RegNext(alu.result)
        val isAmo = l1.ATOMIC && l1.STORE && l1.LOAD
        when(isAmo) {
          writeData := aluBuffer
        }

        val delay = History(!elp.isFreezed(), 1 to 2)
        val freezeIt = isValid && SEL && isAmo && delay.orR
        elp.freezeWhen(freezeIt) //Note that if the refill is faster than 2 cycle, it may create issues

        assert(Global.HART_COUNT.get == 1)
        val nc = new Area {
          val capture = False
          val reserved = RegInit(False)
          val address = Reg(l1.PHYSICAL_ADDRESS)
          when(!elp.isFreezed() && isValid && FROM_LSU && l1.SEL && !lsuTrap && !IO) {
            when(l1.STORE){
              reserved := False
            } elsewhen(apply(l1.ATOMIC)){
              capture := True
            }
          }
          scMiss := !reserved
          l1.lockPort.valid := reserved
          l1.lockPort.address := address

          val age = Reg(UInt(6 bits)) //Will make the reservation die after 2**(bits-1) cycles
          when(age.msb || io.cmdSent){ // io.cmdSent ensure we do not create external deadlock
            reserved := False
          } otherwise {
            age := age + U(!elp.isFreezed())
          }
          when(capture){
            reserved  := !reserved //Toggling the reservation is a way to ensure that if the code is pulling value in a loop using lr => it doesn't always keep the reservation
            address :=  l1.PHYSICAL_ADDRESS
            age := 0
          }
        }
      }
      if(!Riscv.RVA){
        l1.lockPort.valid := False
        l1.lockPort.address := 0
      }

      val mapping = (0 to log2Up(Riscv.LSLEN / 8)).map { size =>
        val w = (1 << size) * 8
        size -> writeData(0, w bits).#*(Riscv.LSLEN / w)
      }
      l1.WRITE_DATA := l1.SIZE.muxListDc(mapping)

      val wb = withStoreBuffer generate new Area{
        when(FROM_WB) {
          l1.WRITE_DATA := onAddress0.SB_DATA
        }
        val tag = p2t(LsuL1.PHYSICAL_ADDRESS)
        val hits = B(storeBuffer.slots.map(s => s.valid && s.tag === tag))
        val hit = hits.orR
        val compatibleOp = FROM_LSU && STORE && !ATOMIC && !IO
        val notFull = !storeBuffer.ops.full && (storeBuffer.slotsFree || hit)
        val allowed = notFull && compatibleOp
        val slotOh = hits | storeBuffer.slotsFreeFirst.andMask(!hit)
        val loadHazard = LOAD && hit
        val selfHazard = FROM_WB && SB_PTR =/= storeBuffer.ops.freePtr
      }

      flushPort.valid := False
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False

      //TODO handle case were address isn't in the range of the virtual address ?
      trapPort.valid := False
      trapPort.hartId := Global.HART_ID
      trapPort.laneAge := Execute.LANE_AGE
      trapPort.tval := l1.MIXED_ADDRESS.asBits.resized //PC RESIZED
      trapPort.exception.assignDontCare()
      trapPort.code.assignDontCare()
      trapPort.arg.allowOverride() := 0

      val accessFault = (pmaL1.rsp.fault).mux[Bool](io.rsp.valid && io.rsp.error || l1.ATOMIC, l1.FAULT)
      when(accessFault) {
        lsuTrap := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
        trapPort.code(1) setWhen (STORE)
      }

      val l1Failed = !pmaL1.rsp.fault && (l1.HAZARD || l1.MISS || l1.MISS_UNIQUE)
      when(withStoreBuffer.mux((l1Failed || wb.hit) && !wb.allowed, l1Failed)){
        lsuTrap := True
        trapPort.exception := False
        trapPort.code := TrapReason.REDO
      }

      val pmaFault = pmaL1.rsp.fault && pmaIo.rsp.fault
      when(pmaFault) {
        lsuTrap := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
        trapPort.code(1) setWhen (STORE)
      }

      when(mmuPageFault) {
        lsuTrap := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_PAGE_FAULT
        trapPort.code(1) setWhen (STORE)
      }

      when(tpk.ACCESS_FAULT) {
        lsuTrap := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
        trapPort.code(1) setWhen (STORE)
      }

      trapPort.arg(0, 2 bits) := STORE.mux(B(TrapArg.STORE, 2 bits), B(TrapArg.LOAD, 2 bits))
      trapPort.arg(2, ats.getStorageIdWidth() bits) := ats.getStorageId(translationStorage)
      when(tpk.REDO) {
        lsuTrap := True
        trapPort.exception := False
        trapPort.code := TrapReason.MMU_REFILL
      }

      when(preCtrl.MISS_ALIGNED) {
        lsuTrap := True
        trapPort.exception := True
        trapPort.code := STORE.mux[Bits](CSR.MCAUSE_ENUM.STORE_MISALIGNED, CSR.MCAUSE_ENUM.LOAD_MISALIGNED).andMask(preCtrl.MISS_ALIGNED).resized
      }

      val triggerId = B(OHToUInt(onTrigger.HITS))
      when(onTrigger.HIT) {
        lsuTrap := True
        trapPort.exception := False
        trapPort.code := TrapReason.DEBUG_TRIGGER
        trapPort.tval(triggerId.bitsRange) := B(OHToUInt(onTrigger.HITS))
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

      when(!withAddress){
        lsuTrap := False
      }

      val fenceTrap = withStoreBuffer generate new Area{
        val valid = (ATOMIC || FENCE) && (!storeBuffer.empty || !onAddress0.STORE_BUFFER_EMPTY)
        when(valid) {
          lsuTrap := True
          trapPort.exception := False
          trapPort.code := TrapReason.REDO
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
          t.elsewhen((l1Failed || wb.hit) && wb.allowed && down.isFiring){
            storeBuffer.push.valid   := True
          }
          when(wb.compatibleOp && !wb.notFull) {
            storeBuffer.holdHart.waitIt := True
          }
        }
      }

//      val fromLsuAbord = FROM_LSU && (!isValid || isCancel || mmuFailure)
//      val generalAbord = l1.HAZARD || l1.FAULT || !l1.FLUSH && pmaL1.rsp.fault || preCtrl.MISS_ALIGNED || withStoreBuffer.mux(wb.loadHazard, False)
//      l1.ABORD := generalAbord || fromLsuAbord
////      l1.ABORD := FROM_LSU && (!isValid || isCancel || pmaL1.rsp.fault || l1.FAULT || mmuPageFault || tpk.ACCESS_FAULT || tpk.REDO || preCtrl.MISS_ALIGNED || pmaFault || withStoreBuffer.mux(wb.loadHazard, False))
//
//      l1.SKIP_WRITE := l1.ATOMIC && !l1.LOAD && scMiss || withStoreBuffer.mux(!FROM_WB && wb.hit || wb.selfHazard, False) || FROM_LSU && onTrigger.HIT


      val mmuFailure = mmuPageFault || tpk.ACCESS_FAULT || tpk.REDO
      l1.ABORD := FROM_LSU && (!isValid || isCancel || pmaL1.rsp.fault || l1.FAULT || mmuFailure || preCtrl.MISS_ALIGNED || pmaL1.rsp.fault || withStoreBuffer.mux(wb.loadHazard || fenceTrap.valid, False))
      l1.SKIP_WRITE := l1.ATOMIC && !l1.LOAD && scMiss || withStoreBuffer.mux(!FROM_WB && wb.hit || wb.selfHazard, False) || FROM_LSU && onTrigger.HIT

      if (withStoreBuffer) l1.ABORD setWhen (FROM_WB && wb.selfHazard)


      if(flusher != null) when(l1.SEL && l1.FLUSH && (l1.FLUSH_HIT || l1.HAZARD)){
        flusher.cmdCounter := l1.MIXED_ADDRESS(log2Up(l1.LINE_BYTES), log2Up(l1.SETS) bits).resized
      }

      if(withStoreBuffer) when(l1.SEL && FROM_WB && !elp.isFreezed() && withStoreBuffer.mux(!wb.selfHazard, True)){
        when(l1Failed) {
          storeBuffer.ops.popPtr := storeBuffer.ops.freePtr
          when(!wb.selfHazard){
            storeBuffer.waitL1.capture(down)
          }
        } otherwise {
          storeBuffer.ops.freePtr := storeBuffer.ops.freePtr + 1
          for (slot <- storeBuffer.slots) when(slot.ptr === storeBuffer.ops.freePtr) {
            slot.valid := False
          }
        }
      }

      val access = dbusAccesses.nonEmpty generate new Area {
        assert(dbusAccesses.size == 1)
        val rsp = dbusAccesses.head.rsp
        rsp.valid := l1.SEL && FROM_ACCESS && !elp.isFreezed()
        rsp.data     := l1.READ_DATA
        rsp.error    := l1.FAULT
        rsp.redo     := l1Failed
        rsp.waitSlot := 0
        rsp.waitAny  := False //TODO
        if(withStoreBuffer) when(wb.hit){
          rsp.redo := True
          onAddress0.access.sbWaiter setWhen(rsp.valid)
        }
        when(pmaFault){
          rsp.error := True
          rsp.redo := False
        }
        when(rsp.fire && rsp.redo) {
          onAddress0.access.waiter.capture(down)
        }
      }

      val hartRegulation = new L1Waiter{
        host[DispatchPlugin].haltDispatchWhen(valid)
        when(isValid && SEL && withStoreBuffer.mux(LOAD, True) && (l1.HAZARD || l1.MISS || l1.MISS_UNIQUE)){
          capture(down)
        }
        events.foreach(_.waiting setWhen(valid))
      }
    }

    val onWb = new elp.Execute(wbAt){
      iwb.valid := SEL && !FLOAT
      iwb.payload := onCtrl.READ_SHIFTED

      if (withRva) when(l1.ATOMIC && !l1.LOAD) {
        iwb.payload(0) := onCtrl.SC_MISS
        iwb.payload(7 downto 1) := 0
      }

      fpwb.foreach{p =>
        p.valid := SEL && FLOAT
        p.payload := onCtrl.READ_SHIFTED
        if(Riscv.RVD) when(SIZE === 2) {
          p.payload(63 downto 32).setAll()
        }
      }

      val storeFire = down.isFiring && AguPlugin.SEL && AguPlugin.STORE && !onCtrl.IO
      val storeBroadcast = down.isReady && l1.SEL && l1.STORE && !l1.ABORD && !l1.SKIP_WRITE && !l1.MISS && !l1.MISS_UNIQUE && !l1.HAZARD
    }

    buildBefore.release()
  }


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
    val l1 = new PmaLogic(logic.onCtrl.pmaL1, l1Regions)
    val io = new PmaLogic(logic.onCtrl.pmaIo, ioRegions)
  }
}
