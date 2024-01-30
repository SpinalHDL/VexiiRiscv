package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.decode.Decode
import vexiiriscv.decode.Decode.UOP
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService, DBusAccessService}
import vexiiriscv.misc.{AddressToMask, TrapArg, TrapReason, TrapService}
import vexiiriscv.riscv.Riscv.LSLEN
import vexiiriscv.riscv._
import vexiiriscv.schedule.ScheduleService
import vexiiriscv.{Global, riscv}
import vexiiriscv.execute._
import vexiiriscv.execute.lsu.AguPlugin._
import vexiiriscv.fetch.LsuL1Service

import scala.collection.mutable.ArrayBuffer


class LsuPlugin(var layer : LaneLayer,
                var withRva : Boolean,
                var translationStorageParameter: Any,
                var translationPortParameter: Any,
                var addressAt: Int = 0,
                var ctrlAt: Int = 2,
                var wbAt : Int = 2) extends FiberPlugin with DBusAccessService with LsuCachelessBusProvider with LsuL1Service{

  override def accessRefillCount: Int = 0
  override def accessWake: Bits = B(0)

  override def getLsuCachelessBus(): LsuCachelessBus = logic.bus

  val logic = during setup new Area{
    val elp = host.find[ExecuteLanePlugin](_.laneName == layer.laneName)
    val ifp = host.find[IntFormatPlugin](_.laneName == layer.laneName)
    val srcp = host.find[SrcPlugin](_.layer == layer)
    val ats = host[AddressTranslationService]
    val ts = host[TrapService]
    val ss = host[ScheduleService]
    val buildBefore = retains(elp.pipelineLock, ats.portsLock)
    val atsStorageLock = retains(ats.storageLock)
    val retainer = retains(elp.uopLock, srcp.elaborationLock, ifp.elaborationLock, ts.trapLock, ss.elaborationLock)
    awaitBuild()
    Riscv.RVA.set(withRva)

    val translationStorage = ats.newStorage(translationStorageParameter)
    atsStorageLock.release()

    val trapPort = ts.newTrap(layer.el.getExecuteAge(ctrlAt), Execute.LANE_AGE_WIDTH)
    val flushPort = ss.newFlushPort(layer.el.getExecuteAge(ctrlAt), laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
    val frontend = new AguFrontend(layer, host)

    // IntFormatPlugin specification
    val iwb = ifp.access(wbAt)
    val amos = Riscv.RVA.get.option(frontend.amos.uops).toList.flatten
    for(load <- frontend.writingRf ++ amos){
      val spec = Rvi.loadSpec(load)
      val op = layer(load)
      ifp.addMicroOp(iwb, op)
      spec.signed match {
        case false => ifp.zeroExtend(iwb, op, spec.width)
        case true  => ifp.signExtend(iwb, op, spec.width)
      }
      op.mayFlushUpTo(ctrlAt) // page fault / trap
      op.dontFlushFrom(ctrlAt)
    }

    for(store <- frontend.writingMem ++ amos){
      val op = layer(store)
      op.mayFlushUpTo(ctrlAt)
      op.dontFlushFrom(ctrlAt)
      op.addRsSpec(RS2, 0) //TODO ! for now the executeLanePlugin store bypass bypass its own value XD, need fix to only bypass from point which are solved
    }

    layer.add(Rvi.FENCE) //TODO
    layer(Rvi.FENCE).setCompletion(ctrlAt)

    for(uop <- frontend.writingMem if layer(uop).completion.isEmpty) layer(uop).setCompletion(ctrlAt) //TODO ctrlAt

    retainer.release()

    val injectCtrl = elp.ctrl(0)
    val inject = new injectCtrl.Area {
      SIZE := Decode.UOP(13 downto 12).asUInt
    }

    val busParam = LsuCachelessBusParam(
      addressWidth = Global.PHYSICAL_WIDTH,
      dataWidth = Riscv.LSLEN,
      hartIdWidth = Global.HART_ID_WIDTH,
      uopIdWidth = Decode.UOP_ID_WIDTH,
      withAmo = withRva
    )
    val bus = master(LsuCachelessBus(busParam))

    accessRetainer.await()
    val l1 = LsuL1
    val FROM_ACCESS = Payload(Bool())
    val FROM_LSU = Payload(Bool())


    invalidationRetainer.await()
    val flusher = new StateMachine {
      val IDLE = makeInstantEntry()
      val CMD, COMPLETION = new State()
      val arbiter = StreamArbiterFactory().transactionLock.lowerFirst.buildOn(invalidationPorts.map(_.cmd))
      val cmdCounter = Reg(UInt(log2Up(l1.SETS) + 1 bits))
      val inflight = (addressAt+1 to ctrlAt).map(elp.execute).map(e => e(l1.SEL) && e(l1.FLUSH)).orR

      val waiter = Reg(l1.WRITEBACK_BUSY.get)

      IDLE.whenIsActive{
        cmdCounter := 0
        when(arbiter.io.output.valid) {
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
        }
      }
    }

    val onAddress0 = new elp.Execute(addressAt){
      val translationPort = ats.newTranslationPort(
        nodes = Seq(elp.execute(addressAt).down, elp.execute(addressAt+1).down),
        rawAddress = l1.MIXED_ADDRESS,
        forcePhysical = FROM_ACCESS,
        usage = AddressTranslationPortUsage.LOAD_STORE,
        portSpec = translationPortParameter,
        storageSpec = translationStorage
      )

      case class Cmd() extends Bundle {
        val address = l1.MIXED_ADDRESS()
        val mask = l1.MASK()
        val load, store, atomic = Bool()
        val fromFlush = Bool()
        val fromAccess = Bool()
      }

      val ports = ArrayBuffer[Stream[Cmd]]()

      val ls = new Area {
        val port = ports.addRet(Stream(Cmd()))
        port.valid := isValid && SEL
        port.address := srcp.ADD_SUB.asUInt.resized  //TODO Overflow  ?
        port.mask := AddressToMask(l1.MIXED_ADDRESS, SIZE, Riscv.LSLEN / 8)
        port.load := LOAD
        port.store := STORE
        port.atomic := ATOMIC
        port.fromFlush := False
        port.fromAccess := False
      }

      val access = dbusAccesses.nonEmpty generate new Area {
        assert(dbusAccesses.size == 1)
        val cmd = dbusAccesses.head.cmd
        val port = ports.addRet(Stream(Cmd()))
        port.arbitrationFrom(cmd)
        port.address := cmd.address.resized
        port.mask := AddressToMask(l1.MIXED_ADDRESS, cmd.size, Riscv.LSLEN / 8)
        port.load := True
        port.store := False
        port.atomic := False
        port.fromFlush := False
        port.fromAccess := True
      }

      val flush = new Area {
        val port = ports.addRet(Stream(Cmd()))
        port.valid := flusher.isActive(flusher.CMD) && !flusher.cmdCounter.msb
        port.address := (flusher.cmdCounter << log2Up(l1.LINE_BYTES)).resized
        port.mask := 0
        port.load := False
        port.store := False
        port.atomic := False
        port.fromFlush := True
        port.fromAccess := False
        when(port.fire) {
          flusher.cmdCounter := flusher.cmdCounter + 1
        }
      }

      val arbiter = StreamArbiterFactory().noLock.lowerFirst.buildOn(ports)
      arbiter.io.output.ready := !elp.isFreezed()
      l1.SEL := arbiter.io.output.valid
      l1.MIXED_ADDRESS := arbiter.io.output.address
      l1.MASK := arbiter.io.output.mask
      l1.LOAD := arbiter.io.output.load
      l1.ATOMIC := arbiter.io.output.atomic
      l1.STORE := arbiter.io.output.store
      l1.FLUSH := arbiter.io.output.fromFlush
      FROM_ACCESS := arbiter.io.output.fromAccess
      FROM_LSU := !(arbiter.io.output.fromFlush || arbiter.io.output.fromAccess)
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

    val onCtrl = new elp.Execute(ctrlAt) {
      val MISS_ALIGNED = insert((1 to log2Up(LSLEN / 8)).map(i => SIZE === i && l1.MIXED_ADDRESS(i - 1 downto 0) =/= 0).orR)
      val mmuPageFault = tpk.PAGE_FAULT || STORE.mux(!tpk.ALLOW_WRITE, !tpk.ALLOW_READ)

      val writeData = CombInit[Bits](elp(IntRegFile, riscv.RS2))
      val scMiss = Bool()

      val io = new Area {
        val allowed = CombInit(this (tpk.IO))
        val doIt = isValid && l1.SEL && allowed

        val cmdSent = RegInit(False) setWhen (bus.cmd.fire) clearWhen (!elp.isFreezed())
        bus.cmd.valid := doIt && !cmdSent
        bus.cmd.write := l1.STORE
        bus.cmd.address := l1.PHYSICAL_ADDRESS //TODO Overflow on TRANSLATED itself ?
        bus.cmd.data := l1.WRITE_DATA
        bus.cmd.size := SIZE.resized
        bus.cmd.mask := l1.MASK
        bus.cmd.io := True
        bus.cmd.fromHart := True
        bus.cmd.hartId := Global.HART_ID
        bus.cmd.uopId := Decode.UOP_ID
        if (withRva) {
          bus.cmd.amoEnable := l1.ATOMIC
          bus.cmd.amoOp := UOP(31 downto 27)
        }

        val rsp = bus.rsp.toStream.halfPipe()
        rsp.ready := !elp.isFreezed()

        val freezeIt = doIt && !rsp.valid
        elp.freezeWhen(freezeIt)
      }


      val rspData = io.doIt.mux[Bits](io.rsp.data, l1.READ_DATA)
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
      val SC_MISS = insert(withRva.mux(io.doIt.mux[Bool](io.rsp.scMiss, scMiss), False))


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
          isWord = SIZE === 2
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
          val reserved = RegInit(False)
          when(!elp.isFreezed() && l1.SEL && !l1.ABORD) {
            reserved setWhen (l1.ATOMIC && !l1.STORE)
            reserved clearWhen (l1.STORE)
          }
          scMiss := !reserved
        }
      }

      val mapping = (0 to log2Up(Riscv.LSLEN / 8)).map { size =>
        val w = (1 << size) * 8
        size -> writeData(0, w bits).#*(Riscv.LSLEN / w)
      }
      l1.WRITE_DATA := SIZE.muxListDc(mapping)




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

      val doTrap = False
      when(tpk.IO.mux[Bool](io.rsp.valid && io.rsp.error, l1.FAULT)) {
        doTrap := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
        trapPort.code(1) setWhen (STORE)
      }

      val l1Redo = !tpk.IO && (l1.HAZARD || l1.MISS || l1.MISS_UNIQUE)
      when(l1Redo){
        doTrap := True
        trapPort.exception := False
        trapPort.code := TrapReason.REDO
      }

      when(mmuPageFault) {
        doTrap := True; io.allowed := False
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_PAGE_FAULT
        trapPort.code(1) setWhen (STORE)
      }

      when(tpk.ACCESS_FAULT) {
        doTrap := True; io.allowed := False
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
        trapPort.code(1) setWhen (STORE)
      }

      trapPort.arg(0, 2 bits) := STORE.mux(B(TrapArg.STORE, 2 bits), B(TrapArg.LOAD, 2 bits))
      trapPort.arg(2, ats.getStorageIdWidth() bits) := ats.getStorageId(translationStorage)
      when(tpk.REDO) {
        doTrap := True; io.allowed := False
        trapPort.exception := False
        trapPort.code := TrapReason.MMU_REFILL
      }

      when(MISS_ALIGNED) {
        doTrap := True; io.allowed := False
        trapPort.exception := True
        trapPort.code := STORE.mux[Bits](CSR.MCAUSE_ENUM.STORE_MISALIGNED, CSR.MCAUSE_ENUM.LOAD_MISALIGNED).andMask(MISS_ALIGNED).resized
      }

      when(isValid && SEL && doTrap) {
        trapPort.valid := True
        flushPort.valid := True
        bypass(Global.TRAP) := True
        bypass(Global.COMMIT) := False
      }

      l1.ABORD := FROM_LSU && (!isValid || isCancel || tpk.IO || l1.FAULT || mmuPageFault || tpk.ACCESS_FAULT || tpk.REDO || MISS_ALIGNED)
      l1.SKIP_WRITE := l1.ATOMIC && !l1.LOAD && scMiss

      when(l1.SEL && l1.FLUSH && (l1.FLUSH_HIT || l1.HAZARD)){
        flusher.cmdCounter := l1.MIXED_ADDRESS(log2Up(l1.LINE_BYTES), log2Up(l1.SETS) bits).resized
      }

      val access = dbusAccesses.nonEmpty generate new Area {
        assert(dbusAccesses.size == 1)
        val rsp = dbusAccesses.head.rsp
        rsp.valid := l1.SEL && FROM_ACCESS && !elp.isFreezed()
        rsp.data     := l1.READ_DATA
        rsp.error    := l1.FAULT
        rsp.redo     := l1Redo
        rsp.waitSlot := 0
        rsp.waitAny  := False //TODO
      }
    }

    val onWb = new elp.Execute(wbAt){
      iwb.valid := SEL
      iwb.payload := onCtrl.READ_SHIFTED

      if (withRva) when(l1.ATOMIC && !l1.LOAD) {
        iwb.payload(0) := onCtrl.SC_MISS
        iwb.payload(7 downto 1) := 0
      }
    }

    buildBefore.release()
  }
}
