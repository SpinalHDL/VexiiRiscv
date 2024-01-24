package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.decode.Decode
import vexiiriscv.decode.Decode.UOP
import vexiiriscv.execute.AguPlugin._
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService, DBusAccessService}
import vexiiriscv.misc.{AddressToMask, TrapArg, TrapReason, TrapService}
import vexiiriscv.riscv.Riscv.LSLEN
import vexiiriscv.riscv._
import vexiiriscv.schedule.ScheduleService
import vexiiriscv.{Global, riscv}

class LsuPlugin(var layer : LaneLayer,
                var withRva : Boolean,
                //                var translationStorageParameter: Any,
                //                var translationPortParameter: Any,
                var addressAt: Int = 0,
                var ctrlAt: Int = 2,
                var wbAt : Int = 2) extends FiberPlugin with DBusAccessService{

  val WITH_L1 = Payload(Bool())
  override def accessRefillCount: Int = 0
  override def accessWake: Bits = B(0)

  val logic = during setup new Area{
    val elp = host.find[ExecuteLanePlugin](_.laneName == layer.laneName)
    val ifp = host.find[IntFormatPlugin](_.laneName == layer.laneName)
    val srcp = host.find[SrcPlugin](_.layer == layer)
//    val ats = host[AddressTranslationService]
    val ts = host[TrapService]
    val ss = host[ScheduleService]
    val buildBefore = retains(elp.pipelineLock/*, ats.portsLock*/)
//    val atsStorageLock = retains(ats.storageLock)
    val retainer = retains(elp.uopLock, srcp.elaborationLock, ifp.elaborationLock, ts.trapLock, ss.elaborationLock)
    awaitBuild()
    Riscv.RVA.set(withRva)

//    val translationStorage = ats.newStorage(translationStorageParameter)
//    atsStorageLock.release()

    val trapPort = ts.newTrap(layer.el.getAge(ctrlAt), Execute.LANE_AGE_WIDTH)
    val flushPort = ss.newFlushPort(layer.el.getExecuteAge(addressAt), laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
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
      op.dontFlushFrom(ctrlAt+1)
    }

    for(store <- frontend.writingMem ++ amos){
      val op = layer(store)
      op.mayFlushUpTo(ctrlAt)
      op.dontFlushFrom(ctrlAt+1)
      op.addRsSpec(RS2, 0) //TODO
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

//    val onAddress = new addressCtrl.Area{
//      val RAW_ADDRESS = insert(srcp.ADD_SUB.asUInt)
//
//      val translationPort = ats.newTranslationPort(
//        nodes = Seq(forkCtrl.down),
//        rawAddress = RAW_ADDRESS,
//        allowRefill = insert(True),
//        usage = AddressTranslationPortUsage.LOAD_STORE,
//        portSpec = translationPortParameter,
//        storageSpec = translationStorage
//      )
//    }

    val l1 = LsuL1
    val onAddress = new elp.Execute(addressAt){
//      val tpk =  onAddress.translationPort.keys

      val RS2 = elp(IntRegFile, riscv.RS2)
      val mapping = (0 to log2Up(Riscv.LSLEN / 8)).map { size =>
        val w = (1 << size) * 8
        size -> up(RS2)(0, w bits).#*(Riscv.LSLEN / w)
      }

      val SEL = Payload(Bool())
      l1.MIXED_ADDRESS := srcp.ADD_SUB.asUInt
      l1.VIRTUAL_ENABLE := True
      l1.WRITE_MASK := AddressToMask(l1.MIXED_ADDRESS, SIZE, Riscv.LSLEN/8)
      l1.WRITE_DATA :=  bus.cmd.size.muxListDc(mapping)

      val MISS_ALIGNED = insert((1 to log2Up(LSLEN / 8)).map(i => SIZE === i && l1.MIXED_ADDRESS(i - 1 downto 0) =/= 0).orR)
    }



    for(eid <- addressAt + 1 to ctrlAt) elp.execute(eid).up(WITH_L1).setAsReg().init(False)

    val onCtrl = new elp.Execute(ctrlAt) {
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

      val skip = False

//      if (withSpeculativeLoadFlush) when(LOAD && tpk.IO && elp.atRiskOfFlush(forkAt)) {
//        skip := True
//        trapPort.exception := False
//        trapPort.code := TrapReason.REDO
//      }

      when(l1.PAGE_FAULT/* || LOAD.mux(!tpk.ALLOW_READ, !tpk.ALLOW_WRITE)*/) {
        skip := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_PAGE_FAULT
        trapPort.code(1) setWhen (!LOAD)
      }

      when(l1.ACCESS_FAULT) {
        skip := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
        trapPort.code(1) setWhen (!LOAD)
      }

//      trapPort.arg(0, 2 bits) := LOAD.mux(B(TrapArg.LOAD, 2 bits), B(TrapArg.STORE, 2 bits))
//      trapPort.arg(2, l1.getStorageIdWidth() bits) := l1.getStorageId()
      when(l1.REDO) {
        ???
        skip := True
        trapPort.exception := False
        trapPort.code := TrapReason.MMU_REFILL
      }

      when(onAddress.MISS_ALIGNED) {
        skip := True
        trapPort.exception := True
        trapPort.code := LOAD.mux[Bits](CSR.MCAUSE_ENUM.LOAD_MISALIGNED, CSR.MCAUSE_ENUM.STORE_MISALIGNED).andMask(onAddress.MISS_ALIGNED).resized
      }

      when(isValid && SEL && skip) {
        trapPort.valid := True
        flushPort.valid := True
        bypass(Global.TRAP) := True
        bypass(Global.COMMIT) := False
      }
    }

    val onWb = new elp.Execute(wbAt){
      val rspSplits = l1.READ_DATA.subdivideIn(8 bits)
      val rspShifted = Bits(LSLEN bits)
      val wordBytes = LSLEN/8

      //Generate minimal mux to move from a wide aligned memory read to the register file shifter representation
      for (i <- 0 until wordBytes) {
        val srcSize = 1 << (log2Up(wordBytes) - log2Up(i + 1))
        val srcZipped = rspSplits.zipWithIndex.filter { case (v, b) => b % (wordBytes / srcSize) == i }
        val src = srcZipped.map(_._1)
        val range = log2Up(wordBytes)-1 downto log2Up(wordBytes) - log2Up(srcSize)
        val sel = srcp.ADD_SUB(range).asUInt
        rspShifted(i * 8, 8 bits) := src.read(sel)
      }

      iwb.valid := SEL
      iwb.payload := rspShifted

      if (withRva) when(!LOAD && SC) {
        ???
//        iwb.payload(0) := onJoin.SC_MISS
//        iwb.payload(7 downto 1) := 0
      }
    }

    buildBefore.release()
  }
}
