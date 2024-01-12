package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.{Global, riscv}
import vexiiriscv.riscv.{CSR, Const, IntRegFile, MicroOp, RS1, RS2, Riscv, Rvi}
import AguPlugin._
import spinal.core.fiber.Retainer
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.FetchPipelinePlugin
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService, DBusAccessService}
import vexiiriscv.misc.{AddressToMask, TrapArg, TrapReason, TrapService}
import vexiiriscv.riscv.Riscv.{LSLEN, XLEN}
import spinal.lib.misc.pipeline._
import vexiiriscv.decode.Decode.{INSTRUCTION_SLICE_COUNT_WIDTH, UOP}
import vexiiriscv.schedule.{ReschedulePlugin, ScheduleService}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CachelessBusAmo{
  val LR = 0x02
  val SC = 0x03
  val AMOSWAP = 0x01
  val AMOADD = 0x00
  val AMOXOR = 0x04
  val AMOAND = 0x0C
  val AMOOR = 0x08
  val AMOMIN = 0x10
  val AMOMAX = 0x14
  val AMOMINU = 0x18
  val AMOMAXU = 0x1c
}

case class CachelessBusParam(addressWidth : Int, dataWidth : Int, hartIdWidth : Int, uopIdWidth : Int, withAmo : Boolean){

}

case class CachelessCmd(p : CachelessBusParam) extends Bundle{
  val write = Bool()
  val address = UInt(p.addressWidth bits)
  val data = Bits(p.dataWidth bit)
  val size = UInt(log2Up(log2Up(p.dataWidth / 8) + 1) bits)
  val mask = Bits(p.dataWidth / 8 bits)
  val io = Bool() //This is for verification purposes, allowing RVLS to track stuff
  val fromHart = Bool() //This is for verification purposes, allowing RVLS to track stuff
  val hartId = UInt(p.hartIdWidth bits)
  val uopId = UInt(p.uopIdWidth bits)
  val amoEnable = p.withAmo generate Bool()
  val amoOp = p.withAmo generate Bits(5 bits)
}

case class CachelessRsp(p : CachelessBusParam) extends Bundle{
  val error = Bool()
  val data  = Bits(p.dataWidth bits)
  val scMiss = p.withAmo generate Bool()
}

case class CachelessBus(p : CachelessBusParam) extends Bundle with IMasterSlave {
  var cmd = Stream(CachelessCmd(p))
  var rsp = Flow(CachelessRsp(p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

class LsuCachelessPlugin(var layer : LaneLayer,
                         var withAmo : Boolean,
                         var withSpeculativeLoadFlush : Boolean, //WARNING, the fork cmd may be flushed out of existance before firing
                         var translationStorageParameter: Any,
                         var translationPortParameter: Any,
                         var addressAt: Int = 0,
                         var forkAt: Int = 0,
                         var joinAt: Int = 1,
                         var wbAt: Int = 2) extends FiberPlugin with DBusAccessService{

  val WITH_RSP = Payload(Bool())
  override def accessRefillCount: Int = 0
  override def accessWake: Bits = B(0)

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
    Riscv.RVA.set(withAmo)

    val translationStorage = ats.newStorage(translationStorageParameter)
    atsStorageLock.release()

    val trapPort = ts.newTrap(layer.el.getAge(forkAt), Execute.LANE_AGE_WIDTH)
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
      op.mayFlushUpTo(forkAt) // page fault / trap
      withSpeculativeLoadFlush match {
        case true =>
        case false => op.dontFlushFrom(forkAt + 1)
      }
    }

    for(store <- frontend.writingMem ++ amos){
      val op = layer(store)
      op.mayFlushUpTo(forkAt)
      op.addRsSpec(RS2, forkAt)
      op.dontFlushFrom(forkAt+1)
    }

    layer.add(Rvi.FENCE) //TODO
    layer(Rvi.FENCE).setCompletion(joinAt)

    for(uop <- frontend.writingMem if layer(uop).completion.isEmpty) layer(uop).setCompletion(joinAt)

    retainer.release()

    val injectCtrl = elp.ctrl(0)
    val inject = new injectCtrl.Area {
      SIZE := Decode.UOP(13 downto 12).asUInt
    }

    // Hardware elaboration
    val addressCtrl = elp.execute(addressAt)
    val forkCtrl = elp.execute(forkAt)
    val joinCtrl = elp.execute(joinAt)
    val wbCtrl = elp.execute(wbAt)

    val busParam = CachelessBusParam(
      addressWidth = Global.PHYSICAL_WIDTH,
      dataWidth = Riscv.LSLEN,
      hartIdWidth = Global.HART_ID_WIDTH,
      uopIdWidth = Decode.UOP_ID_WIDTH,
      withAmo = withAmo
    )
    val bus = master(CachelessBus(busParam))

    accessRetainer.await()

    val onAddress = new addressCtrl.Area{
      val RAW_ADDRESS = insert(srcp.ADD_SUB.asUInt)

      val translationPort = ats.newTranslationPort(
        nodes = Seq(forkCtrl.down),
        rawAddress = RAW_ADDRESS,
        allowRefill = insert(True),
        usage = AddressTranslationPortUsage.LOAD_STORE,
        portSpec = translationPortParameter,
        storageSpec = translationStorage
      )
    }

    val onFork = new forkCtrl.Area{
      val tpk =  onAddress.translationPort.keys
      val MISS_ALIGNED = insert((1 to log2Up(LSLEN / 8)).map(i => SIZE === i && onAddress.RAW_ADDRESS(i - 1 downto 0) =/= 0).orR) //TODO remove from speculLoad and handle it with trap
      val RS2 = elp(IntRegFile, riscv.RS2)

      val skip = False

      val cmdSent = RegInit(False) setWhen(bus.cmd.fire) clearWhen(!elp.isFreezed())
      bus.cmd.valid := isValid && SEL && !cmdSent && !hasCancelRequest && !skip
      bus.cmd.write := !LOAD
      bus.cmd.address := tpk.TRANSLATED //TODO Overflow on TRANSLATED itself ?
      val mapping = (0 to log2Up(Riscv.LSLEN / 8)).map{size =>
        val w = (1 << size) * 8
        size -> up(RS2)(0, w bits).#*(Riscv.LSLEN / w)
      }
      bus.cmd.data := bus.cmd.size.muxListDc(mapping)
      bus.cmd.size := SIZE.resized
      bus.cmd.mask := AddressToMask(bus.cmd.address, bus.cmd.size, Riscv.LSLEN/8)
      bus.cmd.io := tpk.IO
      bus.cmd.fromHart := True
      bus.cmd.hartId := Global.HART_ID
      bus.cmd.uopId := Decode.UOP_ID
      if(withAmo) {
        bus.cmd.amoEnable := LOAD.mux[Bool](LR, SC || AMO)
        bus.cmd.amoOp     := UOP(31 downto 27)
      }
      //TODO amo AQ/RL

      elp.freezeWhen(bus.cmd.isStall)

      flushPort.valid := False
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False

      //TODO handle case were address isn't in the range of the virtual address ?
      trapPort.valid :=  False
      trapPort.hartId := Global.HART_ID
      trapPort.laneAge := Execute.LANE_AGE
      trapPort.tval := onAddress.RAW_ADDRESS.asBits.resized //PC RESIZED
      trapPort.exception.assignDontCare()
      trapPort.code.assignDontCare()
      trapPort.arg.allowOverride() := 0

      if(withSpeculativeLoadFlush) when(LOAD && tpk.IO && elp.atRiskOfFlush(forkAt)){
        skip := True
        trapPort.exception := False
        trapPort.code := TrapReason.REDO
      }

      when(tpk.PAGE_FAULT || LOAD.mux(!tpk.ALLOW_READ, !tpk.ALLOW_WRITE)) {
        skip := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_PAGE_FAULT
        trapPort.code(1) setWhen(!LOAD)
      }

      when(tpk.ACCESS_FAULT) {
        skip := True
        trapPort.exception := True
        trapPort.code := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
        trapPort.code(1) setWhen (!LOAD)
      }

      trapPort.arg(0, 2 bits) := LOAD.mux(B(TrapArg.LOAD, 2 bits), B(TrapArg.STORE, 2 bits))
      trapPort.arg(2, ats.getStorageIdWidth() bits) := ats.getStorageId(translationStorage)
      when(tpk.REDO) {
        skip := True
        trapPort.exception := False
        trapPort.code := TrapReason.MMU_REFILL
      }

      when(MISS_ALIGNED){
        skip := True
        trapPort.exception := True
        trapPort.code := LOAD.mux[Bits](CSR.MCAUSE_ENUM.LOAD_MISALIGNED, CSR.MCAUSE_ENUM.STORE_MISALIGNED).andMask(MISS_ALIGNED).resized
      }

      when(isValid && SEL && skip){
        trapPort.valid := True
        flushPort.valid := True
        bypass(Global.TRAP) := True
        bypass(Global.COMMIT) := False
      }

      WITH_RSP := bus.cmd.valid || cmdSent
      val access = dbusAccesses.nonEmpty generate new Area{
        assert(dbusAccesses.size == 1)
        val allowIt = !(isValid && SEL) && !cmdSent
        val cmd = dbusAccesses.head.cmd
        cmd.ready := allowIt && !elp.isFreezed()
        when(allowIt){
          bus.cmd.valid := cmd.valid
          bus.cmd.write := False
          bus.cmd.address := cmd.address
          bus.cmd.size := cmd.size
          bus.cmd.fromHart := False
        }
      }
    }

    val onJoin = new joinCtrl.Area{
      val buffer = bus.rsp.toStream.queueLowLatency(joinAt-forkAt+1).combStage
      val SC_MISS = insert(withAmo.mux(buffer.scMiss, False))
      val READ_DATA = insert(buffer.data)
      elp.freezeWhen(WITH_RSP && !buffer.valid)
      buffer.ready := WITH_RSP && isReady
      assert(!(isValid && hasCancelRequest && SEL && !LOAD && !up(Global.TRAP)), "LsuCachelessPlugin saw unexpected select && !LOAD && cancel request") //TODO add tpk.IO and along the way)) //TODO add tpk.IO and along the way
      val access = dbusAccesses.nonEmpty generate new Area {
        assert(dbusAccesses.size == 1)
        val rsp = dbusAccesses.head.rsp
        rsp.valid := !(isValid && SEL) && WITH_RSP && buffer.valid
        rsp.data := buffer.data
        rsp.error := buffer.error
        rsp.redo := False
        rsp.waitAny := False
      }
    }

    for(eid <- forkAt + 1 to joinAt) elp.execute(eid).up(WITH_RSP).setAsReg().init(False)

    val onWb = new wbCtrl.Area{
      val RSP_DATA = insert(this(onJoin.READ_DATA))
      if(withAmo) when(!LOAD && SC){
        RSP_DATA(0) := onJoin.SC_MISS
        RSP_DATA(7 downto 1) := 0
      }
      val rspSplits = RSP_DATA.subdivideIn(8 bits)
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
    }

    buildBefore.release()
  }
}
