package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.{Global, riscv}
import vexiiriscv.riscv.{Const, IntRegFile, MicroOp, RS1, RS2, Riscv, Rvi}
import AguPlugin._
import vexiiriscv.decode.Decode
import vexiiriscv.memory.{AddressTranslationPortUsage, AddressTranslationService}
import vexiiriscv.misc.AddressToMask
import vexiiriscv.riscv.Riscv.{LSLEN, XLEN}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class CachelessBusParam(addressWidth : Int, dataWidth : Int, hartIdWidth : Int){

}

case class CachelessCmd(p : CachelessBusParam) extends Bundle{
  val write = Bool()
  val address = UInt(p.addressWidth bits)
  val data = Bits(p.dataWidth bit)
  val size = UInt(log2Up(log2Up(p.dataWidth / 8) + 1) bits)
  val mask = Bits(p.dataWidth / 8 bits)
  val io = Bool() //This is for verification purposes, allowing RVLS to track stuff
  val hartId = UInt(p.hartIdWidth bits)
}

case class CachelessRsp(p : CachelessBusParam) extends Bundle{
  val error = Bool()
  val data  = Bits(p.dataWidth bits)
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
                         var withSpeculativeLoadFlush : Boolean, //WARNING, the fork cmd may be flushed out of existance before firing
                         var translationStorageParameter: Any,
                         var translationPortParameter: Any,
                         var addressAt: Int = 0,
                         var forkAt: Int = 0,
                         var joinAt: Int = 1,
                         var wbAt: Int = 2) extends FiberPlugin{
  lazy val elp = host.find[ExecuteLanePlugin](_.laneName == layer.laneName)
  lazy val ifp = host.find[IntFormatPlugin](_.laneName == layer.laneName)
  lazy val srcp = host.find[SrcPlugin](_.layer == layer)
  lazy val ats = host[AddressTranslationService]
  lazy val rp = host[RedoPlugin]
  buildBefore(elp.pipelineLock)
  setupRetain(elp.uopLock)
  setupRetain(srcp.elaborationLock)
  setupRetain(ifp.elaborationLock)
  setupRetain(ats.elaborationLock)
  setupRetain(rp.elaborationLock)

  val logic = during build new Area{
    val redoPort = withSpeculativeLoadFlush generate rp.newPort(forkAt)
    rp.elaborationLock.release()

    val frontend = new AguFrontend(layer, host)

    // IntFormatPlugin specification
    val iwb = ifp.access(wbAt)
    for(load <- frontend.loads){
      val spec = Rvi.loadSpec(load)
      val op = layer(load)
      ifp.addMicroOp(iwb, op)
      spec.signed match {
        case false => ifp.zeroExtend(iwb, op, spec.width)
        case true  => ifp.signExtend(iwb, op, spec.width)
      }
      withSpeculativeLoadFlush match {
        case true =>  op.mayFlushUpTo(forkAt)
        case false => op.dontFlushFrom(forkAt + 1)
      }
    }

    for(store <- frontend.stores){
      val op = layer(store)
      op.addRsSpec(RS2, forkAt)
      op.dontFlushFrom(forkAt+1)
    }

    layer.add(Rvi.FENCE) //TODO

    layer(Rvi.FENCE).setCompletion(joinAt)
    for(uop <- frontend.stores) layer(uop).setCompletion(joinAt)

    elp.uopLock.release()
    srcp.elaborationLock.release()
    ifp.elaborationLock.release()

    val injectCtrl = elp.ctrl(0)
    val inject = new injectCtrl.Area {
      SIZE := Decode.UOP(Const.funct3Range).asUInt
    }

    // Hardware elaboration
    val addressCtrl = elp.execute(addressAt)
    val forkCtrl = elp.execute(forkAt)
    val joinCtrl = elp.execute(joinAt)
    val wbCtrl = elp.execute(wbAt)

    val busParam = CachelessBusParam(addressWidth = Global.PHYSICAL_WIDTH, dataWidth = Riscv.LSLEN, hartIdWidth = Global.HART_ID_WIDTH)
    val bus = master(CachelessBus(busParam))

    val onAddress = new addressCtrl.Area{
      val RAW_ADDRESS = insert(srcp.ADD_SUB.asUInt)

      val translationPort = ats.newTranslationPort(
        nodes = Seq(forkCtrl.down),
        rawAddress = RAW_ADDRESS,
        allowRefill = insert(True),
        usage = AddressTranslationPortUsage.LOAD_STORE,
        portSpec = translationPortParameter,
        storageSpec = translationStorageParameter
      )
    }

    val onFork = new forkCtrl.Area{
      val tpk =  onAddress.translationPort.keys
      val RS2 = elp(IntRegFile, riscv.RS2)
      assert(bus.cmd.ready) // For now
      val cmdSent = RegInit(False) setWhen(bus.cmd.fire) clearWhen(isReady)
      bus.cmd.valid := isValid && SEL && !cmdSent && !hasCancelRequest
      bus.cmd.write := ! LOAD
      bus.cmd.address := tpk.TRANSLATED //TODO Overflow on TRANSLATED itself ?
      val mapping = (0 to log2Up(Riscv.LSLEN / 8)).map{size =>
        val w = (1 << size) * 8
        size -> up(RS2)(0, w bits).#*(Riscv.LSLEN / w)
      }
      bus.cmd.data := bus.cmd.size.muxListDc(mapping)
      bus.cmd.size := SIZE.resized
      bus.cmd.mask := AddressToMask(bus.cmd.address, bus.cmd.size, Riscv.LSLEN/8)
      bus.cmd.io := tpk.IO
      bus.cmd.hartId := Global.HART_ID
      assert(tpk.REDO === False)

      elp.freezeWhen(bus.cmd.isStall)

      val speculLoad = withSpeculativeLoadFlush generate new Area {
        val tooRisky = isValid && SEL && LOAD && tpk.IO && elp.atRiskOfFlush(forkAt)
        redoPort := tooRisky
      }
    }

    val onJoin = new joinCtrl.Area{
      val buffer = bus.rsp.toStream.queueLowLatency(joinAt-forkAt+1).combStage
      val READ_DATA = insert(buffer.data)
      elp.freezeWhen(isValid && SEL && !buffer.valid)
      buffer.ready := isReady && SEL
      assert(!(isValid && hasCancelRequest && SEL && !LOAD)) //TODO add tpk.IO and along the way
    }

    val onWb = new wbCtrl.Area{
      val rspSplits = onJoin.READ_DATA.subdivideIn(8 bits)
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


    ats.elaborationLock.release()
  }
}
