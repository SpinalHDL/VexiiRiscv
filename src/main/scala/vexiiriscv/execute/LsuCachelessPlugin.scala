package vexiiriscv.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.{Global, riscv}
import vexiiriscv.riscv.{Const, IntRegFile, MicroOp, RS1, Riscv, Rvi}
import AguPlugin._
import vexiiriscv.decode.Decode
import vexiiriscv.misc.AddressToMask
import vexiiriscv.riscv.Riscv.{LSLEN, XLEN}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class CachelessBusParam(addressWidth : Int, dataWidth : Int){

}

case class CachelessCmd(p : CachelessBusParam) extends Bundle{
  val write = Bool()
  val address = UInt(p.addressWidth bits)
  val data = Bits(p.dataWidth bit)
  val size = UInt(log2Up(log2Up(p.dataWidth / 8) + 1) bits)
  val mask = Bits(p.dataWidth / 8 bits)
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

class LsuCachelessPlugin(var laneName : String,
                         var forkAt: Int = 0,
                         var joinAt: Int = 1,
                         var wbAt: Int = 2) extends FiberPlugin{
  lazy val elp = host.find[ExecuteLanePlugin](_.laneName == laneName)
  lazy val ifp = host.find[IntFormatPlugin](_.laneName == laneName)
  lazy val srcp = host.find[SrcPlugin](_.laneName == laneName)
  buildBefore(elp.pipelineLock)
  setupRetain(elp.uopLock)
  setupRetain(srcp.elaborationLock)
  setupRetain(ifp.elaborationLock)

  val logic = during build new Area{
    val frontend = new AguFrontend(laneName, host)

    // IntFormatPlugin specification
    val iwb = ifp.access(wbAt)
    for(load <- frontend.loads){
      val spec = Rvi.loadSpec(load)
      ifp.addMicroOp(iwb, load)
      spec.signed match {
        case false => ifp.zeroExtend(iwb, load, spec.width)
        case true  => ifp.signExtend(iwb, load, spec.width)
      }
    }

    elp.addMicroOp(Rvi.FENCE) //TODO

    elp.setCompletion(joinAt, List(Rvi.FENCE))
    elp.setCompletion(joinAt, frontend.stores)

    elp.uopLock.release()
    srcp.elaborationLock.release()
    ifp.elaborationLock.release()

    val injectCtrl = elp.ctrl(0)
    val inject = new injectCtrl.Area {
      SIZE := Decode.UOP(Const.funct3Range).asUInt
    }

    // Hardware elaboration
    val forkCtrl = elp.execute(forkAt)
    val joinCtrl = elp.execute(joinAt)
    val wbCtrl = elp.execute(wbAt)

    val busParam = CachelessBusParam(addressWidth = Global.PHYSICAL_WIDTH, dataWidth = Riscv.LSLEN)
    val bus = master(CachelessBus(busParam))
    val onFork = new forkCtrl.Area{
      val RS2 = elp(IntRegFile, riscv.RS2)
      assert(bus.cmd.ready) // For now
      bus.cmd.valid := isValid && SEL && !hasCancelRequest
      bus.cmd.write := ! LOAD
      bus.cmd.address := SrcStageables.ADD_SUB.asUInt //TODO Overflow ?
      val mapping = (0 to log2Up(Riscv.LSLEN / 8)).map{size =>
        val w = (1 << size) * 8
        size -> RS2(0, w bits).#*(Riscv.LSLEN / w)
      }
      bus.cmd.data := bus.cmd.size.muxListDc(mapping)
      bus.cmd.size := SIZE.resized
      bus.cmd.mask := AddressToMask(bus.cmd.address, bus.cmd.size, Riscv.LSLEN/8)
    }

    val onJoin = new joinCtrl.Area{
      when(isValid && SEL){
        assert(!(LOAD && !bus.rsp.valid)) //For now
      }

      val READ_DATA = insert(bus.rsp.data)
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
        val sel = SrcStageables.ADD_SUB(range).asUInt
        rspShifted(i * 8, 8 bits) := src.read(sel)
      }

      iwb.valid := SEL
      iwb.payload := rspShifted
    }
  }
}
