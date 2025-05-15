package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.{Global, riscv}
import vexiiriscv.riscv.{CSR, Const, IntRegFile, MicroOp, RS1, RS2, Riscv, Rvi}
import AguPlugin._
import spinal.core.fiber.Retainer
import spinal.lib.bus.amba4.axi.Axi4Config
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.DebugId
import spinal.lib.bus.wishbone.WishboneConfig
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

object LsuCachelessBusAmo {
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

case class LsuCachelessBusParam(addressWidth : Int, dataWidth : Int, hartIdWidth : Int, uopIdWidth : Int, withAmo : Boolean, pendingMax : Int){
  def toTilelinkM2s(name: Nameable) = {
    assert(!withAmo)
    new tilelink.M2sParameters(
      addressWidth = addressWidth,
      dataWidth = dataWidth,
      masters = List(
        new tilelink.M2sAgent(
          name = name,
          mapping = List(
            new tilelink.M2sSource(
              id = SizeMapping(0, pendingMax),
              emits = tilelink.M2sTransfers(
                get = tilelink.SizeRange(1, dataWidth / 8),
                putFull = tilelink.SizeRange(1, dataWidth / 8)
              )
            )
          )
        )
      )
    )
  }

  def toAxi4Config() = Axi4Config(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    idWidth = log2Up(pendingMax),
    useId = true,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useLen = false,
    useResp = true
  )

  def toWishboneConfig() = WishboneConfig(
    addressWidth = addressWidth-log2Up(dataWidth/8),
    dataWidth = dataWidth,
    selWidth = dataWidth/8,
    useSTALL = false,
    useLOCK = false,
    useERR = true,
    useRTY = false,
    useBTE = true,
    useCTI = true
  )
}

/**
 * VexiiRiscv native memory bus used for uncached and IO memory accesses. It it meant to be easily bridged to other memory busses.
 * Responses are out of order and can be reordered via the id field.
 * It also supports AMO/LR/SC atomics
 */
case class LsuCachelessCmd(p : LsuCachelessBusParam) extends Bundle {
  val id = UInt(log2Up(p.pendingMax) bits) // Unique identifier, only one inflight transaction per ID is allowed
  val write = Bool()
  val address = UInt(p.addressWidth bits)
  val data = Bits(p.dataWidth bit)
  val size = UInt(log2Up(log2Up(p.dataWidth / 8) + 1) bits)
  val mask = Bits(p.dataWidth / 8 bits)
  val amoEnable = p.withAmo generate Bool()
  val amoOp = p.withAmo generate Bits(5 bits) // See LsuCachelessBusAmo

  // Signals for verification purposes, allowing RVLS to track stuff
  val io = Bool()
  val fromHart = Bool()
  val uopId = UInt(p.uopIdWidth bits)
  val hartId = UInt(p.hartIdWidth bits)
}

case class LsuCachelessRsp(p : LsuCachelessBusParam, withId : Boolean = true) extends Bundle{
  val id = withId generate UInt(log2Up(p.pendingMax) bits)
  val error = Bool()
  val data  = Bits(p.dataWidth bits)
  val scMiss = p.withAmo generate Bool()
}

case class LsuCachelessBus(p : LsuCachelessBusParam) extends Bundle with IMasterSlave {
  var cmd = Stream(LsuCachelessCmd(p))
  var rsp = Flow(LsuCachelessRsp(p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}
