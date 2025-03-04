package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4Config
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.DebugId
import spinal.lib.bus.wishbone.WishboneConfig

case class CachelessBusParam(addressWidth : Int,
                             dataWidth : Int,
                             idCount : Int,
                             cmdPersistence : Boolean){
  val idWidth = log2Up(idCount)

  def toTilelinkM2s(name : Nameable) = new tilelink.M2sParameters(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    masters = List(
      new tilelink.M2sAgent(
        name = name,
        mapping = List(
          new tilelink.M2sSource(
            id = SizeMapping(0, idCount),
            emits = tilelink.M2sTransfers(
              get = tilelink.SizeRange(dataWidth/8)
            )
          )
        )
      )
    )
  )

  def toAxi4Config() = Axi4Config(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    idWidth = log2Up(idCount),
    useId = true,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useLen = false,
    useResp = true
  )

  def toWishboneConfig() = WishboneConfig(
    addressWidth = addressWidth - log2Up(dataWidth/8),
    dataWidth = dataWidth,
    selWidth = dataWidth/8,
    useSTALL = false,
    useLOCK = false,
    useERR = true,
    useRTY = false,
    tgaWidth = 0,
    tgcWidth = 0,
    tgdWidth = 0,
    useBTE = true,
    useCTI = true
  )

}

case class CachelessCmd(p : CachelessBusParam) extends Bundle{
  val id = UInt(p.idWidth bits)
  val address = UInt(p.addressWidth bits)
}

case class CachelessRsp(p : CachelessBusParam, withId : Boolean = true) extends Bundle{
  val id = withId generate UInt(p.idWidth bits)
  val error = Bool()
  val word  = Bits(p.dataWidth bits)
}

/**
 * The fetch CachelessBus has the following caracteristics :
 * - address in byte, always aligned on the full data width
 * - supports out of order responses via the id signals
 * - Only one transaction per id can be inflight at a given time
 */
case class CachelessBus(p : CachelessBusParam) extends Bundle with IMasterSlave {
  var cmd = Stream(CachelessCmd(p))
  var rsp = Flow(CachelessRsp(p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

