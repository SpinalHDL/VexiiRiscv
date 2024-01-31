package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.DebugId

case class CachelessBusParam(addressWidth : Int, dataWidth : Int, idCount : Int, cmdPersistence : Boolean){
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

case class CachelessBus(p : CachelessBusParam) extends Bundle with IMasterSlave {
  var cmd = Stream(CachelessCmd(p))
  var rsp = Flow(CachelessRsp(p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

