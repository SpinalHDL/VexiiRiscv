package vexiiriscv.memory

import spinal.core._
import spinal.lib._

case class CoupledBusParam(addressWidth : Int,
                           dataWidth : Int,
                           canRead : Boolean,
                           canWrite : Boolean,
                           readLatency : Int)

case class CoupledBusCmd(p : CoupledBusParam) extends Bundle  {
  val address = UInt(p.addressWidth bits)
  val write = Bool()
  val data = p.canWrite generate Bits(p.dataWidth bits)
  val mask = p.canWrite generate Bits(p.dataWidth/8 bits)

  override def asMaster(): Unit = {
    out(enable, address)
    in(data)
  }
}

case class CoupledBusRsp(p : CoupledBusParam) extends Bundle  {
  val data = p.canRead generate Bits(p.dataWidth bits)

  override def asMaster(): Unit = {
    out(enable, address)
    in(data)
  }
}


case class CoupledBus(p : CoupledBusParam) extends Bundle with IMasterSlave {
  val enable = Bool()
  val cmd = Stream(CoupledBusCmd(p))
  val rsp = CoupledBusRsp(p)

  override def asMaster(): Unit = {
    out(enable, cmd)
    in(rsp)
  }
}