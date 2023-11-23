package vexiiriscv.regfile

import spinal.core._
import spinal.core.fiber.Lockable
import spinal.lib._
import vexiiriscv.riscv.RegfileSpec

case class RegFilePortParam(addressWidth: Int,
                            dataWidth: Int,
                            hartIdWidth : Int,
                            uopIdWidth : Int)

case class RegFileIo( rfpp : RegFilePortParam,
                      readsParameter: Seq[RegFileReadParameter],
                      writesParameter: Seq[RegFileWriteParameter]
                    ) extends Bundle{
  val writes = Vec(writesParameter.map(p => slave(RegFileWrite(rfpp, p.withReady))))
  val reads = Vec(readsParameter.map(p => slave(RegFileRead(rfpp, p.withReady))))
//  val bypasses = Vec.fill(bypasseCount)(slave(RegFileBypass(addressWidth, dataWidth)))
}

case class RegFileWrite(rfpp : RegFilePortParam, withReady : Boolean) extends Bundle with IMasterSlave {
  import rfpp._
  val valid = Bool()
  val ready = withReady generate Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)
  val hartId = UInt(hartIdWidth bits)
  val uopId = UInt(uopIdWidth bits)

  def fire = if(withReady) valid && ready else valid

  def asWithoutReady() = {
    val ret = RegFileWrite(rfpp, false)
    ret.valid := this.fire
    ret.address := this.address
    ret.data := this.data
    ret.hartId := this.hartId
    ret.uopId := this.uopId
    ret
  }

  override def asMaster() = {
    out(valid, address, data, hartId, uopId)
    in(ready)
  }
}

case class RegFileRead(rfpp : RegFilePortParam, withReady : Boolean) extends Bundle with IMasterSlave{
  import rfpp._
  val valid = Bool()
  val ready = withReady generate Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)

  override def asMaster() = {
    out(valid, address)
    in(ready, data)
  }
}

//case class RegFileBypass(addressWidth : Int, dataWidth : Int, priority : Int) extends Bundle with IMasterSlave{
//  val valid = Bool()
//  val address = UInt(addressWidth bits)
//  val data = Bits(dataWidth bits)
//
//  override def asMaster() = {
//    out(valid, address, data)
//  }
//}

trait RegfileService extends Lockable {
  def rfSpec : RegfileSpec
  def getPhysicalDepth : Int

  def writeLatency : Int
  def readLatency : Int

  def newRead(withReady : Boolean) : RegFileRead
  def newWrite(withReady : Boolean, sharingKey : Any = null, priority : Int = 0) : RegFileWrite
//  def newBypass(priority : Int) : RegFileBypass

  def getWrites() : Seq[RegFileWrite]
}

