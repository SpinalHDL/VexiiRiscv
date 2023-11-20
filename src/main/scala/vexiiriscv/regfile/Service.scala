package vexiiriscv.regfile

import spinal.core._
import spinal.core.fiber.Lockable
import spinal.lib._
import vexiiriscv.riscv.RegfileSpec

case class RegFileIo( addressWidth: Int,
                      dataWidth: Int,
                      readsParameter: Seq[RegFileReadParameter],
                      writesParameter: Seq[RegFileWriteParameter]
                    ) extends Bundle{
  val writes = Vec(writesParameter.map(p => slave(RegFileWrite(addressWidth, dataWidth, p.withReady))))
  val reads = Vec(readsParameter.map(p => slave(RegFileRead(addressWidth, dataWidth, p.withReady))))
//  val bypasses = Vec.fill(bypasseCount)(slave(RegFileBypass(addressWidth, dataWidth)))
}

case class RegFileWrite(addressWidth : Int, dataWidth : Int, withReady : Boolean) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = withReady generate Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)

  def fire = if(withReady) valid && ready else valid

  def asWithoutReady() = {
    val ret = RegFileWrite(addressWidth, dataWidth, false)
    ret.valid := this.fire
    ret.address := this.address
    ret.data := this.data
    ret
  }

  override def asMaster() = {
    out(valid, address, data)
    in(ready)
  }
}

case class RegFileRead(addressWidth : Int, dataWidth : Int, withReady : Boolean) extends Bundle with IMasterSlave{
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

