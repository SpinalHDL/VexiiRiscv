package vexiiriscv.fetch

import spinal.lib.misc.plugin.FiberPlugin



import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4ReadOnly}
import spinal.lib.bus.tilelink
import spinal.lib.bus.amba4.axilite.{AxiLite4Config, AxiLite4ReadOnly}
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbParameter, BmbSourceParameter}
import spinal.lib.bus.tilelink.{M2sSupport, SizeRange}
import spinal.lib.misc.Plru

import scala.collection.mutable.ArrayBuffer



case class FetchL1Cmd(physicalWidth : Int, refillCount : Int) extends Bundle{
  val id      = UInt(log2Up(refillCount) bits)
  val address = UInt(physicalWidth bits)
  val io      = Bool()
}

case class FetchL1Rsp(dataWidth : Int, refillCount : Int) extends Bundle{
  val id    = UInt(log2Up(refillCount) bits)
  val data  = Bits(dataWidth bits)
  val error = Bool()
}


case class FetchL1BusParam(physicalWidth : Int,
                           dataWidth : Int,
                           lineSize : Int,
                           refillCount : Int,
                           withBackPresure : Boolean){
  def toTileLinkM2sParameters(name : Nameable) = tilelink.M2sParameters(
    sourceCount = refillCount,
    support = M2sSupport(
      addressWidth = physicalWidth,
      dataWidth = dataWidth,
      transfers = tilelink.M2sTransfers(get = SizeRange(lineSize))
    ),
    name = name
  )
}

case class FetchL1Bus(p : FetchL1BusParam) extends Bundle with IMasterSlave {
  import p._

  val cmd = Stream(FetchL1Cmd(physicalWidth, refillCount))
  val rsp = Stream(FetchL1Rsp(dataWidth, refillCount))

  def beatCount = lineSize*8/dataWidth
  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }

  def split(selGen : FetchL1Cmd => Bool) : (FetchL1Bus, FetchL1Bus) = new Composite(this, "split"){
    val bus0, bus1 = cloneOf(self)
    val selCmd = selGen(cmd)
    val selRsp = RegNextWhen(selCmd, cmd.valid)

    bus1.cmd.valid := cmd.valid && selCmd
    bus0.cmd.valid := cmd.valid && !selCmd

    bus1.cmd.payload  := cmd.payload
    bus0.cmd.payload := cmd.payload

    cmd.ready := selCmd ? bus1.cmd.ready | bus0.cmd.ready

    rsp.valid := bus1.rsp.valid || bus0.rsp.valid
    rsp.payload := selRsp ? bus1.rsp.payload | bus0.rsp.payload
    bus1.rsp.ready := rsp.ready
    bus0.rsp.ready := rsp.ready

    val ret = (bus0, bus1)
  }.ret

  def ioSplit() : (FetchL1Bus, FetchL1Bus) = split(!_.io)



  def resizer(newDataWidth : Int) : FetchL1Bus = new Composite(this, "resizer"){
    val ret = FetchL1Bus(
      p.copy(
        withBackPresure = withBackPresure || newDataWidth > dataWidth
      )
    )

    ret.cmd << self.cmd

    val rspOutputStream = Stream(Bits(dataWidth bits))
    StreamWidthAdapter(ret.rsp.translateWith(ret.rsp.data), rspOutputStream)

    rsp.valid := rspOutputStream.valid
    rsp.data  := rspOutputStream.payload
    rsp.error := ret.rsp.error
    rspOutputStream.ready :=  (if(withBackPresure) rsp.ready else True)
  }.ret

  def toAxi4(): Axi4ReadOnly = new Composite(this, "toAxi4"){
    val axiConfig = Axi4Config(
      addressWidth = physicalWidth,
      dataWidth    = dataWidth,
      idWidth      = 0,
      useId        = true,
      useRegion    = false,
      useBurst     = true,
      useLock      = false,
      useCache     = false,
      useSize      = true,
      useQos       = false,
      useLen       = true,
      useLast      = true,
      useResp      = true,
      useProt      = true,
      useStrb      = false
    )

    val axi = Axi4ReadOnly(axiConfig)
    axi.ar.valid := cmd.valid
    axi.ar.addr  := cmd.address
    axi.ar.id    := 0
    axi.ar.prot  := B"110"
    axi.ar.len   := lineSize*8/dataWidth-1
    axi.ar.size  := log2Up(dataWidth/8)
    axi.ar.setBurstINCR()
    cmd.ready := axi.ar.ready

    rsp.valid := axi.r.valid
    rsp.data  := axi.r.data
    rsp.error := !axi.r.isOKAY()
    axi.r.ready := (if(withBackPresure) rsp.ready else True)
  }.axi


  def toBmb(): Bmb = new Composite(this, "toBmb"){
    val bmbConfig = BmbAccessParameter(
      addressWidth = physicalWidth,
      dataWidth    = dataWidth
    ).addSources(1, BmbSourceParameter(
      contextWidth     = 0,
      lengthWidth      = log2Up(lineSize),
      alignment        = BmbParameter.BurstAlignement.LENGTH,
      canWrite         = false,
      withCachedRead   = true
    ))

    val bmb = Bmb(bmbConfig)
    bmb.cmd.arbitrationFrom(cmd)
    bmb.cmd.setRead()
    bmb.cmd.address := cmd.address
    bmb.cmd.length := lineSize-1
    bmb.cmd.last := True


    rsp.arbitrationFrom(bmb.rsp)
    rsp.data  := bmb.rsp.data
    rsp.error := bmb.rsp.isError
  }.bmb

  def toTilelink(): tilelink.Bus = new Composite(this, "toTilelink"){
    val bus = tilelink.Bus(p.toTileLinkM2sParameters(FetchL1Bus.this))
    bus.a.valid := cmd.valid
    bus.a.opcode  := tilelink.Opcode.A.GET
    bus.a.param   := 0
    bus.a.source  := cmd.id
    bus.a.address := cmd.address
    bus.a.size    := log2Up(lineSize)
    cmd.ready := bus.a.ready

    rsp.valid := bus.d.valid
    rsp.id    := bus.d.source
    rsp.data  := bus.d.data
    rsp.error := bus.d.denied || bus.d.corrupt
    bus.d.ready := (if(withBackPresure) rsp.ready else True)
  }.bus

  def toAxiLite4(): AxiLite4ReadOnly = new Composite(this, "toAxi4"){
    val axiConfig = AxiLite4Config(
      addressWidth = physicalWidth,
      dataWidth    = dataWidth
    )

    val counter = Reg(UInt(log2Up(lineSize*8/dataWidth) bits)) init(0)
    val last = counter.andR

    val axi = AxiLite4ReadOnly(axiConfig)
    axi.ar.valid := cmd.valid
    axi.ar.addr  := cmd.address | (counter << log2Up(dataWidth/8)).resized
    axi.ar.setUnprivileged
    cmd.ready := axi.ar.ready && last
    when(axi.ar.fire){
      counter := counter + 1
    }

    rsp.valid := axi.r.valid
    rsp.data  := axi.r.data
    rsp.error := !axi.r.isOKAY()
    axi.r.ready := True
  }.axi

}