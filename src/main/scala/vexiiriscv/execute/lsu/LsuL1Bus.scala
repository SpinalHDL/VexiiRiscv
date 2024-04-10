package vexiiriscv.execute

import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbParameter, BmbSourceParameter}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib._

case class LsuL1BusParameter( addressWidth: Int,
                                dataWidth: Int,
                                readIdCount : Int,
                                writeIdCount : Int,
                                probeIdWidth : Int,
                                ackIdWidth : Int,
                                lineSize: Int,
                                withReducedBandwidth : Boolean,
                                withCoherency : Boolean){

  val readIdWidth = log2Up(readIdCount)
  val writeIdWidth = log2Up(writeIdCount)

  def toTileLinkM2sParameters(name : Nameable) = {
    val masters = withCoherency match {
      case false => List(
        M2sAgent(
          name = name,
          M2sSource(
            id    = SizeMapping(0, writeIdCount),
            emits = tilelink.M2sTransfers(
              putFull = SizeRange(lineSize)
            )
          )
        ),
        M2sAgent(
          name = name,
          M2sSource(
            id    = SizeMapping(1 << log2Up(readIdCount max writeIdCount), readIdCount),
            emits = tilelink.M2sTransfers(
              get = SizeRange(lineSize)
            )
          )
        )
      )
      case true => List(
        M2sAgent(
          name = name,
          M2sSource(
            id    = SizeMapping(0, readIdCount),
            emits = tilelink.M2sTransfers(
              acquireB = SizeRange(lineSize),
              acquireT = SizeRange(lineSize)
            )
          )
        )
      )
    }

    tilelink.M2sParameters(
      addressWidth = addressWidth,
      dataWidth = dataWidth,
      masters = masters
    )
  }
}

case class LsuL1ReadCmd(p : LsuL1BusParameter) extends Bundle {
  val id = UInt(p.readIdWidth bits)
  val address = UInt(p.addressWidth bits)
  val unique = p.withCoherency generate Bool()
  val data = p.withCoherency generate Bool()
}

case class LsuL1ReadRsp(p : LsuL1BusParameter) extends Bundle {
  val id = UInt(p.readIdWidth bits)
  val data = Bits(p.dataWidth bits)
  val error = Bool()
  val unique = p.withCoherency generate Bool()
  val ackId =  p.withCoherency generate UInt(p.ackIdWidth bits)
  val withData = p.withCoherency generate Bool()
}

case class LsuL1ReadAck(p : LsuL1BusParameter) extends Bundle {
  val ackId = UInt(p.ackIdWidth bits)
}

case class LsuL1ReadBus(p : LsuL1BusParameter) extends Bundle with IMasterSlave {
  val cmd = Stream(LsuL1ReadCmd(p))
  val rsp = Stream(LsuL1ReadRsp(p))
  val ack = p.withCoherency generate Stream(LsuL1ReadAck(p))

  override def asMaster() = {
    master(cmd, ack)
    slave(rsp)
  }

  def <<(m : LsuL1ReadBus): Unit ={
    m.cmd >> this.cmd
    m.rsp << this.rsp
  }

  def resizer(newDataWidth : Int) : LsuL1ReadBus = new Composite(this, "resizer"){
    val ret = LsuL1ReadBus(
      p = p.copy(
        dataWidth = newDataWidth,
        withReducedBandwidth = p.withReducedBandwidth || newDataWidth > p.dataWidth
      )
    )

    ret.cmd << self.cmd

    val rspOutputStream = Stream(Bits(p.dataWidth bits))
    StreamWidthAdapter(ret.rsp.translateWith(ret.rsp.data), rspOutputStream)

    rsp.valid := rspOutputStream.valid
    rsp.data  := rspOutputStream.payload
    rsp.id    := ret.rsp.id
    rsp.error := ret.rsp.error
    rspOutputStream.ready :=  (if(p.withReducedBandwidth) rspOutputStream.ready else True)
  }.ret

  def toBmb(): Bmb = new Composite(this, "toBmb"){
    val bmbConfig = BmbAccessParameter(
      addressWidth = p.addressWidth,
      dataWidth    = p.dataWidth
    ).addSources(p.readIdCount, BmbSourceParameter(
      contextWidth     = 0,
      lengthWidth      = log2Up(p.lineSize),
      alignment        = BmbParameter.BurstAlignement.LENGTH,
      canWrite         = false,
      withCachedRead   = true
    ))

    val bmb = Bmb(bmbConfig)
    bmb.cmd.arbitrationFrom(cmd)
    bmb.cmd.setRead()
    bmb.cmd.address := cmd.address
    bmb.cmd.length := p.lineSize-1
    bmb.cmd.source := cmd.id
    bmb.cmd.last := True

    rsp.arbitrationFrom(bmb.rsp)
    rsp.id    := bmb.rsp.source
    rsp.data  := bmb.rsp.data
    rsp.error := bmb.rsp.isError
  }.bmb

}

case class LsuL1WriteCmd(p : LsuL1BusParameter) extends Bundle {
  val address = UInt(p.addressWidth bits)
  val data    = Bits(p.dataWidth bits)
  val id      = UInt(p.writeIdWidth bits)
  val coherent = p.withCoherency generate new Bundle{
    val release = Bool() //else from probe
    val dirty = Bool() //Meaning with data
    val fromUnique = Bool()
    val toUnique = Bool()
    val toShared = Bool()
    val probeId = UInt(p.probeIdWidth bits)
  }
}

case class LsuL1WriteRsp(p : LsuL1BusParameter) extends Bundle {
  val error = Bool()
  val id = UInt(p.writeIdWidth bits)
}

case class LsuL1WriteBus(p : LsuL1BusParameter) extends Bundle with IMasterSlave {
  val cmd = Stream(Fragment(LsuL1WriteCmd(p)))
  val rsp = Flow(LsuL1WriteRsp(p))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }


  def <<(m : LsuL1WriteBus): Unit ={
    m.cmd >> this.cmd
    m.rsp << this.rsp
  }

  def resizer(newDataWidth : Int) : LsuL1WriteBus = new Composite(this, "resizer"){
    val ret = LsuL1WriteBus(
      p = p.copy(
        dataWidth = newDataWidth,
        withReducedBandwidth = p.withReducedBandwidth || newDataWidth > p.dataWidth
      )
    )

    val cmdOutputStream = Stream(Fragment(Bits(newDataWidth bits)))
    StreamFragmentWidthAdapter(cmd.translateWith(cmd.data).addFragmentLast(cmd.last), cmdOutputStream)

    ret.cmd.arbitrationFrom(cmdOutputStream)
    ret.cmd.id      := self.cmd.id
    ret.cmd.address := self.cmd.address
    ret.cmd.data    := cmdOutputStream.fragment
    ret.cmd.last    := cmdOutputStream.last

    self.rsp << ret.rsp
  }.ret


  def toBmb(): Bmb = new Composite(this, "toBmb"){
    val bmbConfig = BmbAccessParameter(
      addressWidth = p.addressWidth,
      dataWidth    = p.dataWidth
    ).addSources(p.readIdCount, BmbSourceParameter(
      contextWidth     = 0,
      lengthWidth      = log2Up(p.lineSize),
      alignment        = BmbParameter.BurstAlignement.LENGTH,
      canRead         = false,
      withCachedRead   = true
    ))

    val bmb = Bmb(bmbConfig)
    bmb.cmd.arbitrationFrom(cmd)
    bmb.cmd.setWrite()
    bmb.cmd.address := cmd.address
    bmb.cmd.length := p.lineSize-1
    bmb.cmd.source := cmd.id
    bmb.cmd.data := cmd.data
    bmb.cmd.last := cmd.last
    bmb.cmd.mask.setAll()


    bmb.rsp.ready := True
    rsp.valid := bmb.rsp.valid
    rsp.id    := bmb.rsp.source
    rsp.error := bmb.rsp.isError
  }.bmb
}


case class LsuL1ProbeCmd(p : LsuL1BusParameter) extends Bundle {
  val address = UInt(p.addressWidth bits)
  val id = UInt(p.probeIdWidth bits)
  val allowUnique = Bool()
  val allowShared = Bool()
  val getDirtyData = Bool()
}

case class LsuL1ProbeRsp(p : LsuL1BusParameter, fromProbe : Boolean) extends Bundle {
  val id = UInt(p.probeIdWidth bits)
  val address = UInt(p.addressWidth bits)
  val fromUnique, fromShared = Bool()
  val toShared, toUnique = Bool()
  val allowShared, allowUnique, getDirtyData = Bool() //Used for redo
  val redo = fromProbe generate Bool()
  val writeback = fromProbe generate Bool()

  def assignTilelinkC(c : ChannelC) = {
    c.opcode := tilelink.Opcode.C.PROBE_ACK()
    c.param := Param.report(
      fromUnique,
      fromShared,
      toUnique,
      toShared
    )
    c.source := id
    c.address := address
    c.size := log2Up(p.lineSize)
    c.data.assignDontCare()
    c.corrupt.assignDontCare()
  }
}


case class LsuL1ProbeBus(p : LsuL1BusParameter) extends Bundle with IMasterSlave {
  val cmd = Flow(LsuL1ProbeCmd(p))
  val rsp = Flow(LsuL1ProbeRsp(p, true))
  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }


  def <<(m : LsuL1ProbeBus): Unit ={
    m.cmd >> this.cmd
    m.rsp << this.rsp
  }
}



case class LsuL1Bus(p : LsuL1BusParameter) extends Bundle with IMasterSlave {
  val read = LsuL1ReadBus(p)
  val write = LsuL1WriteBus(p)
  val probe = p.withCoherency generate LsuL1ProbeBus(p)

  override def asMaster() = {
    master(read, write)
    slave(probe)
  }

  def resizer(newDataWidth : Int) : LsuL1Bus = new Composite(this, "resizer") {
    val ret = LsuL1Bus(
      p = p.copy(
        dataWidth = newDataWidth,
        withReducedBandwidth = p.withReducedBandwidth || newDataWidth > p.dataWidth
      )
    )

    ret.read << read.resizer(newDataWidth)
    ret.write << write.resizer(newDataWidth)

  }.ret


  def toAxi4(): Axi4 = new Composite(this, "toAxi4"){
    assert(!p.withCoherency)
    val idWidth = p.readIdWidth max p.writeIdWidth

    val axiConfig = Axi4Config(
      addressWidth = p.addressWidth,
      dataWidth    = p.dataWidth,
      idWidth      = idWidth,
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
      useStrb      = true
    )

    val axi = Axi4(axiConfig)

    //READ
    axi.ar.valid := read.cmd.valid
    axi.ar.addr  := read.cmd.address
    axi.ar.id    := read.cmd.id
    axi.ar.prot  := B"010"
    axi.ar.len   := p.lineSize*8/p.dataWidth-1
    axi.ar.size  := log2Up(p.dataWidth/8)
    axi.ar.setBurstINCR()
    read.cmd.ready := axi.ar.ready

    read.rsp.valid := axi.r.valid
    read.rsp.data  := axi.r.data
    read.rsp.id    := axi.r.id
    read.rsp.error := !axi.r.isOKAY()
    axi.r.ready    := (if(p.withReducedBandwidth) read.rsp.ready else True)

    //WRITE
    val (awRaw, wRaw) = StreamFork2(write.cmd)
    val awFiltred = awRaw.throwWhen(!awRaw.first)
    val aw = awFiltred.stage()
    axi.aw.valid := aw.valid
    axi.aw.addr  := aw.address
    axi.aw.id    := aw.id
    axi.aw.prot  := B"010"
    axi.aw.len   := p.lineSize*8/p.dataWidth-1
    axi.aw.size  := log2Up(p.dataWidth/8)
    axi.aw.setBurstINCR()
    aw.ready := axi.aw.ready

    val w = wRaw.haltWhen(awFiltred.valid)
    axi.w.valid := w.valid
    axi.w.data  := w.data
    axi.w.strb.setAll()
    axi.w.last  := w.last
    w.ready := axi.w.ready

    write.rsp.valid :=  axi.b.valid
    write.rsp.id    :=  axi.b.id
    write.rsp.error := !axi.b.isOKAY()
    axi.b.ready     :=  True
  }.axi



  def toTilelink(probeInflightMax : Int = 2): tilelink.Bus = new Composite(this, "toTilelink"){
    val m2s = p.toTileLinkM2sParameters(null)
    val bus = tilelink.Bus(
      BusParameter(
        addressWidth = m2s.addressWidth,
        dataWidth    = m2s.dataWidth,
        sizeBytes    = m2s.sizeBytes,
        sourceWidth  = m2s.sourceWidth,
        sinkWidth    = p.ackIdWidth,
        withBCE      = p.withCoherency,
        withDataA    = !p.withCoherency,
        withDataB    = false,
        withDataD    = true,
        withDataC    = true,
        node         = new NodeParameters(
          m2s
        )
      )
    )

    val nonCoherent = !p.withCoherency generate new Area{
      val onA = new Area {
        val lock = RegInit(False) setWhen (bus.a.valid) clearWhen (bus.a.fire && bus.a.isLast())
        val selReg = Reg(Bool())
        val sel = lock.mux(selReg, read.cmd.valid)
        selReg := sel

        bus.a.param := 0
        bus.a.size := log2Up(p.lineSize)
        bus.a.mask.setAll()
        bus.a.data := write.cmd.data
        bus.a.corrupt := False

        when(sel) {
          bus.a.valid := read.cmd.valid
          bus.a.opcode := tilelink.Opcode.A.GET()
          bus.a.source := read.cmd.id.resized
          bus.a.address := read.cmd.address
        } otherwise {
          bus.a.valid := write.cmd.valid
          bus.a.opcode := tilelink.Opcode.A.PUT_FULL_DATA()
          bus.a.source := write.cmd.id.resized
          bus.a.address := write.cmd.address
        }

        val beat = bus.a.beatCounter()
        bus.a.address(log2Up(p.dataWidth/8), widthOf(beat) bits) := beat
        bus.a.source.allowOverride()
        bus.a.source.msb := sel

        write.cmd.ready := !sel && bus.a.ready
        read.cmd.ready := sel && bus.a.ready
      }

      val onD = new Area{
        val sel = bus.d.source.msb

        read.rsp.valid := bus.d.valid && sel
        read.rsp.data  := bus.d.data
        read.rsp.error := bus.d.denied || bus.d.corrupt
        read.rsp.id    := bus.d.source.resized

        write.rsp.valid := bus.d.valid && !sel
        write.rsp.error := bus.d.denied || bus.d.corrupt
        write.rsp.id    := bus.d.source.resized

        bus.d.ready := sel.mux(read.rsp.ready, True)
      }
    }


    val coherent = p.withCoherency generate new Area{
      val onA = new Area{
        bus.a.arbitrationFrom(read.cmd)
        bus.a.opcode := tilelink.Opcode.A.ACQUIRE_BLOCK
        bus.a.param   := tilelink.Param.Grow(read.cmd.data, read.cmd.unique)
        bus.a.source  := read.cmd.id.resized
        bus.a.address := read.cmd.address
        bus.a.size    := log2Up(p.lineSize)
      }

      val onB = new Area{
        val haltIt = False
        probe.cmd.valid        := bus.b.valid
        probe.cmd.address      := bus.b.address
        probe.cmd.id           := bus.b.source
        probe.cmd.allowUnique  := bus.b.param === Param.Cap.toT
        probe.cmd.allowShared  := bus.b.param =/= Param.Cap.toN
        probe.cmd.getDirtyData := bus.b.opcode === Opcode.B.PROBE_BLOCK
        bus.b.ready := True
      }

      val onC = new Area{
        val rspFifo = StreamFifo(probe.rsp.payloadType, probeInflightMax, latency = 1)
        val inflight = Reg(UInt(log2Up(probeInflightMax+1) bits)) init(0)
        inflight := inflight + U(bus.b.fire) - U(rspFifo.io.pop.fire) - U(probe.rsp.fire && probe.rsp.writeback && !probe.rsp.redo)
        val full = inflight === probeInflightMax
        val rspFlow = probe.rsp.throwWhen(!probe.rsp.redo && probe.rsp.writeback)
        val rspStream = rspFlow.takeWhen(!rspFlow.redo).toStream
        assert(!rspStream.isStall)
        when(full){
          probe.cmd.valid := False
          bus.b.ready := False
        }
        rspFifo.io.push << rspStream

        //TODO NAX FIX
        when(rspFlow.valid && rspFlow.redo) {
          probe.cmd.valid := True
          probe.cmd.address := probe.rsp.address
          probe.cmd.id := probe.rsp.id
          probe.cmd.allowUnique := probe.rsp.allowUnique
          probe.cmd.allowShared := probe.rsp.allowShared
          probe.cmd.getDirtyData := probe.rsp.getDirtyData
          bus.b.ready := False
        }

        val arbiter = StreamArbiterFactory().lambdaLock[ChannelC](_.isLast()).roundRobin.build(bus.c.payloadType, 2)
        val i0 = arbiter.io.inputs(0)
        i0.arbitrationFrom(write.cmd)
        i0.opcode := write.cmd.coherent.release mux(
          write.cmd.coherent.dirty mux(
            tilelink.Opcode.C.RELEASE_DATA(),
            tilelink.Opcode.C.RELEASE()
          ),
          tilelink.Opcode.C.PROBE_ACK_DATA()
        )
        i0.param := Param.report(
          write.cmd.coherent.fromUnique,
          !write.cmd.coherent.fromUnique,
          write.cmd.coherent.toUnique,
          write.cmd.coherent.toShared
        )
        i0.source := write.cmd.coherent.release.mux(
          write.cmd.id,
          write.cmd.coherent.probeId
        )

        i0.address := write.cmd.address
        i0.size := log2Up(p.lineSize)
        i0.data := write.cmd.data
        i0.corrupt := False

        val i1 = arbiter.io.inputs(1)
        i1.arbitrationFrom(rspFifo.io.pop)
        rspFifo.io.pop.assignTilelinkC(i1)

        val beat = bus.c.beatCounter()
        bus.c << arbiter.io.output
        bus.c.address(log2Up(p.dataWidth/8), widthOf(beat) bits) := beat
      }


      val onD = new Area{
        val sel = tilelink.Opcode.D.fromA(bus.d.opcode)

        read.rsp.valid  := bus.d.valid && sel
        read.rsp.data   := bus.d.data
        read.rsp.error  := bus.d.denied || bus.d.corrupt
        read.rsp.id     := bus.d.source.resized
        read.rsp.unique := bus.d.param === Param.Cap.toT
        read.rsp.ackId  := bus.d.sink
        read.rsp.withData := bus.d.opcode === Opcode.D.GRANT_DATA

        write.rsp.valid := bus.d.valid && !sel
        write.rsp.error := bus.d.denied || bus.d.corrupt
        write.rsp.id    := bus.d.source.resized

        bus.d.ready := sel.mux(read.rsp.ready, True)
      }

      val onE = new Area{
        bus.e.arbitrationFrom(read.ack)
        bus.e.sink := read.ack.ackId
      }
    }
  }.bus

}
