package vexiiriscv.soc.micro

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.{M2sSupport, M2sTransfers}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.com.spi.ddr.{SpiXdrMasterCtrl, SpiXdrParameter}
import spinal.lib.com.spi.xdr.TilelinkSpiXdrMasterFiber
import spinal.lib.com.uart.TilelinkUartFiber
import spinal.lib.misc.{Elf, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.MemoryConnection
import vexiiriscv.execute.cfu.{CfuPlugin, CfuTest}
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.system.tag.MemoryEndpoint


// -----------------------------------------------------------------------------
// SRAM macro
// -----------------------------------------------------------------------------
class spram_8192_32 extends BlackBox {
  val io = new Bundle {
    val A   = in Bits(13 bits)
    val CEB = in Bool()
    val WEB = in Bool()
    val OEB = in Bool()
    val CSB = in Bool()
    val I   = in Bits(32 bits)
    val O   = out Bits(32 bits)
  }
  noIoPrefix()
}

// -----------------------------------------------------------------------------
// RMW FSM
// -----------------------------------------------------------------------------
object RmwState extends SpinalEnum {
  val IDLE, READ, READ_WAIT, MERGE, WRITE, WRITE_WAIT, RESP = newElement()
}

// -----------------------------------------------------------------------------
// Store buffer entry
// -----------------------------------------------------------------------------
case class StoreBufEntry() extends Bundle {
  val valid  = Bool()
  val addr   = UInt(13 bits)
  val data   = Bits(32 bits)
  val mask   = Bits(4 bits)
  val size   = UInt(3 bits)
  val source = UInt(4 bits)
}

// -----------------------------------------------------------------------------
// TileLink Store Buffer (FINAL, CORRECT)
// -----------------------------------------------------------------------------
class TilelinkStoreBuffer extends Component {

  val io = new Bundle {
    val tlIn  = slave(Tilelink())
    val tlOut = master(Tilelink())
  }

  val buf = Reg(StoreBufEntry())
  buf.valid init(False)

  val isStore =
    io.tlIn.a.opcode === Opcode.A.PUT_FULL_DATA ||
    io.tlIn.a.opcode === Opcode.A.PUT_PARTIAL_DATA

  val isLoad = io.tlIn.a.opcode === Opcode.A.GET

  // ---------------------------------------------------------------------------
  // A channel: STRICT ordering
  // Stall everything while store buffer is occupied
  // ---------------------------------------------------------------------------
  io.tlIn.a.ready := !buf.valid

  io.tlOut.a.valid := False
  io.tlOut.a.payload.assignDontCare()

  when(io.tlIn.a.fire && isStore) {
    buf.valid  := True
    buf.addr   := io.tlIn.a.address(14 downto 2)
    buf.data   := io.tlIn.a.data
    buf.mask   := io.tlIn.a.mask
    buf.size   := io.tlIn.a.size
    buf.source := io.tlIn.a.source
  }

  // Replay buffered store only
  when(buf.valid) {
    io.tlOut.a.valid   := True
    io.tlOut.a.opcode  := Opcode.A.PUT_PARTIAL_DATA
    io.tlOut.a.address := buf.addr @@ U"2'b00"
    io.tlOut.a.data    := buf.data
    io.tlOut.a.mask    := buf.mask
    io.tlOut.a.size    := buf.size
    io.tlOut.a.source  := buf.source
  }

  when(io.tlOut.a.fire && buf.valid) {
    buf.valid := False
  }

  // ---------------------------------------------------------------------------
  // D channel: SINGLE OWNER (TileLink-legal)
  // ---------------------------------------------------------------------------
io.tlIn.d.valid := io.tlOut.d.valid
io.tlIn.d       := io.tlOut.d

  }


// -----------------------------------------------------------------------------
// TileLink SRAM wrapper (NO byte enable, RMW supported)
// -----------------------------------------------------------------------------
class TilelinkSram8192x32(base: BigInt) extends Component {

  val node = Node.slave(
    supports = M2sSupport(
      transfers = M2sTransfers(
        get         = SizeRange(4),
        putFull    = SizeRange(4),
        putPartial = SizeRange(1, 4)
      )
    )
  )

  node at base
  node.addTag(new MemoryEndpoint {
    override def mapping = SizeMapping(base, 8192 * 4)
  })

  val tl = node.bus
  val state = RegInit(RmwState.IDLE)

  val savedAddr = Reg(UInt(13 bits))
  val savedData = Reg(Bits(32 bits))
  val savedMask = Reg(Bits(4 bits))
  val savedSize = Reg(UInt(tl.a.size.getWidth bits))
  val savedSrc  = Reg(UInt(tl.a.source.getWidth bits))

  val oldData = Reg(Bits(32 bits))
  val rspData = Reg(Bits(32 bits))
  val wasRead = Reg(Bool())

  tl.a.ready := (state === RmwState.IDLE)
  val aFire = tl.a.fire

  when(aFire) {
    savedAddr := tl.a.address(14 downto 2)
    savedData := tl.a.data
    savedMask := tl.a.mask
    savedSize := tl.a.size
    savedSrc  := tl.a.source
    wasRead   := tl.a.opcode === Opcode.A.GET

    state := tl.a.opcode match {
      case Opcode.A.GET              => RmwState.READ
      case Opcode.A.PUT_PARTIAL_DATA => RmwState.READ
      case Opcode.A.PUT_FULL_DATA    => RmwState.WRITE
    }
  }

  val sramCd = ClockDomain(
    clock = !ClockDomain.current.readClockWire,
    reset = ClockDomain.current.readResetWire
  )

  val sramArea = new ClockingArea(sramCd) {
    val sram = new spram_8192_32
    val addr = Reg(UInt(13 bits))
    when(aFire) { addr := savedAddr }

    sram.io.A   := addr.asBits
    sram.io.I   := rspData
    sram.io.CSB := False
    sram.io.WEB := !(state === RmwState.WRITE || state === RmwState.WRITE_WAIT)
    sram.io.OEB := !(state === RmwState.READ)
    sram.io.CEB := !(state =/= RmwState.IDLE)
  }

  def expandMask(m: Bits): Bits = Cat(
    m(3) ? B"8'hFF" | B"8'h00",
    m(2) ? B"8'hFF" | B"8'h00",
    m(1) ? B"8'hFF" | B"8'h00",
    m(0) ? B"8'hFF" | B"8'h00"
  )

  val mask32 = expandMask(savedMask)

  switch(state) {
    is(RmwState.READ)      { state := RmwState.READ_WAIT }
    is(RmwState.READ_WAIT){
      oldData := sramArea.sram.io.O
      rspData := sramArea.sram.io.O
      state   := wasRead ? RmwState.RESP | RmwState.MERGE
    }
    is(RmwState.MERGE) {
      rspData := (oldData & ~mask32) | (savedData & mask32)
      state   := RmwState.WRITE
    }
    is(RmwState.WRITE) {
      when(!wasRead) { rspData := savedData }
      state := RmwState.WRITE_WAIT
    }
    is(RmwState.WRITE_WAIT) { state := RmwState.RESP }
    is(RmwState.RESP) {
      when(tl.d.fire) { state := RmwState.IDLE }
    }
  }

  tl.d.valid  := (state === RmwState.RESP)
  tl.d.opcode := wasRead ? Opcode.D.ACCESS_ACK_DATA | Opcode.D.ACCESS_ACK
  tl.d.param  := 0
  tl.d.size   := savedSize
  tl.d.source := savedSrc
  tl.d.data   := rspData
  tl.d.error  := False
}

// Lets define our SoC toplevel
class MicroSoc(p : MicroSocParam) extends Component {
  // socCtrl will provide clocking, reset controllers and debugModule (through jtag) to our SoC
  val socCtrl = new SocCtrl(p.socCtrl)
  
  
  val system = new ClockingArea(socCtrl.system.cd) {
    // Let's define our main tilelink bus on which the CPU, RAM and peripheral "portal" will be plugged later.
    val mainBus = tilelink.fabric.Node()

    val cpu = new TilelinkVexiiRiscvFiber(p.vexii.plugins())
    if(p.socCtrl.withDebug) socCtrl.debugModule.bindHart(cpu)
    mainBus << cpu.buses
    cpu.dBus.setDownConnection(a = StreamPipe.S2M) // Let's add a bit of pipelining on the cpu.dBus to increase FMax

    val sram = new TilelinkSram8192x32(0x80000000L)
    val storeBuf = new TilelinkStoreBuffer
mainBus << storeBuf.io.tlIn
storeBuf.io.tlOut >> sram.node


    // Handle all the IO / Peripheral things
    val peripheral = new Area {
      // Some peripheral may require to have an access as big as the CPU XLEN, so, lets define a bus which ensure it.
      val busXlen = Node()
      busXlen.forceDataWidth(p.vexii.xlen)
      busXlen << mainBus
      busXlen.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)

      // Most peripheral will work with a 32 bits data bus.
      val bus32 = Node()
      bus32.forceDataWidth(32)
      bus32 << busXlen

      // The clint is a regular RISC-V timer peripheral
      val clint = new TilelinkClintFiber()
      clint.node at 0x10010000 of busXlen

      // The clint is a regular RISC-V interrupt controller
      val plic = new TilelinkPlicFiber()
      plic.node at 0x10C00000 of bus32

      val uart = new TilelinkUartFiber()
      uart.node at 0x10001000 of bus32
      plic.mapUpInterrupt(1, uart.interrupt)

      val spiFlash = p.withSpiFlash generate new TilelinkSpiXdrMasterFiber(SpiXdrMasterCtrl.MemoryMappingParameters(
        SpiXdrMasterCtrl.Parameters(8, 12, SpiXdrParameter(2, 2, 1)).addFullDuplex(0,1,false),
        xipEnableInit = true,
        xip = SpiXdrMasterCtrl.XipBusParameters(addressWidth = 24, lengthWidth = 6)
      )) {
        plic.mapUpInterrupt(2, interrupt)
        ctrl at 0x10002000 of bus32
        xip at 0x20000000 of bus32
      }

      val demo = p.demoPeripheral.map(new PeripheralDemoFiber(_){
        node at 0x10003000 of bus32
        plic.mapUpInterrupt(3, interrupt)
      })

      // Let's connect a few of the CPU interfaces to their respective peripherals
      val cpuPlic = cpu.bind(plic) // External interrupts connection
      val cpuClint = cpu.bind(clint) // Timer interrupt + time reference + stop time connection
    }

    val cfu = p.vexii.withCfu generate (Fiber patch new Area {
      val cpuCfuBus = cpu.logic.core.host[CfuPlugin].logic.bus
      val cfu = CfuTest() // If instead you want to export the CFU bus to the io, replace with : val bus = cpuCfuBus.toIo()
      cfu.io.bus << cpuCfuBus
    })

    val patcher = Fiber patch new Area {
      println(MemoryConnection.getMemoryTransfers(cpu.dBus).mkString("\n"))
    }
  }
}
