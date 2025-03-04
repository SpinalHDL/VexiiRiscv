package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4ReadOnly, Axi4Shared}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{DebugId, S2mSupport}
import spinal.lib.bus.wishbone.Wishbone
import spinal.lib.misc.plugin.FiberPlugin

// Plugin to embed a bridge in VexiiRiscv to convert the CachelessBus to Tilelink
class FetchCachelessTileLinkPlugin(node : bus.tilelink.fabric.Node) extends FiberPlugin {
  val logic = during build new Area{
    val fcp = host[FetchCachelessPlugin]
    fcp.logic.bus.setAsDirectionLess()

    val bridge = new CachelessBusToTilelink(fcp.logic.bus)
    master(bridge.down)

    node.m2s.forceParameters(bridge.m2sParam)
    node.s2m.supported.load(S2mSupport.none())
    node.bus.component.rework(node.bus << bridge.down)
  }
}

class CachelessBusToTilelink(up : CachelessBus) extends Area{
  assert(up.p.cmdPersistence)
  val m2sParam = up.p.toTilelinkM2s(this)
  val down = tilelink.Bus(m2sParam)
  down.a.arbitrationFrom(up.cmd)
  down.a.opcode  := tilelink.Opcode.A.GET
  down.a.param   := 0
  down.a.source  := up.cmd.id
  down.a.address := up.cmd.address
  down.a.size    := log2Up(up.p.dataWidth/8)
  down.a.debugId := DebugId.withPostfix(up.cmd.id)

  down.d.ready := True
  up.rsp.valid := down.d.valid
  up.rsp.id    := down.d.source
  up.rsp.error := down.d.denied
  up.rsp.word  := down.d.data
}


class FetchCachelessAxi4Plugin() extends FiberPlugin {
  val logic = during build new Area{
    val fcp = host[FetchCachelessPlugin]
    fcp.logic.bus.setAsDirectionLess()

    val bridge = new CachelessBusToAxi4(fcp.logic.bus)
    master(bridge.axi)
  }
}

class CachelessBusToAxi4(up : CachelessBus) extends Area{
  assert(up.p.cmdPersistence)
  val axi = Axi4ReadOnly(up.p.toAxi4Config())

  axi.ar.valid := up.cmd.valid
  axi.ar.addr  := up.cmd.address
  axi.ar.id    := up.cmd.id
  axi.ar.size  := log2Up(up.p.dataWidth/8)
  axi.ar.prot  := "110"
  axi.ar.cache := "1111"
  up.cmd.ready := axi.ar.ready

  up.rsp.valid := axi.r.valid
  up.rsp.id    := axi.r.id
  up.rsp.word  := axi.r.data
  up.rsp.error := !axi.r.isOKAY()
  axi.r.ready  := True
}


class FetchCachelessWishbonePlugin() extends FiberPlugin {
  val logic = during build new Area{
    val fcp = host[FetchCachelessPlugin]
    fcp.logic.bus.setAsDirectionLess()

    val bridge = new CachelessBusToWishbone(fcp.logic.bus)
    master(bridge.bus)
  }
}

class CachelessBusToWishbone(up : CachelessBus) extends Area{
  val wishboneConfig = up.p.toWishboneConfig()
  val bus = Wishbone(wishboneConfig)
  val cmdPipe = up.cmd.stage()

  bus.ADR := (cmdPipe.address >>  log2Up(up.p.dataWidth/8))
  bus.CTI := B"000"
  bus.BTE := "00"
  bus.SEL.setAll()
  bus.WE  := False
  bus.DAT_MOSI.assignDontCare()
  bus.CYC := cmdPipe.valid
  bus.STB := cmdPipe.valid

  cmdPipe.ready := cmdPipe.valid && (bus.ACK || bus.ERR)
  up.rsp.valid := bus.CYC && (bus.ACK || bus.ERR)
  up.rsp.word := bus.DAT_MISO
  up.rsp.id := cmdPipe.id
  up.rsp.error := bus.ERR
  bus
}


