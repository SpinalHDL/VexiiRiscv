package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{DebugId, S2mSupport}
import spinal.lib.misc.pipeline._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class LsuCachelessBusToTilelink(up : LsuCachelessBus, hashWidth : Int) extends Area{
  assert(!up.p.withAmo)

  val m2sParam = up.p.toTilelinkM2s(this)
  val down = tilelink.Bus(m2sParam)

  val cmdHash = up.cmd.address(log2Up(up.p.dataWidth / 8), hashWidth bits)

  val pendings = List.fill(up.p.pendingMax)(new Area{ //TODO could be one less
    val valid = RegInit(False)
    val hash = Reg(UInt(hashWidth bits))
    val mask = Reg(Bits(up.p.dataWidth/8 bits))
    val io = Reg(Bool())
    val hazard = valid && (hash === cmdHash && (mask & up.cmd.mask).orR || io && up.cmd.io)
  })
  val hazard = pendings.map(_.hazard).orR

  when(down.d.fire) {
    pendings.onSel(down.d.source) { e =>
      e.valid := False
    }
  }

  when(down.a.fire) {
    pendings.onSel(up.cmd.id) { e =>
      e.valid := True
      e.hash := cmdHash
      e.mask := up.cmd.mask
      e.io := up.cmd.io
    }
  }

  down.a.arbitrationFrom(up.cmd.haltWhen(hazard))
  down.a.opcode  := up.cmd.write.mux(tilelink.Opcode.A.PUT_FULL_DATA, tilelink.Opcode.A.GET)
  down.a.param   := 0
  down.a.source  := up.cmd.id
  down.a.address := up.cmd.address
  down.a.size    := log2Up(up.p.dataWidth/8)
  down.a.debugId := DebugId.withPostfix(up.cmd.id)
  down.a.mask    := up.cmd.mask
  down.a.data    := up.cmd.data
  down.a.corrupt := False

  down.d.ready := True
  up.rsp.valid := down.d.valid
  up.rsp.id    := down.d.source
  up.rsp.error := down.d.denied
  up.rsp.data  := down.d.data
}


class LsuCachelessTileLinkPlugin(node : bus.tilelink.fabric.Node, hashWidth : Int = 8) extends FiberPlugin {
  val logic = during build new Area{
    val lsucp = host[LsuCachelessPlugin]

    node.m2s.forceParameters(lsucp.busParam.toTilelinkM2s(LsuCachelessTileLinkPlugin.this))
    node.s2m.supported.load(S2mSupport.none())

    lsucp.logic.bus.setAsDirectionLess()

    val bridge = new LsuCachelessBusToTilelink(lsucp.logic.bus, hashWidth)
    master(bridge.down)
    node.bus.component.rework(node.bus << bridge.down)
  }
}



class LsuTileLinkPlugin(node : bus.tilelink.fabric.Node, hashWidth : Int = 8) extends FiberPlugin {
  val logic = during build new Area{
    val lsucp = host[LsuPlugin]

    node.m2s.forceParameters(lsucp.busParam.toTilelinkM2s(LsuTileLinkPlugin.this))
    node.s2m.supported.load(S2mSupport.none())

    lsucp.logic.bus.setAsDirectionLess()
    val bridge = new LsuCachelessBusToTilelink(lsucp.logic.bus, hashWidth)
    master(bridge.down)
    node.bus.component.rework(node.bus << bridge.down)
  }
}
