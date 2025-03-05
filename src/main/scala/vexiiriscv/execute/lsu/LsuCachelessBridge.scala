package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4Shared
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{DebugId, S2mSupport}
import spinal.lib.misc.pipeline._
import vexiiriscv.execute

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Simple bridge to tilelink which avoid ordering issues by keeping track of all inflight memory transactions and halt
 * new request if a conflict is detected.
 */
class LsuCachelessBusToTilelink(up : LsuCachelessBus, hashWidth : Int, trackHazard : Boolean = true) extends Area{
  assert(!up.p.withAmo)

  val m2sParam = up.p.toTilelinkM2s(this)
  val down = tilelink.Bus(m2sParam)

  val cmdHash = up.cmd.address(log2Up(up.p.dataWidth / 8), hashWidth bits)

  val tracker = trackHazard generate new Area {
    // Storage for all inflight transactions
    val pendings = List.fill(up.p.pendingMax)(new Area {
      val valid = RegInit(False)
      val hash = Reg(UInt(hashWidth bits))
      val mask = Reg(Bits(up.p.dataWidth / 8 bits))
      val io = Reg(Bool())
      val hazard = valid && (hash === cmdHash && (mask & up.cmd.mask).orR || io && up.cmd.io)
    })

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

    val hazard = pendings.map(_.hazard).orR
    down.a.arbitrationFrom(up.cmd.haltWhen(hazard))
  }

  val noTracker = !trackHazard generate new Area {
    down.a.arbitrationFrom(up.cmd)
  }

  down.a.opcode  := up.cmd.write.mux(tilelink.Opcode.A.PUT_FULL_DATA, tilelink.Opcode.A.GET)
  down.a.param   := 0
  down.a.source  := up.cmd.id
  down.a.address := up.cmd.address
  down.a.size    := up.cmd.size
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

/**
 * Bridge the LsuCachelessPlugin to a tilelink Node using LsuCachelessBusToTilelink
 */
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


/**
 * Bridge the LsuPlugin io bus to tilelink node
 */
class LsuTileLinkPlugin(node : bus.tilelink.fabric.Node, hashWidth : Int = 8) extends FiberPlugin {
  val logic = during build new Area{
    val lsucp = host[LsuPlugin]

    node.m2s.forceParameters(lsucp.busParam.toTilelinkM2s(LsuTileLinkPlugin.this))
    node.s2m.supported.load(S2mSupport.none())

    lsucp.logic.bus.setAsDirectionLess()
    val bridge = new LsuCachelessBusToTilelink(lsucp.logic.bus, hashWidth, trackHazard = false) //We don't need trackHazard, has the LsuPlugin will only generate single io access
    master(bridge.down)
    node.bus.component.rework(node.bus << bridge.down)
  }
}




class LsuCachelessBusToAxi4Shared(up : LsuCachelessBus, hashWidth : Int = 8, trackHazard : Boolean = true) extends Area{
  assert(!up.p.withAmo)
  val down = Axi4Shared(up.p.toAxi4Config())
  up.setAsDirectionLess()

  val cmdHalted = cloneOf(up.cmd)

  val cmdHash = up.cmd.address(log2Up(up.p.dataWidth / 8), hashWidth bits)
  val tracker = trackHazard generate new Area {
    // Storage for all inflight transactions
    val pendings = List.fill(up.p.pendingMax)(new Area {
      val valid = RegInit(False)
      val hash = Reg(UInt(hashWidth bits))
      val mask = Reg(Bits(up.p.dataWidth / 8 bits))
      val io = Reg(Bool())
      val hazard = valid && (hash === cmdHash && (mask & up.cmd.mask).orR || io && up.cmd.io)
    })

    when(up.rsp.fire) {
      pendings.onSel(up.rsp.id) { e =>
        e.valid := False
      }
    }

    when(up.cmd.fire) {
      pendings.onSel(up.cmd.id) { e =>
        e.valid := True
        e.hash := cmdHash
        e.mask := up.cmd.mask
        e.io := up.cmd.io
      }
    }

    val hazard = pendings.map(_.hazard).orR
    cmdHalted << up.cmd.haltWhen(hazard)
  }

  val noTracker = !trackHazard generate new Area {
    cmdHalted << up.cmd
  }

  val (cmdFork, dataFork) = StreamFork2(cmdHalted)
  down.sharedCmd.arbitrationFrom(cmdFork)
  down.sharedCmd.write := cmdFork.write
  down.sharedCmd.prot := "010"
  down.sharedCmd.cache := cmdFork.io.mux(B"0000", B"1111")
  down.sharedCmd.size := cmdFork.size.resized
  down.sharedCmd.addr := cmdFork.address
  down.sharedCmd.id   := cmdFork.id

  val dataStage = dataFork.throwWhen(!dataFork.write)
  down.writeData.arbitrationFrom(dataStage)
  down.writeData.last := True
  down.writeData.data := dataStage.data
  down.writeData.strb := dataStage.mask

  up.rsp.valid := down.b.valid
  up.rsp.error := !down.b.isOKAY()
  up.rsp.id    := down.b.id
  up.rsp.data  := down.r.data
  down.r.ready := True
  down.b.ready := True

  when(down.r.valid) {
    up.rsp.valid := down.r.valid
    up.rsp.error := !down.r.isOKAY()
    up.rsp.id    := down.r.id
    down.b.ready := False
  }
}

class LsuCachelessAxi4Plugin() extends FiberPlugin {
  val logic = during build new Area{
    val bus = host[LsuCachelessBusProvider].getLsuCachelessBus()
    bus.setAsDirectionLess()

    val bridge = new LsuCachelessBusToAxi4Shared(bus)
    val axi = master(bridge.down.toAxi4())
  }
}

