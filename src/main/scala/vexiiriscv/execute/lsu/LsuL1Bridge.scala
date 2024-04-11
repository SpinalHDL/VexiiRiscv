package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{DebugId, S2mSupport, S2mTransfers, SizeRange}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.LsuL1Bus



class LsuL1TileLinkPlugin(node : bus.tilelink.fabric.Node, probeInflightMax : Int = 2) extends FiberPlugin {
  val logic = during setup new Area{
    val lsucp = host[LsuL1Plugin]
    val l1Lock = lsucp.elaborationRetainer()
    awaitBuild()

    val memParam = lsucp.memParameter
    node.m2s.forceParameters(memParam.toTileLinkM2sParameters(LsuL1TileLinkPlugin.this))
    node.s2m.supported.load(
      S2mSupport(
        S2mTransfers(
          probe = SizeRange(lsucp.withCoherency.mux(lsucp.lineSize, 0))
        )
      )
    )
    lsucp.probeIdWidth = node.m2s.parameters.sourceWidth
    lsucp.ackIdWidth = node.s2m.parameters.sinkWidth
    l1Lock.release()

    lsucp.logic.bus.setAsDirectionLess()
    val down = master(lsucp.logic.bus.toTilelink(probeInflightMax))
    node.bus.component.rework(node.bus << down)
  }
}
//--lsu-l1-coherency