package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{DebugId, S2mSupport}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.LsuL1Bus



class LsuL1TileLinkPlugin(node : bus.tilelink.fabric.Node) extends FiberPlugin {
  val logic = during setup new Area{
    val lsucp = host[LsuL1Plugin]
    lsucp.probeIdWidth = 0
    lsucp.ackIdWidth = 0
    assert(!lsucp.withCoherency)

    awaitBuild()
    lsucp.logic.bus.setAsDirectionLess()

    val down = lsucp.logic.bus.toTilelink()
    master(down)

    node.m2s.forceParameters(down.p.node.m)
    node.s2m.supported.load(S2mSupport.none())
    node.bus.component.rework(node.bus << down)
  }
}
