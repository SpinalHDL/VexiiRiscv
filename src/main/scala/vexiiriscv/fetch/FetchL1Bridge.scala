package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{DebugId, S2mSupport}
import spinal.lib.misc.plugin.FiberPlugin


class FetchFetchL1TileLinkPlugin(node : bus.tilelink.fabric.Node) extends FiberPlugin {
  val logic = during build new Area{
    val fcp = host[FetchL1Plugin]
    fcp.logic.bus.setAsDirectionLess()
    val down = fcp.logic.bus.toTilelink()
    master(down)
    node.m2s.forceParameters(down.p.node.m)
    node.s2m.supported.load(S2mSupport.none())
    node.bus.component.rework(node.bus << down)
  }
}
