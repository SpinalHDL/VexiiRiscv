package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{DebugId, S2mSupport}
import spinal.lib.misc.plugin.FiberPlugin


class FetchL1TileLinkPlugin(node : bus.tilelink.fabric.Node) extends FiberPlugin {
  val logic = during build new Area{
    val fcp = host[FetchL1Plugin]

    val memParam = fcp.getBusParameter()
    node.m2s.forceParameters(memParam.toTileLinkM2sParameters(FetchL1TileLinkPlugin.this))
    node.s2m.supported.load(S2mSupport.none())

    fcp.logic.bus.setAsDirectionLess()
    val down = master(fcp.logic.bus.toTilelink())
    node.bus.component.rework(node.bus << down)
  }
}
