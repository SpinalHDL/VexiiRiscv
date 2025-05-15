package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{DebugId, S2mSupport}
import spinal.lib.misc.plugin.FiberPlugin

// Plugin to embed a bridge in VexiiRiscv to convert the FetchL1Bus to Tilelink
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

class FetchL1Axi4Plugin() extends FiberPlugin {
  val logic = during build new Area{
    val fcp = host[FetchL1Plugin]
    fcp.logic.bus.setAsDirectionLess()
    val axi = master(fcp.logic.bus.toAxi4())
  }
}

class FetchL1WishbonePlugin() extends FiberPlugin {
  val logic = during build new Area{
    val fcp = host[FetchL1Plugin]
    fcp.logic.bus.setAsDirectionLess()
    val bus = master(fcp.logic.bus.toWishbone())
  }
}

