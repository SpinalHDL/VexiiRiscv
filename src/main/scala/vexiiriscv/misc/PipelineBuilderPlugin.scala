package vexiiriscv.misc

import spinal.core.Area
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline

class PipelineBuilderPlugin extends FiberPlugin{
  val logic = during build new Area{
    val chunks = host.list[PipelineService]
    val connectors = chunks.flatMap(_.getConnectors())
    pipeline.Builder(connectors)
  }
}
