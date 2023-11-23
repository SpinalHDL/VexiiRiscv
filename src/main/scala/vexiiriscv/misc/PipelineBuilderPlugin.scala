package vexiiriscv.misc

import spinal.core.Area
import spinal.core.fiber.Lock
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline

class PipelineBuilderPlugin extends FiberPlugin{
  val elaborationLock = Lock()
  val logic = during build new Area{
    elaborationLock.await()
    val chunks = host.list[PipelineService]
    val connectors = chunks.flatMap(_.getConnectors())
    pipeline.Builder(connectors)
  }
}
