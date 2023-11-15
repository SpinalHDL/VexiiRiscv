package vexiiriscv.decode

import spinal.core._
import spinal.lib.misc.pipeline.{Connector, CtrlConnector}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.fetch.FetchPipelinePlugin
import vexiiriscv.misc.PipelineService

import scala.collection.mutable.ArrayBuffer

class DecoderPlugin() extends FiberPlugin {
  lazy val dpp = host[DecodePipelinePlugin]
  addLockable(dpp)

  val logic = during build new Area{
    Decode.INSTRUCTION_WIDTH.set(32)
  }
}
