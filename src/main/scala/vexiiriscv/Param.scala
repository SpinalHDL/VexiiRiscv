package vexiiriscv

import spinal.lib.misc.plugin.Hostable
import vexiiriscv._
import vexiiriscv.execute.{IntAluPlugin, SrcPlugin}

import scala.collection.mutable.ArrayBuffer

class ParamSimple(){
  val xlen = 32
  val rvc = false
  val hartCount = 1
  val withMmu = false
  val resetVector = 0x80000000l

  def plugins() = {
    val plugins = ArrayBuffer[Hostable]()

    plugins += new riscv.RiscvPlugin(xlen, rvc, hartCount)
    withMmu match {
      case false => plugins += new memory.StaticTranslationPlugin(32)
      case true =>
    }
    plugins += new schedule.FlusherPlugin()
    plugins += new fetch.PcPlugin(resetVector)
    plugins += new fetch.FetchPipelinePlugin()
    plugins += new fetch.CachelessPlugin(wordWidth = 32)
    plugins += new misc.PipelineBuilderPlugin()
    plugins += new decode.DecodePipelinePlugin()
    plugins += new decode.AlignerPlugin(lanes = 1)
    plugins += new decode.DecoderPlugin()
    plugins += new schedule.DispatchPlugin()

    plugins += new execute.ExecuteUnitPlugin("eu0", priority = 0)
    plugins += new SrcPlugin("eu0")
    plugins += new IntAluPlugin("eu0")

    plugins
  }
}

