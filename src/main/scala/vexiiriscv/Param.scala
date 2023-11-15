package vexiiriscv

import spinal.lib.misc.plugin.Hostable
import vexiiriscv._

import scala.collection.mutable.ArrayBuffer

object Param {
  def simple(xlen : Int,
             rvc : Boolean = false,
             hartCount: Int = 1,
             withMmu : Boolean = false,
             resetVector : BigInt = 0x80000000l) = {
    val plugins = ArrayBuffer[Hostable]()

    plugins += new riscv.RiscvPlugin(xlen, rvc, hartCount)
    withMmu match {
      case false => plugins += new memory.StaticTranslationPlugin(32)
      case true  =>
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

    plugins
  }
}
