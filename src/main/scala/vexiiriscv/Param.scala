package vexiiriscv

import spinal.lib.misc.plugin.Hostable
import vexiiriscv._
import vexiiriscv.execute.{BarrelShifterPlugin, BranchPlugin, IntAluPlugin, IntFormatPlugin, SrcPlugin, WriteBackPlugin}
import vexiiriscv.misc.WhiteboxerPlugin
import vexiiriscv.riscv.IntRegFile

import scala.collection.mutable.ArrayBuffer

class ParamSimple(){
  val xlen = 32
  val rvc = false
  val hartCount = 1
  val withMmu = false
  val resetVector = 0x80000000l
  val decoders = 1

  def plugins() = {
    val plugins = ArrayBuffer[Hostable]()

    plugins += new riscv.RiscvPlugin(xlen, rvc, hartCount)
    withMmu match {
      case false => plugins += new memory.StaticTranslationPlugin(32)
      case true =>
    }
    plugins += new schedule.ReschedulePlugin()
    plugins += new fetch.PcPlugin(resetVector)
    plugins += new fetch.FetchPipelinePlugin()
    plugins += new fetch.CachelessPlugin(wordWidth = 32*decoders)
    plugins += new misc.PipelineBuilderPlugin()
    plugins += new decode.DecodePipelinePlugin()
    plugins += new decode.AlignerPlugin(lanes = decoders)
    plugins += new decode.DecoderPlugin()
    plugins += new schedule.DispatchPlugin()

    plugins += new regfile.RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 32,
      preferedWritePortForInit = "lane0",
      syncRead = true
    )

    plugins += new execute.ExecutePipelinePlugin()

    plugins += new execute.ExecuteLanePlugin("lane0", priority = 0, rfReadAt = 0, decodeAt = 1, executeAt = 2)
    plugins += new SrcPlugin("lane0")
    plugins += new IntAluPlugin("lane0", formatAt = 0)
    plugins += new BarrelShifterPlugin("lane0", formatAt = 1)
    plugins += new IntFormatPlugin("lane0")
    plugins += new BranchPlugin("lane0")
    plugins += new WriteBackPlugin("lane0", IntRegFile, writeAt = 2, bypassOn = _ >= 0)

//    plugins += new execute.ExecuteLanePlugin("lane1", priority = 0, rfReadAt = 0, decodeAt = 1, executeAt = 2)
//    plugins += new SrcPlugin("lane1")
//    plugins += new IntAluPlugin("lane1", formatAt = 0)
//    plugins += new BarrelShifterPlugin("lane1", formatAt = 1)
//    plugins += new IntFormatPlugin("lane1")
//    plugins += new BranchPlugin("lane1")
//    plugins += new WriteBackPlugin("lane1", IntRegFile, writeAt = 2, bypassOn = _ >= 0)


    plugins += new WhiteboxerPlugin()

    plugins
  }
}

