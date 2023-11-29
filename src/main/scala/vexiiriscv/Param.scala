package vexiiriscv

import spinal.core._
import spinal.lib.misc.plugin.Hostable
import vexiiriscv._
import vexiiriscv.execute.{AguPlugin, BarrelShifterPlugin, BranchPlugin, CsrAccessPlugin, DivPlugin, IntAluPlugin, IntFormatPlugin, LsuCachelessPlugin, MulPlugin, RsUnsignedPlugin, SrcPlugin, WriteBackPlugin}
import vexiiriscv.misc.{PrivilegedConfig, PrivilegedPlugin, WhiteboxerPlugin}
import vexiiriscv.riscv.IntRegFile

import scala.collection.mutable.ArrayBuffer

class ParamSimple(){
  var xlen = 32
  var rvc = false
  var hartCount = 1
  var withMmu = false
  var resetVector = 0x80000000l
  var decoders = 1
  var lanes = 1
  var ioRange    : UInt => Bool = a => a(31 downto 28) === 0x1
  var fetchRange : UInt => Bool = a => a(31 downto 28) =/= 0x1

  def plugins() = {
    val plugins = ArrayBuffer[Hostable]()

    plugins += new riscv.RiscvPlugin(xlen, rvc, hartCount)
    withMmu match {
      case false => plugins += new memory.StaticTranslationPlugin(32, ioRange, fetchRange)
      case true =>
    }

    plugins += new misc.PipelineBuilderPlugin()
    plugins += new schedule.ReschedulePlugin()

    plugins += new fetch.PcPlugin(resetVector)
    plugins += new fetch.FetchPipelinePlugin()
    plugins += new fetch.CachelessPlugin(wordWidth = 32*decoders)

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
    val intRegFileRelaxedPort = "intShared" //Used by out of pip units to write stuff into the pipeline, //TODO ensure some sort of fairness between no ready and with ready

    plugins += new execute.ExecuteLanePlugin("lane0", priority = 0, rfReadAt = 0, decodeAt = 1, executeAt = 2)
    plugins += new SrcPlugin("lane0")
    plugins += new IntAluPlugin("lane0", formatAt = 0)
    plugins += new BarrelShifterPlugin("lane0", formatAt = 1)
    plugins += new IntFormatPlugin("lane0")
    plugins += new BranchPlugin("lane0")
    plugins += new LsuCachelessPlugin(
      laneName = "lane0",
      translationStorageParameter = null,
      translationPortParameter = null
    )
    plugins += new RsUnsignedPlugin("lane0")
    plugins += new MulPlugin("lane0")
    plugins += new DivPlugin("lane0")
    plugins += new CsrAccessPlugin("lane0", writeBackKey = intRegFileRelaxedPort)
    plugins += new PrivilegedPlugin(PrivilegedConfig.full)
    plugins += new WriteBackPlugin("lane0", IntRegFile, writeAt = 2, bypassOn = _ >= 0, writeBackKey = if(lanes == 1) intRegFileRelaxedPort else null)

    if(lanes >= 2) {
      plugins += new execute.ExecuteLanePlugin("lane1", priority = 1, rfReadAt = 0, decodeAt = 1, executeAt = 2)
      plugins += new SrcPlugin("lane1")
      plugins += new IntAluPlugin("lane1", formatAt = 0)
      plugins += new BarrelShifterPlugin("lane1", formatAt = 1)
      plugins += new IntFormatPlugin("lane1")
      plugins += new BranchPlugin("lane1")
      plugins += new WriteBackPlugin("lane1", IntRegFile, writeAt = 2, bypassOn = _ >= 0, writeBackKey = intRegFileRelaxedPort)
    }




    plugins += new WhiteboxerPlugin()

    plugins
  }
}

