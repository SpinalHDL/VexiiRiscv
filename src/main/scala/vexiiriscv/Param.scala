package vexiiriscv

import spinal.lib.misc.plugin.Hostable
import vexiiriscv._

import scala.collection.mutable.ArrayBuffer

object Param {
  def simple(xlen : Int,
             hartCount: Int = 1,
             withMmu : Boolean = false,
             resetVector : BigInt = 0x80000000l) = {
    val plugins = ArrayBuffer[Hostable]()

    plugins += new riscv.RiscvPlugin(xlen, hartCount)
    withMmu match {
      case false => plugins += new memory.StaticTranslationPlugin(32)
      case true  =>
    }
    plugins += new fetch.PcPlugin(resetVector)
    plugins += new fetch.PipelinePlugin()
    plugins += new fetch.CachelessPlugin(wordWidth = 32)

    plugins
  }
}
