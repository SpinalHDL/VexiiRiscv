package vexiiriscv.execute

import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core._
import spinal.lib.Flow
import vexiiriscv.decode.DecodeListType
import vexiiriscv.riscv.{MicroOp, RD, RfResource}


//This is a simple skeleton to ease the implementation of simple ExecutionUnit elements. It assume a single writeback and a single completion
abstract class ExecutionUnitElementSimple(euId : String) extends FiberPlugin {
  lazy val eu = host.find[ExecuteLanePlugin](_.laneName == euId)
  lazy val srcp = host.find[SrcPlugin](_.laneName == euId)
  buildBefore(eu.pipelineLock)
  setupRetain(eu.uopLock)
  setupRetain(srcp.elaborationLock)
  withPrefix(euId)

  val SEL = Payload(Bool())

  class Logic extends Area {
    case class ImplicitIntFormatPluginPort(ifp: IntFormatPlugin, port: Flow[Bits])
    case class ImplicitWriteBackPluginPort(wbp: WriteBackPlugin, port: Flow[Bits])

    eu.setDecodingDefault(SEL, False)
    def add(microOp: MicroOp)(implicit iifpp : ImplicitIntFormatPluginPort = null,
                                       iwbpp : ImplicitWriteBackPluginPort = null) = new {
      eu.addMicroOp(microOp)
      decode(SEL -> True)
      if (iifpp != null) iifpp.ifp.addMicroOp(iifpp.port, microOp)
      if (iwbpp != null) iwbpp.wbp.addMicroOp(iwbpp.port, microOp)

      def decode(decoding: DecodeListType = Nil): this.type = {
        eu.addDecoding(microOp, decoding)
        this
      }

      def decode(head: (Payload[_ <: BaseType], Any), tail : (Payload[_ <: BaseType], Any)*): this.type = {
        eu.addDecoding(microOp, head +:tail)
        this
      }


      def srcs(srcKeys: Seq[SrcKeys]): this.type = {
        if (srcKeys.nonEmpty) host.find[SrcPlugin](_.laneName == euId).specify(microOp, srcKeys)
        this
      }

      def srcs(head: SrcKeys, tail : SrcKeys*): this.type = {
        this.srcs(head +: tail)
        this
      }
    }
  }
}