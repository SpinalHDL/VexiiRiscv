package vexiiriscv.execute

import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core._
import spinal.lib.Flow
import vexiiriscv.decode.DecodeListType
import vexiiriscv.riscv.{MicroOp, RD, RfResource}


//This is a simple skeleton to ease the implementation of simple ExecutionUnit elements. It assume a single writeback and a single completion
abstract class ExecutionUnitElementSimple(euId : String) extends FiberPlugin {
  lazy val eu = host.find[ExecuteUnitPlugin](_.euId == euId)
  addLockable(eu.pipelineLock)
  addRetain(eu)
  addLockable(host[SrcPlugin])
  withPrefix(euId)

  val SEL = Payload(Bool())

  class Logic extends Area {
    case class ImplicitIntFormatPluginPort(ifp : IntFormatPlugin, port : Flow[Bits])
    eu.setDecodingDefault(SEL, False)
    def add(microOp: MicroOp)(implicit wbp : ImplicitIntFormatPluginPort = null) = new {
      eu.addMicroOp(microOp)
      decode(SEL -> True)
      if(wbp != null) wbp.ifp.addMicroOp(wbp.port, microOp)

      def decode(decoding: DecodeListType = Nil): this.type = {
        eu.addDecoding(microOp, decoding)
        this
      }

      def decode(head: (Payload[_ <: BaseType], Any), tail : (Payload[_ <: BaseType], Any)*): this.type = {
        eu.addDecoding(microOp, head +:tail)
        this
      }


      def srcs(srcKeys: Seq[SrcKeys]): this.type = {
        if (srcKeys.nonEmpty) host.find[SrcPlugin](_.euId == euId).specify(microOp, srcKeys)
        this
      }

      def srcs(head: SrcKeys, tail : SrcKeys*): this.type = {
        this.srcs(head +: tail)
        this
      }
    }
  }
}