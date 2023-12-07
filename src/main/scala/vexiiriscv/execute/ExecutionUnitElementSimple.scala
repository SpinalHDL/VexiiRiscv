package vexiiriscv.execute

import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core._
import spinal.lib.Flow
import vexiiriscv.decode.DecodeListType
import vexiiriscv.riscv.{MicroOp, RD, RfResource}


object ExecuteUnitElementSimple{
  class Api(implName : LaneLayer, val srcPlugin: SrcPlugin, val SEL : Payload[Bool], val rsUnsignedPlugin: RsUnsignedPlugin = null){
    def add(microOp: MicroOp)(implicit iifpp: (IntFormatPlugin, Flow[Bits]) = null,
                                       iwbpp: (WriteBackPlugin, Flow[Bits]) = null) = new {
      val impl = implName.add(microOp)

      decode(SEL -> True)

      if (iifpp != null) iifpp._1.addMicroOp(iifpp._2, impl)
      if (iwbpp != null) iwbpp._1.addMicroOp(iwbpp._2, impl)

      def decode(decoding: DecodeListType = Nil): this.type = {
        impl.addDecoding(decoding)
        this
      }

      def decode(head: (Payload[_ <: BaseType], Any), tail: (Payload[_ <: BaseType], Any)*): this.type = {
        impl.addDecoding(head +: tail)
        this
      }

      def srcs(srcKeys: Seq[SrcKeys]): this.type = {
        if (srcKeys.nonEmpty) srcPlugin.specify(impl, srcKeys)
        this
      }

      def srcs(head: SrcKeys, tail: SrcKeys*): this.type = {
        this.srcs(head +: tail)
        this
      }

      def rsUnsigned(rs1Signed : Boolean, rs2Signed : Boolean) : this.type = {
        rsUnsignedPlugin.addUop(impl, rs1Signed, rs2Signed)
        this
      }
    }
  }
}

//This is a simple skeleton to ease the implementation of simple ExecutionUnit elements. It assume a single writeback and a single completion
abstract class ExecutionUnitElementSimple(layer : LaneLayer) extends FiberPlugin {
  val eu = layer.el
  lazy val srcp = host.find[SrcPlugin](_.layer == layer)
  buildBefore(eu.pipelineLock)
  setupRetain(eu.uopLock)
  setupRetain(srcp.elaborationLock)
  withPrefix(layer.name)

  val SEL = Payload(Bool())

  class Logic extends ExecuteUnitElementSimple.Api(layer, srcp, SEL, rsUnsignedPlugin = host.get[RsUnsignedPlugin].getOrElse(null)) with Area {
    eu.setDecodingDefault(SEL, False)
  }
}