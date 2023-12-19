package vexiiriscv.execute

import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core._
import spinal.idslplugin.PostInitCallback
import spinal.lib.Flow
import vexiiriscv.decode.DecodeListType
import vexiiriscv.execute.RsUnsignedPlugin.RS1_SIGNED
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

      def rsUnsigned(rs1Signed : Boolean, rs2Signed : Boolean, cond : Boolean = true) : this.type = {
        if(cond) rsUnsignedPlugin.addUop(impl, rs1Signed, rs2Signed)
        else impl.addDecoding(RsUnsignedPlugin.RS1_SIGNED -> Bool(rs1Signed), RsUnsignedPlugin.RS2_SIGNED -> Bool(rs2Signed))
        this
      }
    }
  }
}

//This is a simple skeleton to ease the implementation of simple ExecutionUnit elements. It assume a single writeback and a single completion
abstract class ExecutionUnitElementSimple(layer : LaneLayer) extends FiberPlugin {
  withPrefix(layer.name)

  val SEL = Payload(Bool())

  class Logic extends ExecuteUnitElementSimple.Api(layer,  host.find[SrcPlugin](_.layer == layer), SEL, rsUnsignedPlugin = host.get[RsUnsignedPlugin].getOrElse(null)) with Area with PostInitCallback {
    val eu = layer.el
    val srcp = srcPlugin
    val ifp = host.find[IntFormatPlugin](_.laneName == layer.el.laneName)
    val uopRetainer = retains(eu.uopLock, srcp.elaborationLock, ifp.elaborationLock)
    val euPipelineRetainer = retains(eu.pipelineLock)

    eu.setDecodingDefault(SEL, False)

    override def postInitCallback() = {
      euPipelineRetainer.release()
      this
    }
  }
}