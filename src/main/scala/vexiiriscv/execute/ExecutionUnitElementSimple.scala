package vexiiriscv.execute

import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core._
import spinal.idslplugin.PostInitCallback
import spinal.lib.Flow
import vexiiriscv.decode.DecodeListType
import vexiiriscv.execute.RsUnsignedPlugin.RS1_SIGNED
import vexiiriscv.riscv.{MicroOp, RD, RfResource}

import scala.collection.mutable.ArrayBuffer


object ExecuteUnitElementSimple{
  class Api(layer : LaneLayer, val srcPlugin: SrcPlugin, val SEL : Payload[Bool], val rsUnsignedPlugin: RsUnsignedPlugin = null){
    var iwbpp = Option.empty[(IntFormatPlugin, Flow[Bits])]
    def setWriteback(ifp : IntFormatPlugin, bus : Flow[Bits]): Unit = {
      iwbpp = Some(ifp -> bus)
    }
    def newWriteback(ifp: IntFormatPlugin, at : Int) : Flow[Bits] = {
      val bus = ifp.access(at)
      setWriteback(ifp, bus)
      bus
    }

    val uopList = ArrayBuffer[MicroOp]()
    def add(microOp: MicroOp) = new {
      val uop = microOp
      uopList += microOp
      val impl = layer.add(microOp)
      val spec = layer(microOp)

      decode(SEL -> True)

      iwbpp.foreach(v => v._1.addMicroOp(v._2, impl))

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
    val el = layer.el
    val srcp = srcPlugin
    val ifp = host.find[IntFormatPlugin](_.lane == layer.el)
    val uopRetainer = retains(el.uopLock, srcp.elaborationLock, ifp.elaborationLock)
    val euPipelineRetainer = retains(el.pipelineLock)

    el.setDecodingDefault(SEL, False)

    override def postInitCallback() = {
      euPipelineRetainer.release()
      this
    }
  }
}