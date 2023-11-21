package vexiiriscv.execute

import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core._
import spinal.lib.Flow
import vexiiriscv.decode.DecodeListType
import vexiiriscv.riscv.{MicroOp, RD, RfResource}


//This is a simple skeleton to ease the implementation of simple ExecutionUnit elements. It assume a single writeback and a single completion
abstract class ExecutionUnitElementSimple(euId : String, staticLatency : Boolean) extends FiberPlugin {
  lazy val eu = host.find[ExecuteUnitPlugin](_.euId == euId)
  addLockable(eu.pipelineLock)
  addRetain(eu)
  addLockable(host[SrcPlugin])
  withPrefix(euId)

  val SEL = Payload(Bool())

  class Logic(latency: Int) extends Area {
    case class ImplicitIntFormatPluginPort(ifp : IntFormatPlugin, port : Flow[Bits])
    def add(microOp: MicroOp)(implicit wbp : ImplicitIntFormatPluginPort = null) = new {
      eu.addMicroOp(microOp)
      decode(SEL -> True)
      if(wbp != null) wbp.ifp.addMicroOp(wbp.port, microOp)

      def decode(decoding: DecodeListType = Nil): this.type = {
        eu.addDecoding(microOp, decoding :+ (SEL -> True))
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
//  def euCompletionAt : Int = euWritebackAt
//  def euWritebackAt : Int
//
//  var _setup : Setup = null
//  class Setup extends Area {
//    val eu = host[ExecuteUnitPlugin](_.euId == euId)
//    eu.retain()
//
//    def add(microOp: MicroOp,  decoding: DecodeListType = Nil) = {
//      eu.addMicroOp(microOp)
//      eu.setLatency(microOp, euCompletionAt)
//      if (staticLatency && microOp.resources.exists{
//        case RfResource(_, RD) => true
//        case _ => false
//      })
////      eu.setStaticWake(microOp, euWritebackAt)
//      eu.addDecoding(microOp, decoding :+ (SEL -> True))
////      if (srcKeys.nonEmpty) {
////        findService[SrcPlugin](_.euId == euId).specify(microOp, srcKeys)
////      }
//    }
//
//    eu.setDecodingDefault(SEL, False)
//
////    val intFormat = findService[IntFormatPlugin](_.euId == euId)
////    val intFormatPort = intFormat.access(euWritebackAt, 1 - staticLatency.toInt)
////    _setup = this
////
////    def signExtend(op : MicroOp, bitId : Int) = intFormat.signExtend(intFormatPort, op, 31)
//  }
//
//  class Logic extends Area with PostInitCallback{
//    val eu = findService[ExecutionUnitBase](_.euId == euId)
//    val decode = getService[DecoderService]
//
//    val wbStage = eu.getExecute(euWritebackAt)
//    val wb = _setup.intFormatPort
//    wb.valid := wbStage(SEL)
//
//    val wake = !staticLatency generate new Area{
//      val wakeRobsSel    = eu.newWakeRobsSelAt(euWritebackAt)
//      val wakeRegFileSel = eu.newWakeRegFileSelAt(euWritebackAt)
//
//      wakeRobsSel    := wbStage(SEL)
//      wakeRegFileSel := wbStage(SEL)
//    }
//
//    override def postInitCallback() = {eu.release() ; this}
//
//    class ExecuteArea(stageId : Int) extends Area{
//      val stage = eu.getExecute(stageId)
//    }
//  }
//
//  def setup : Handle[_ <: Setup]
//  def logic : Handle[_ <: Logic]
}