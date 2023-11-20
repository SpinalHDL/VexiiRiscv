package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.{Flow, MuxOH}
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.riscv.{MicroOp, Riscv}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IntFormatPlugin(val euId : String) extends FiberPlugin{
  withPrefix(euId)
  lazy val eu = host.find[ExecuteUnitPlugin](_.euId == euId)
  lazy val wbp = host.find[WriteBackPlugin](_.euId == euId)
  addLockable(eu.pipelineLock)
  addLockable(wbp)

  case class SignExtend(op : MicroOp, bitId : Int)
  case class Spec(port : Flow[Bits],
                  ctrlId : Int){
    val signExtends = ArrayBuffer[SignExtend]()
  }
  val portToSpec = mutable.LinkedHashMap[Flow[Bits],Spec]()

  def access(ctrlId : Int) : Flow[Bits] = {
    val port = Flow(Bits(Riscv.XLEN bits))
    portToSpec(port) = Spec(port, ctrlId)
    port
  }

  def signExtend(port : Flow[Bits], microOp: MicroOp, bitId : Int) = {
    portToSpec(port).signExtends += SignExtend(microOp, bitId)
  }

  val logic = during build new Area{
    val specs = portToSpec.values
    val grouped = specs.groupByLinked(_.ctrlId)
    val stages = for(group <- grouped.values) yield new Area{
      val stageId = group.head.ctrlId
      val stage = eu.execute(stageId)

      val wb = wbp.createPort(stageId)
      val hits = B(group.map(_.port.valid))
      wb.valid := stage.isValid && hits.orR

      val signExtendsGroups = group.flatMap(_.signExtends).groupByLinked(_.bitId).map(_._2)
      val raw = MuxOH.or(hits, group.map(_.port.payload), true)
      wb.payload := raw

      val signExtend = for(seg <- signExtendsGroups) yield new Area{
        val bitId = seg.head.bitId
        val DO_IT = SignalKey(Bool())
        eu.setDecodingDefault(DO_IT, False)
        for(e <- seg){
          eu.addDecoding(e.op, decode.DecodeList(DO_IT -> True))
        }
        when(stage(DO_IT)){
          wb.payload(Riscv.XLEN.get-1 downto bitId+1) := (default -> raw(bitId))
        }
      }
    }
  }
}
