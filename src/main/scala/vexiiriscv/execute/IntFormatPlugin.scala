package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.riscv.{IntRegFile, MicroOp, Riscv}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IntFormatPlugin(val laneName : String) extends FiberPlugin{
  withPrefix(laneName)
  lazy val eu = host.find[ExecuteLanePlugin](_.laneName == laneName)
  lazy val wbp = host.find[WriteBackPlugin](p => p.laneName == laneName && p.rf == IntRegFile)
  buildBefore(eu.pipelineLock)
  buildBefore(wbp.elaborationLock)
  val elaborationLock = Lock()

  case class ExtendsSpec(op: MicroOp, bitId: Int)
  case class Spec(port : Flow[Bits],
                  ctrlId : Int){
    val microOps = mutable.LinkedHashSet[MicroOp]()
    val signExtends = ArrayBuffer[ExtendsSpec]()
    val zeroExtends = ArrayBuffer[ExtendsSpec]()
    def extendSpecs = (signExtends ++ zeroExtends)
  }
  val portToSpec = mutable.LinkedHashMap[Flow[Bits],Spec]()

  def access(ctrlId : Int) : Flow[Bits] = {
    val port = Flow(Bits(Riscv.XLEN bits))
    portToSpec(port) = Spec(port, ctrlId)
    port
  }

  def signExtend(port: Flow[Bits], microOp: MicroOp, bitId: Int) = {
    val spec = portToSpec(port)
    spec.signExtends += ExtendsSpec(microOp, bitId)
    spec.microOps += microOp
  }

  def zeroExtend(port: Flow[Bits], microOp: MicroOp, bitId: Int) = {
    val spec = portToSpec(port)
    spec.zeroExtends += ExtendsSpec(microOp, bitId)
    spec.microOps += microOp
  }

  def addMicroOp(port: Flow[Bits], microOp: MicroOp) : Unit = {
    val spec = portToSpec(port)
    spec.microOps += microOp
    eu.setCompletion(portToSpec(port).ctrlId, microOp)
  }

  val logic = during build new Area{
    elaborationLock.await()
    val specs = portToSpec.values
    val grouped = specs.groupByLinked(_.ctrlId)

    for(spec <- specs){
      val fullwidths = spec.microOps.toSet -- spec.extendSpecs.map(_.op)
      spec.zeroExtends ++= fullwidths.map(uop => ExtendsSpec(uop, IntRegFile.width))
    }

    val widths = specs.flatMap(spec => spec.extendSpecs.map(_.bitId)).toSeq.distinct.sorted
    val widthsToId = widths.zipWithIndex.toMap
    val wiw = log2Up(widths.size)

    val WIDTH_ID = Payload(UInt(wiw bits))
    val SIGNED = Payload(Bool())

    for(spec <- specs) {
      for(ext <- spec.signExtends){
        eu.addDecoding(ext.op, SIGNED -> True, WIDTH_ID ->  U(widthsToId(ext.bitId), wiw bits))
      }
      for (ext <- spec.zeroExtends) {
        eu.addDecoding(ext.op, SIGNED -> False, WIDTH_ID -> U(widthsToId(ext.bitId), wiw bits))
      }
    }


    val stages = for(group <- grouped.values) yield new Area{
      val stageId = group.head.ctrlId
      val stage = eu.execute(stageId)
      import stage._

      val wb = wbp.createPort(stageId)
      for(spec <- group) wbp.addMicroOp(wb, spec.microOps.toSeq)

      val hits = B(group.map(_.port.valid))
      wb.valid := isValid && hits.orR

      val raw = MuxOH.or(hits, group.map(_.port.payload), true)
      wb.payload := raw

      val extendSpecs = group.flatMap(_.extendSpecs)
      val extendBitIds = extendSpecs.map(_.bitId).distinct.sorted

      var from = extendBitIds.head
      var widthId = 0
      val segments = for(to <- extendBitIds.tail) yield new Area {
        val width = to-from
        val signeds = group.flatMap(_.signExtends.map(_.bitId))
        val sign = signeds.nonEmpty generate new Area{
          val widths = signeds.filter(_ < to)
          val sels = widths.map(bitId => raw(bitId-1))
          val mapping = (widths.map(widthsToId), sels).zipped.toSeq
          val value = SIGNED && WIDTH_ID.muxListDc(mapping)
        }

        val doIt = WIDTH_ID <= widthId
        when(doIt) {
          wb.payload(from, width bits) := (if(signeds.nonEmpty) sign.value #* width else B(0))
        }
        widthId += 1
        from = to
      }
    }
  }
}
