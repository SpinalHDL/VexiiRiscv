package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.riscv.{IntRegFile, MicroOp, Riscv}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IntFormatPlugin(val lane : ExecuteLanePlugin) extends FiberPlugin{
  withPrefix(lane.laneName)
  val elaborationLock = Retainer()

  case class ExtendsSpec(op: UopLayerSpec, bitId: Int)
  case class Spec(port : Flow[Bits],
                  ctrlId : Int){
    val impls = mutable.LinkedHashSet[UopLayerSpec]()
    val signExtends = ArrayBuffer[ExtendsSpec]()
    val zeroExtends = ArrayBuffer[ExtendsSpec]()
    def extendSpecs = (signExtends ++ zeroExtends)
  }
  val portToSpec = mutable.LinkedHashMap[Flow[Bits],Spec]()

  def access(executeId : Int) : Flow[Bits] = {
    val port = Flow(Bits(Riscv.XLEN bits))
    portToSpec(port) = Spec(port, executeId)
    port
  }

  def signExtend(port: Flow[Bits], impl: UopLayerSpec, bitId: Int) : Unit = {
    val spec = portToSpec(port)
    spec.signExtends += ExtendsSpec(impl, bitId)
    spec.impls += impl
  }

  def zeroExtend(port: Flow[Bits], impl: UopLayerSpec, bitId: Int) : Unit = {
    val spec = portToSpec(port)
    spec.zeroExtends += ExtendsSpec(impl, bitId)
    spec.impls += impl
  }

  def addMicroOp(port: Flow[Bits], impl: UopLayerSpec) : Unit = {
    val spec = portToSpec(port)
    spec.impls += impl
    impl.setCompletion(portToSpec(port).ctrlId)
  }

  val logic = during setup new Area{
    val eu = host.find[ExecuteLanePlugin](_ == lane)
    val wbp = host.find[WriteBackPlugin](p => p.lane == lane && p.rf == IntRegFile)
    val buildBefore = retains(eu.pipelineLock, wbp.elaborationLock)
    awaitBuild()

    elaborationLock.await()
    val specs = portToSpec.values
    val grouped = specs.groupByLinked(_.ctrlId)

    for(spec <- specs){
      val fullwidths = spec.impls.toSet -- spec.extendSpecs.map(_.op)
      spec.zeroExtends ++= fullwidths.map(uop => ExtendsSpec(uop, IntRegFile.width))
    }

    val widths = specs.flatMap(spec => spec.extendSpecs.map(_.bitId)).toSeq.distinct.sorted
    val widthsToId = widths.zipWithIndex.toMap
    val wiw = log2Up(widths.size)

    val WIDTH_ID = Payload(UInt(wiw bits))
    val SIGNED = Payload(Bool())

    for(spec <- specs) {
      for(ext <- spec.signExtends){
        ext.op.addDecoding(SIGNED -> True, WIDTH_ID ->  U(widthsToId(ext.bitId), wiw bits))
      }
      for (ext <- spec.zeroExtends) {
        ext.op.addDecoding(SIGNED -> False, WIDTH_ID -> U(widthsToId(ext.bitId), wiw bits))
      }
    }


    val stages = for(group <- grouped.values; stageId = group.head.ctrlId) yield new eu.Execute(stageId){
      val wb = wbp.createPort(stageId)
      for(spec <- group) wbp.addMicroOp(wb, spec.impls.toSeq)

      val hits = B(group.map(_.port.valid))
      wb.valid := isValid && hits.orR

      val raw = MuxOH.or(hits, group.map(_.port.payload), true)
      wb.payload := raw

      val extendSpecs = group.flatMap(_.extendSpecs)
      val extendBitIds = extendSpecs.map(_.bitId).distinct.sorted

      var from = extendBitIds.head
      val segments = for(to <- extendBitIds.tail) yield new Area { // For each independent section
        var widthId = widthsToId(to)
        val width = to-from
        val signeds = group.flatMap(_.signExtends.map(_.bitId))
        val sign = signeds.nonEmpty generate new Area{
          val widths = signeds.filter(_ < to).distinctLinked
          val sels = widths.map(bitId => raw(bitId-1))
          val mapping = (widths.map(widthsToId), sels).zipped.toSeq
          val value = SIGNED && WIDTH_ID.muxListDc(mapping)
        }

        val doIt = WIDTH_ID < widthId
        when(doIt) {
          wb.payload(from, width bits) := (if(signeds.nonEmpty) sign.value #* width else B(0))
        }
        from = to
      }
    }
    buildBefore.release()
  }
}
