package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.regfile.{RegFileWriter, RegFileWriterService}
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class FpuAddSharedCmd(p1 : FloatUnpackedParam, p2 : FloatUnpackedParam, ats : Seq[Int]) extends Bundle{
  val at = Bits(ats.size bits)
  val rs1 = FloatUnpacked(p1)
  val rs2 = FloatUnpacked(p2)
  val format = FpuFormat()
  val roundMode = FpuRoundMode()
  val hartId = Global.HART_ID()
  val uopId = Decode.UOP_ID()
  val flags = FpuFlags()
}

class FpuAddSharedPort(_cmd : FpuAddSharedCmd) extends Area{
  val uopsAt = ArrayBuffer[(UopLayerSpec, Int)]()
  val cmd = _cmd
}

class FpuAddSharedPlugin(lane: ExecuteLanePlugin,
                         preShiftStage : Int = 0,
                         shifterStage : Int = 1,
                         mathStage : Int = 2,
                         normStage : Int = 3,
                         packAt : Int = 4) extends FiberPlugin {
  val p = FpuUtils

  val elaborationLock = Retainer()

  val ports = ArrayBuffer[FpuAddSharedPort]()
  def createPort(ats : Seq[Int], p1 : FloatUnpackedParam, p2 : FloatUnpackedParam): FpuAddSharedPort = {
    ports.addRet(new FpuAddSharedPort(FpuAddSharedCmd(p1, p2, ats)))
  }

  val logic = during setup new Area{
    val fpp = host.find[FpuPackerPlugin](p => p.lane == lane)
    val buildBefore = retains(lane.pipelineLock, fpp.elaborationLock)
    val uopLock = retains(lane.uopLock)
    awaitBuild()
    val latency = packAt

    elaborationLock.await()

    val p1Param = ports.map(_.cmd.p1).reduce(_ union _)
    val p2Param = ports.map(_.cmd.p2).reduce(_ union _)

    val packParam = FloatUnpackedParam(
      exponentMax = (p1Param.exponentMax max p2Param.exponentMax)+1,
      exponentMin = (p1Param.exponentMin min p2Param.exponentMin) - Math.max(p1Param.mantissaWidth, p2Param.mantissaWidth) - 1,
      mantissaWidth = FpuUtils.mantissaWidth+2
    )

    val completion = Flow(CompletionPayload())

    val uopsAt = mutable.LinkedHashMap[Int, ArrayBuffer[UopLayerSpec]]()
    for (port <- ports; (uop, at) <- port.uopsAt) uopsAt.getOrElseUpdate(at, ArrayBuffer[UopLayerSpec]()) += uop
    val packAts = uopsAt.keys.map(_ + latency).toList
    val packPort = fpp.createPort(packAts, packParam)
    for ((at, uops) <- uopsAt) {
//      wbPorts(at) = port
      for (uop <- uops) {
        packPort.uopsAt += uop -> (at + latency)
        uop.reserve(FpuAddSharedPlugin.this, at)
      }
    }

    uopLock.release()

    val pip = new StagePipeline()

    val inserter = new pip.Area(0){
      val portsRs1 = ports.map(_.cmd.rs1.to(p1Param))
      val portsRs2 = ports.map(_.cmd.rs2.to(p2Param))

      val reader = ports.map(_.cmd).reader(ports.map(_.cmd.at.orR))
      val rs1 = insert(OhMux.or(reader.oh.asBits, portsRs1.toSeq))
      val rs2 = insert(OhMux.or(reader.oh.asBits, portsRs2.toSeq))
      val FORMAT = insert(reader(_.format))
      val ROUNDMODE = insert(reader(_.roundMode))
      val RDN = insert( ROUNDMODE === FpuRoundMode.RDN)
      val FLAGS = insert(reader(_.flags))
      Global.HART_ID := reader(_.hartId)
      Decode.UOP_ID := reader(_.uopId)
      valid := reader.oh.orR
      val GROUP_OH = Payload(Bits(uopsAt.size bits))
      when(isValid) {
        assert(CountOne(GROUP_OH) <= 1)
      }
      for ((at, sel) <- (uopsAt.keys, GROUP_OH.asBools).zipped) {
        sel := (for (port <- ports; (portAt, i) <- port.cmd.ats.zipWithIndex; if portAt == at) yield port.cmd.at(i)).orR
      }
    }

    val adder = new FpuAdd(
      rs1           = inserter.rs1,
      rs2           = inserter.rs2,
      roundDown     = inserter.RDN,
      preShiftStage = pip.node(preShiftStage),
      shifterStage  = pip.node(shifterStage),
      mathStage     = pip.node(mathStage),
      normStage     = pip.node(normStage),
      resultStage   = pip.node(packAt),
    )

    val onPack = new pip.Area(packAt) {
      val mask = (inserter.GROUP_OH & uopsAt.map(e => lane.execute(e._1+packAt).isValid).asBits)

      packPort.cmd.at             := mask
      packPort.cmd.value.mode     := adder.result.RESULT.mode
      packPort.cmd.value.quiet    := adder.result.RESULT.quiet
      packPort.cmd.value.sign     := adder.result.RESULT.sign
      packPort.cmd.value.exponent := adder.result.RESULT.exponent
      packPort.cmd.value.mantissa := adder.result.RESULT.mantissa.rounded(RoundType.SCRAP)
      packPort.cmd.format         := inserter.FORMAT
      packPort.cmd.roundMode      := inserter.ROUNDMODE
      packPort.cmd.hartId         := Global.HART_ID
      packPort.cmd.uopId          := Decode.UOP_ID
      packPort.cmd.flags          := inserter.FLAGS
      packPort.cmd.flags.NV.setWhen(adder.result.NV)

      ready := !lane.isFreezed()
    }

    pip.build(withoutCollapse = true)
    buildBefore.release()
  }
}
