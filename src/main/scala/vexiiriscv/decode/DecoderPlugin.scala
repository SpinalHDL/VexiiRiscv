package vexiiriscv.decode

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{CtrlLink, Link, Payload}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.{CompletionPayload, CompletionService, ExecuteLaneService}
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.misc.{PipelineService, PrivilegedPlugin, TrapReason, TrapService}
import vexiiriscv.{Global, riscv}
import Decode._
import spinal.core
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import vexiiriscv.prediction.{FetchWordPrediction, ForgetCmd, ForgetSource, Prediction}
import vexiiriscv.riscv._
import vexiiriscv.schedule.{Ages, ScheduleService}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DecoderPlugin(var decodeAt : Int) extends FiberPlugin with DecoderService with CompletionService with ForgetSource{

  def getForgetPort() = logic.forgetPort
  override def getCompletions(): Seq[Flow[CompletionPayload]] = logic.laneLogic.map(_.completionPort)

  val decodingSpecs = mutable.LinkedHashMap[Payload[_ <: BaseType], DecodingSpec[_ <: BaseType]]()
  def getDecodingSpec(key: Payload[_ <: BaseType]) = decodingSpecs.getOrElseUpdate(key, new DecodingSpec(key))
  def setDecodingDefault(key: Payload[_ <: BaseType], value: BaseType): Unit = {
    getDecodingSpec(key).setDefault(Masked(value))
  }

  override def addMicroOpDecoding(microOp: MicroOp, decoding: DecodeListType) = {
    val op = Masked(microOp.key)
    for ((key, value) <- decoding) {
      getDecodingSpec(key).addNeeds(op, Masked(value))
    }
  }

  override def addMicroOpDecodingDefault(key: Payload[_ <: BaseType], value: BaseType) = {
    getDecodingSpec(key).setDefault(Masked(value))
  }

  override def covers() =  {
    elaborationLock.await()
    host.list[ExecuteLaneService].flatMap(_.getUops()).map(e => Masked(e.key))
  }



  val logic = during setup new Area{
    val dpp = host[DecodePipelinePlugin]
    val ts = host[TrapService]
    val ss = host[ScheduleService]
    val buildBefore = retains(dpp.elaborationLock, ts.trapLock, ss.elaborationLock)
    Decode.INSTRUCTION_WIDTH.soon()

    awaitBuild()

    elaborationLock.await()

    Decode.INSTRUCTION_WIDTH.set(32)

    val withPredictionFixer = host.get[FetchWordPrediction].nonEmpty
    val forgetPort = withPredictionFixer generate Flow(ForgetCmd()).setIdle()


    val eus = host.list[ExecuteLaneService]
    val microOps = eus.flatMap(_.getUops())
    val resources = microOps.flatMap(_.resources).distinctLinked
    val rfAccesses = mutable.LinkedHashSet[RfAccess]()
    resources.foreach{
      case r : RfResource => rfAccesses += r.access
      case _ =>
    }

    val rfaKeys = mutable.LinkedHashMap[RfAccess, AccessKeys]()
    for(rfa <- rfAccesses){
      val physWidth = 5
      val rfMapping = resources.collect{case r : RfResource if r.access == rfa => r.rf }.toList
      val ak = AccessKeys(rfa, physWidth, rfMapping)
      ak.setPartialName(rfa)
      rfaKeys(rfa) = ak
    }
    Decode.rfaKeys.set(rfaKeys)

    val singleDecodings = mutable.LinkedHashSet[SingleDecoding]()
    microOps.foreach {
      case sd: SingleDecoding => singleDecodings += sd
    }


    val encodings = new Area {
      val all = mutable.LinkedHashSet[Masked]()
      val one = Masked(1, 1)
      val zero = Masked(0, 1)
      class RfAccessDecoding(val rfa : RfAccess){
        val rfaKey = rfaKeys(rfa)
        val read = new DecodingSpec(Bool()).setDefault(zero)
        val rfid = new DecodingSpec(UInt(rfaKey.rfIdWidth bits))
      }
      val rfAccessDec = rfAccesses.map(rfa => rfa -> new RfAccessDecoding(rfa)).toMapLinked()

      for (e <- singleDecodings) {
        val key = Masked(e.key)
        all += key

        e.resources.foreach {
          case r: RfResource => {
            val dec = rfAccessDec(r.access)
            dec.read.addNeeds(key, one)
            dec.rfid.addNeeds(key,  Masked(dec.rfaKey.idOf(r.rf), (1 << dec.rfaKey.rfIdWidth)-1))
          }
          case PC_READ =>
          case INSTRUCTION_SIZE =>
          case LQ =>
          case vexiiriscv.riscv.SQ =>
        }
      }
    }

    val decodeCtrl = dpp.ctrl(decodeAt)
    val harts = for (hartId <- Global.hartsIds) yield new Area {
      val uopId = Reg(Decode.UOP_ID) init (0)
      when(decodeCtrl.link.up.isMoving && decodeCtrl.link(Global.HART_ID) === hartId) {
        uopId := uopId + Decode.LANES.get
      }
    }

    val interrupt = new Area {
      val async = B(host[PrivilegedPlugin].logic.harts.map(_.int.pending))
      //We need to buffer interrupts request to ensure we don't generate sporadic flushes while the ctrl is stuck
      val buffered = RegNextWhen(async, !decodeCtrl.link.up.valid || decodeCtrl.link.up.ready || decodeCtrl.link.up.isCanceling) init(0)
    }

    val predictionSpec = new Area {
      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
      val jalKeys = List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key))
      val any = new DecodingSpec(Bool()).setDefault(Masked.zero)
      any.addNeeds(branchKeys ++ jalKeys, Masked.one)
    }

    val laneLogic = for(laneId <- 0 until Decode.LANES) yield new decodeCtrl.LaneArea(laneId) {
      for(rfa <- rfAccesses){
        val keys = rfaKeys(rfa)
        val dec = encodings.rfAccessDec(rfa)
        keys.ENABLE := dec.read.build(Decode.INSTRUCTION, encodings.all)
        keys.RFID := dec.rfid.build(Decode.INSTRUCTION, encodings.all)
        keys.PHYS := Decode.INSTRUCTION(rfa match {
          case RS1 => riscv.Const.rs1Range
          case RS2 => riscv.Const.rs2Range
          case RS3 => riscv.Const.rs3Range
          case RD  => riscv.Const.rdRange
        }).asUInt
      }

      LEGAL := Symplify(Decode.INSTRUCTION, encodings.all) && !Decode.DECOMPRESSION_FAULT


      val interruptPending = interrupt.buffered(Global.HART_ID)
      val trapPort = ts.newTrap(dpp.getAge(decodeAt), Decode.LANES)
      trapPort.valid := False
      trapPort.exception := True
      trapPort.tval := Decode.INSTRUCTION_RAW.resized
      trapPort.code := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      trapPort.laneAge := laneId
      trapPort.hartId := Global.HART_ID
      trapPort.arg := 0

      val fixer = withPredictionFixer generate new Area{
        val isJb = predictionSpec.any.build(Decode.INSTRUCTION, encodings.all)
        val doIt = up.isValid && (Prediction.ALIGNED_JUMPED && !isJb || Prediction.ALIGN_REDO)
        when(doIt) {
          trapPort.exception := False
          trapPort.code := TrapReason.REDO
          forgetPort.valid := True
          forgetPort.hartId := Global.HART_ID
          forgetPort.pcOnLastSlice := Global.PC + (Decode.INSTRUCTION_SLICE_COUNT << Fetch.SLICE_RANGE_LOW.get).andMask(!Prediction.ALIGN_REDO)
          assert(Decode.INSTRUCTION_SLICE_COUNT_MAX < 3)
          //Prediction.ALIGN_REDO mean that a branch prediction cuted a instruction fetch
          //Prediction.ALIGNED_JUMPED && !isJb mean that it predicted a taken jump instruction where it wasn't a jump even an instruction to begin width
        }
      }

      when(interruptPending){
        trapPort.exception := False
        trapPort.code := TrapReason.INTERRUPT
      }

      val completionPort = Flow(CompletionPayload())
      completionPort.valid := isValid && Global.TRAP && up.transactionSpawn
      completionPort.hartId := Global.HART_ID
      completionPort.uopId := Decode.UOP_ID
      completionPort.trap := True
      completionPort.commit := False

      when(isValid && (!LEGAL || interruptPending || withPredictionFixer.mux(fixer.doIt, False))) {
        bypass(Global.TRAP) := True
        trapPort.valid := !up(Global.TRAP) || interruptPending
        if(withPredictionFixer) trapPort.valid setWhen(fixer.doIt)
      }

      //Will also flush instructions after a fetch trap
      val flushPort = ss.newFlushPort(dpp.getAge(decodeAt), log2Up(Decode.LANES), true)
      flushPort.valid := isValid && Global.TRAP
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := laneId
      flushPort.self := False

      // Clear RD.ENABLE if it select a regfile which doesn't allow writing x0
      val x0Logic = Decode.rfaKeys.get.get(RD) map { rfaRd =>
        val rdRfidZero = rfaRd.rfMapping.zipWithIndex.filter(_._1.x0AlwaysZero).map(_._2)
        val rdZero = Decode.INSTRUCTION(riscv.Const.rdRange) === 0 && rdRfidZero.map(rfaRd.RFID === _).orR
        rfaRd.ENABLE clearWhen (rdZero)
      }

      val microOpDecoding = new Area {
        for ((key, spec) <- decodingSpecs) {
          key.assignFromBits(spec.build(Decode.INSTRUCTION, encodings.all).asBits)
        }
      }

      Decode.UOP := Decode.INSTRUCTION

      val uopIdBase = harts.map(_.uopId).read(decodeCtrl.link(Global.HART_ID))
      Decode.UOP_ID := uopIdBase + laneId
    }

    buildBefore.release()
  }
}
