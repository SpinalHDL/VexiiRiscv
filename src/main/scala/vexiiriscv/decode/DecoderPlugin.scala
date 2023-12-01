package vexiiriscv.decode

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{CtrlLink, Link, Payload}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.ExecuteLaneService
import vexiiriscv.fetch.FetchPipelinePlugin
import vexiiriscv.misc.PipelineService
import vexiiriscv.{Global, riscv}
import Decode._
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DecoderPlugin(var decodeAt : Int) extends FiberPlugin with DecoderService{
  lazy val dpp = host[DecodePipelinePlugin]
  buildBefore(dpp.elaborationLock)

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
    host.list[ExecuteLaneService].flatMap(_.getMicroOp()).map(e => Masked(e.key))
  }

  val logic = during build new Area{
    elaborationLock.await()

    Decode.INSTRUCTION_WIDTH.set(32)

    val eus = host.list[ExecuteLaneService]
    val microOps = eus.flatMap(_.getMicroOp())
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
      val ak = AccessKeys(physWidth, rfMapping)
      ak.setCompositeName(rfa)
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
      when(decodeCtrl.link.down.isFiring && decodeCtrl.link(Global.HART_ID) === hartId) {
        uopId := uopId + CountOne(laneIds.map(decodeCtrl.lane).map(_.down.isFiring))//decodeCtrl.lane(Decode.LANES - 1)(Decode.UOP_ID) + 1
      }
    }

    val laneLogic = for(laneId <- 0 until Decode.LANES) yield new decodeCtrl.LaneArea(laneId) {
      LEGAL := Symplify(Decode.INSTRUCTION, encodings.all)
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

      Decode.UOP_ID := (laneId match {
        case 0 => harts.map(_.uopId).read(decodeCtrl.link(Global.HART_ID))
        case _ => decodeCtrl.lane(laneId-1)(Decode.UOP_ID) + decodeCtrl.lane(laneId-1).isValid.asUInt
      })
    }
  }
}
