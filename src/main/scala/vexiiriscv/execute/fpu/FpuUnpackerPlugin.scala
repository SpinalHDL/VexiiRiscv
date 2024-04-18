package vexiiriscv.execute.fpu

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class FpuUnpackerPlugin(val layer : LaneLayer, unpackAt : Int = 0) extends FiberPlugin{
  val p = FpuUtils

  val elaborationLock = Retainer()

  val unpackSpec = mutable.LinkedHashMap[RfRead, mutable.LinkedHashSet[MicroOp]]()
  def unpack(uop : MicroOp, rs : RfRead) = {
    unpackSpec.getOrElseUpdate(rs, mutable.LinkedHashSet[MicroOp]()) += uop
  }

  def apply(rs : RfRead) = {
    logic.onUnpack.rs.toList(logic.rsList.toList.indexOf(rs)).RS
  }

  def unpackingDone(at : Int) : Bool = at match {
    case unpackAt => !logic.onUnpack.rs.map(_.normalizer.freezeIt).toList.orR
    case _ => True
  }

  val logic = during setup new Area{
    val buildBefore = retains(layer.el.pipelineLock)
    val uopLock = retains(layer.el.uopLock)
    awaitBuild()

    elaborationLock.await()

    for((rs, uops) <- unpackSpec; uop <- uops) layer(uop).addRsSpec(rs, 0)
    uopLock.release()

    val rsList = unpackSpec.keys.toArray

    val unpacker = new StagePipeline {
      val ohInputWidth = p.rsIntWidth max Riscv.fpuMantissaWidth

      case class Request() extends Bundle {
        val data = Bits(ohInputWidth bits)
      }

      case class Result() extends Bundle {
        val shift = UInt(log2Up(ohInputWidth + 1) bits)
        val data = Bits(ohInputWidth bits)
      }

      val portCount = 1 //TODO
      val arbiter = StreamArbiterFactory().noLock.lowerFirst.build(Request(), portCount)
      val results = Vec.fill(portCount)(Flow(Result()))

      val input = new Area(0) {
        valid := arbiter.io.output.valid
        arbiter.io.output.ready := True

        val args = insert(arbiter.io.output.payload)
        val source = insert(arbiter.io.chosen)
      }

      import input._

      val setup = new Area(1) {
        val shiftBy = insert(OHToUInt(OHMasking.firstV2(args.data.reversed << 1)))
      }
      import setup._

      val logic = new Area(2) {
        val shifter = args.data |<< shiftBy

        for ((port, id) <- results.zipWithIndex) {
          port.valid := False
          port.data := shifter
          port.shift := shiftBy
        }

        when(isValid) {
          results(source).valid := True
        }
      }
    }


    val onUnpack = new layer.el.Execute(unpackAt){
      val fsmPortId = 0
      val fsmCmd = unpacker.arbiter.io.inputs(fsmPortId)
      val fsmRsp = unpacker.results(fsmPortId)
      fsmCmd.setIdle()

      val rsValues = rsList.map(rs => this(layer.el(FloatRegFile, rs)))

      val fsmRequesters = Bits(rsList.size bits)
      val fsmServed = Bits(rsList.size bits)

      val clear = isReady || isCancel
      val rs = for ((input, inputId) <- rsValues.zipWithIndex) yield new Area {
        val rfRead = rsList(inputId)
        val rfa = Decode.rfaKeys.get(rfRead)
        setName("FpuUnpack_" + rfRead.getName)
        val RS_PRE_NORM = Payload(FloatUnpacked(
          exponentMax = (1 << p.exponentWidth - 1) - 1,
          exponentMin = -(1 << p.exponentWidth - 1) + 1,
          mantissaWidth = Riscv.fpuMantissaWidth
        ))
        val RS = Payload(FloatUnpacked(
          exponentMax = (1 << p.exponentWidth - 1) - 1,
          exponentMin = -(1 << p.exponentWidth - 1) + 1 - Riscv.fpuMantissaWidth,
          mantissaWidth = Riscv.fpuMantissaWidth
        ))

        val unpackerSel = isValid && rfa.ENABLE && rfa.is(FloatRegFile, rfa.RFID) //A bit pessimistic, as not all float instruction will need unpacking

        val f32 = new Area {
          val mantissa = input(0, 23 bits).asUInt
          val exponent = input(23, 8 bits).asUInt
          val sign = input(31)
        }
        val f64 = p.rvd generate new Area {
          val mantissa = input(0, 52 bits).asUInt
          val exponent = input(52, 11 bits).asUInt
          val sign = input(63)
        }

        val manZero = Bool()
        val expZero = Bool()
        val expOne = Bool()
        val isSubnormal = expZero && !manZero
        val recodedExpOffset = UInt(p.exponentWidth bits)
        val recodedExpSub = SInt(p.exponentWidth + 1 bits)
        val expRaw = UInt(p.exponentWidth bits)

        p.whenDouble(p.FORMAT) {
          RS_PRE_NORM.sign := f64.sign
          expRaw := f64.exponent.resized
          RS_PRE_NORM.mantissa.raw := B(f64.mantissa)
          RS_PRE_NORM.quiet := f64.mantissa.msb
          manZero := f64.mantissa === 0
          expZero := f64.exponent === 0
          expOne := f64.exponent.andR
          recodedExpOffset := p.exponentF64One
          recodedExpSub := -p.exponentF64One + 1
        } {
          RS_PRE_NORM.sign := f32.sign
          expRaw := f32.exponent.resized
          RS_PRE_NORM.quiet := f32.mantissa.msb
          RS_PRE_NORM.mantissa.raw := B(f32.mantissa << (if (p.rvd) 29 else 0))
          manZero := f32.mantissa === 0
          expZero := f32.exponent === 0
          expOne := f32.exponent.andR
          recodedExpOffset := p.exponentF32One
          recodedExpSub := -p.exponentF32One + 1
        }
        RS_PRE_NORM.exponent := expRaw - recodedExpOffset
        RS_PRE_NORM.mode := (expOne ## expZero).mux(
          default -> FloatMode.NORMAL(),
          1 -> (manZero ? FloatMode.ZERO | FloatMode.NORMAL),
          2 -> (manZero ? FloatMode.INF | FloatMode.NAN)
        )
        apply(RS) := RS_PRE_NORM
        val normalizer = new Area {
          val valid = unpackerSel && isSubnormal
          val asked = RegInit(False) setWhen (fsmRequesters(inputId) && !fsmRequesters.dropLow(inputId + 1).orR) clearWhen (clear)
          val served = RegInit(False) setWhen (fsmRsp.valid && fsmServed.dropLow(inputId + 1).andR) clearWhen (clear)
          fsmRequesters(inputId) := valid && !asked
          fsmServed(inputId) := !valid || served

          val exponent = Reg(RS.exponent)
          val mantissa = Reg(RS.mantissa)

          when(fsmRequesters(inputId)) {
            fsmCmd.valid := True
            fsmCmd.data := RS_PRE_NORM.mantissa.raw << widthOf(fsmCmd.data) - widthOf(RS_PRE_NORM.mantissa.raw)
          }
          when(asked) {
            RS.exponent := exponent
            RS.mantissa := mantissa
          }
          when(!served) {
            exponent := recodedExpSub - fsmRsp.shift.intoSInt
            mantissa.raw := fsmRsp.data >> widthOf(fsmCmd.data) - widthOf(RS_PRE_NORM.mantissa.raw)
          }
          val freezeIt = valid && !served
          layer.el.freezeWhen(freezeIt)
        }

        if (p.rvd) when(p.FORMAT === FpuFormat.FLOAT && !input(63 downto 32).andR) {
          RS.setNanQuiet
          RS.sign := False
          RS.exponent := AFix(128)
          RS.mantissa.raw := (default -> False, RS.mantissa.raw.high -> True)
        }
      }
    }
    buildBefore.release()
    unpacker.build()
  }
}
