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
import FpuUtils._
import vexiiriscv.Global.TRAP

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Recode the floating point number comming from the register file to get rid of subnormal numbers by extending the exponant range instead.
 * Getting ride of subnormal helps various floating point operations, as it ensure that the range of the mantissa stay between [1.0, 2.0[
 *
 * When a subnormal number come in, the pipeline will be stuck for a few cycles in order to preserve the FMax.
 */
class FpuUnpackerPlugin(val layer : LaneLayer,
                        var ignoreSubnormal : Boolean = false,
                        unpackAt : Int = 0,
                        packAt : Int = 0) extends FiberPlugin{
  val p = FpuUtils

  val elaborationLock = Retainer()

  val unpackSpec = mutable.LinkedHashMap[RfRead, mutable.LinkedHashSet[MicroOp]]()
  def unpack(uop : MicroOp, rs : RfRead) = {
    unpackSpec.getOrElseUpdate(rs, mutable.LinkedHashSet[MicroOp]()) += uop
  }

  def apply(rs : RfRead) = {
    logic.onUnpack.rs.toList(logic.rsList.toList.indexOf(rs)).RS
  }

  def getSubnormal(rs : RfRead) : Payload[Bool] = {
    logic.onUnpack.rs.toList(logic.rsList.toList.indexOf(rs)).IS_SUBNORMAL
  }

  def getBadBoxing(rs: RfRead): Payload[Bool] = {
    logic.onUnpack.rs.toList(logic.rsList.toList.indexOf(rs)).badBoxing.HIT
  }

  def unpackingDone(at : Int) : Bool = at match {
    case `unpackAt` => logic.unpackDone
    case _ => True
  }

  val SEL_I2F = Payload(Bool())

  val logic = during setup new Area{
    val fpp = host[FpuPackerPlugin]
    val rsUnsignedPlugin = host[RsUnsignedPlugin]
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fpp.elaborationLock, rsUnsignedPlugin.elaborationLock)
    awaitBuild()

    val packParam = FloatUnpackedParam(
      exponentMax = p.rsIntWidth,
      exponentMin = 0,
      mantissaWidth = p.mantissaWidth + 2
    )
    val packPort = fpp.createPort(List(packAt), packParam)

    elaborationLock.await()

    layer.lane.setDecodingDefault(SEL_I2F, False)
    def i2f(uop: MicroOp, size: Int, signed : Boolean, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(decodings)
      spec.addDecoding(SEL_I2F -> True)
      packPort.uopsAt += spec -> packAt
      spec.addDecoding(size match {
        case 32 => RsUnsignedPlugin.IS_W -> True
        case 64 => RsUnsignedPlugin.IS_W -> False
      })
      rsUnsignedPlugin.addUop(spec, signed)
    }

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    i2f(Rvfd.FCVT_S_WU, 32, false, f32)
    i2f(Rvfd.FCVT_S_W , 32, true , f32)
    if (Riscv.XLEN.get == 64) {
      i2f(Rvfd.FCVT_S_LU, 64, false, f32)
      i2f(Rvfd.FCVT_S_L , 64, true , f32)
    }
    if (Riscv.RVD) {
      i2f(Rvfd.FCVT_D_WU, 32, false, f64)
      i2f(Rvfd.FCVT_D_W , 32, true , f64)
      if (Riscv.XLEN.get == 64) {
        i2f(Rvfd.FCVT_D_LU, 64, false, f64)
        i2f(Rvfd.FCVT_D_L , 64, true , f64)
      }
    }

    for((rs, uops) <- unpackSpec; uop <- uops) layer(uop).addRsSpec(rs, 0)
    uopLock.release()

    val rsList = unpackSpec.keys.toArray

    val withRsUnpack = !ignoreSubnormal

    val unpacker = new StagePipeline { //TODO this kinda bloated now that all unpack are unified
      val ohInputWidth = p.rsIntWidth max Riscv.fpuMantissaWidth

      case class Request() extends Bundle {
        val data = Bits(ohInputWidth bits)
      }

      case class Result() extends Bundle {
        val shift = UInt(log2Up(ohInputWidth + 1) bits)
        val data = Bits(ohInputWidth bits)
      }

      val portCount = 1+withRsUnpack.toInt
      val arbiter = StreamArbiterFactory().noLock.lowerFirst.build(Request(), portCount)
      val results = Vec.fill(portCount)(Flow(Result()))

      val input = new Area(0) {
        valid := arbiter.io.output.valid
        arbiter.io.output.ready := True

        val args = insert(arbiter.io.output.payload)
        val source = insert(arbiter.io.chosen)
      }

      import input._

      val setup = new Area(1) { //TODO can probably be shorter pip
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


    val onUnpack = new layer.lane.Execute(unpackAt){
      val fsmPort = withRsUnpack generate new Area {
        val id = 0
        val cmd = unpacker.arbiter.io.inputs(id)
        val rsp = unpacker.results(id)
        cmd.setIdle()
      }

      val firstCycle = RegNext(False) setWhen(!layer.lane.isFreezed())

      val rsValues = rsList.map(rs => this.up(layer.lane(FloatRegFile, rs)))

      val fsmRequesters = Bits(rsList.size bits)
      val fsmServed = Bits(rsList.size bits)

      val clear = isReady
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
        val IS_SUBNORMAL = insert(expZero && !manZero)
        val recodedExpSub = SInt(p.exponentWidth + 1 bits)

        p.whenDouble(p.FORMAT) {
          RS_PRE_NORM.sign := f64.sign
          RS_PRE_NORM.mantissa.raw := B(f64.mantissa)
          RS_PRE_NORM.quiet := f64.mantissa.msb
          RS_PRE_NORM.exponent := f64.exponent.resize(p.exponentWidth) - p.exponentF64One
          manZero := f64.mantissa === 0
          expZero := f64.exponent === 0
          expOne := f64.exponent.andR
          recodedExpSub := -p.exponentF64One + 1
        } {
          RS_PRE_NORM.sign := f32.sign
          RS_PRE_NORM.quiet := f32.mantissa.msb
          RS_PRE_NORM.mantissa.raw := B(f32.mantissa << (if (p.rvd) 29 else 0))
          RS_PRE_NORM.exponent := f32.exponent.resize(p.exponentWidth) - p.exponentF32One
          manZero := f32.mantissa === 0
          expZero := f32.exponent === 0
          expOne := f32.exponent.andR
          recodedExpSub := -p.exponentF32One + 1
        }
        RS_PRE_NORM.mode := (expOne ## expZero).mux(
          default -> FloatMode.NORMAL(),
          1 -> (manZero ? FloatMode.ZERO | FloatMode.NORMAL),
          2 -> (manZero ? FloatMode.INF | FloatMode.NAN)
        )
        apply(RS) := RS_PRE_NORM
        val normalizer = withRsUnpack generate new Area {
          val unpackerSel = isValid && up(rfa.ENABLE) && rfa.is(FloatRegFile, rfa.RFID) && !up(TRAP) //A bit pessimistic, as not all float instruction will need unpacking
          val valid = unpackerSel && IS_SUBNORMAL
          val validReg = RegNext(unpackerSel && IS_SUBNORMAL ) clearWhen(!layer.lane.isFreezed()) init(False)
          val asked = RegInit(False) setWhen (fsmRequesters(inputId) && !fsmRequesters.dropLow(inputId + 1).orR || isCancel) clearWhen (clear)
          val served = RegInit(False) setWhen (fsmPort.rsp.valid && fsmServed.dropLow(inputId + 1).andR || isCancel) clearWhen (clear)
          fsmRequesters(inputId) := valid && !asked
          fsmServed(inputId) := !valid || served

          val exponent = Reg(RS.exponent)
          val mantissa = Reg(RS.mantissa)

          when(fsmRequesters(inputId)) {
            fsmPort.cmd.valid := True
            fsmPort.cmd.data := RS_PRE_NORM.mantissa.raw << widthOf(fsmPort.cmd.data) - widthOf(RS_PRE_NORM.mantissa.raw)
          }
          when(asked) {
            RS.exponent := exponent
            RS.mantissa := mantissa
          }
          when(!served) {
            exponent := recodedExpSub - fsmPort.rsp.shift.intoSInt
            mantissa.raw := fsmPort.rsp.data >> widthOf(fsmPort.cmd.data) - widthOf(RS_PRE_NORM.mantissa.raw)
          }
          val freezeIt = validReg && !served || firstCycle && unpackerSel && expZero  //Maybe a bit hard on timings
          layer.lane.freezeWhen(freezeIt)
        }

        val badBoxing = p.rvd generate new Area {
          val HIT = insert(p.FORMAT === FpuFormat.FLOAT && !input(63 downto 32).andR)
          when(HIT) { //This kinda create a long combinatoral path
            RS.setNanQuiet
            RS.sign := False
          }
        }
      }
    }

    val unpackDone = withRsUnpack.mux(!onUnpack.rs.map(_.normalizer.freezeIt).toList.orR, True)


    val onCvt = new layer.lane.Execute(unpackAt){ //TODO fmax
      val rs1 = up(layer.lane(IntRegFile, RS1))
      val rs1Zero = Riscv.XLEN.get match {
        case 32 => rs1(31 downto 0) === 0
        case 64 => rs1(31 downto 0) === 0 && (RsUnsignedPlugin.IS_W || rs1(63 downto 32) === 0)
      }

      val fsmPortId = withRsUnpack.toInt
      val fsmCmd = unpacker.arbiter.io.inputs(fsmPortId)
      val fsmRsp = unpacker.results(fsmPortId)
      val clear = isReady
      val asked = RegInit(False) setWhen (fsmCmd.ready) clearWhen (clear)
      val served = RegInit(False) setWhen (fsmRsp.valid) clearWhen (clear)
      val fsmResult = fsmRsp.toReg

      fsmCmd.valid := isValid && SEL_I2F && unpackDone && !asked
      fsmCmd.data := RsUnsignedPlugin.RS1_UNSIGNED.asBits.resized

      val freezeIt = isValid && SEL_I2F && !served
      layer.lane.freezeWhen(freezeIt)

      packPort.cmd.at(0) := isValid && SEL_I2F
      packPort.cmd.flags.clearAll()
      packPort.cmd.format := FORMAT
      packPort.cmd.roundMode := FpuUtils.ROUNDING
      packPort.cmd.hartId := Global.HART_ID
      packPort.cmd.uopId := Decode.UOP_ID

      packPort.cmd.value.quiet := False
      packPort.cmd.value.sign := RsUnsignedPlugin.RS1_REVERT
      packPort.cmd.value.exponent := unpacker.ohInputWidth - fsmResult.shift
      if (widthOf(fsmResult.data) > widthOf(packPort.cmd.value.mantissa.raw)) {
        packPort.cmd.value.mantissa.raw := fsmResult.data.takeHigh(p.mantissaWidth + 1) ## fsmResult.data.dropHigh(p.mantissaWidth + 1).orR
      } else {
        packPort.cmd.value.mantissa.raw := fsmResult.data << widthOf(packPort.cmd.value.mantissa.raw) - widthOf(fsmResult.data)
      }
      packPort.cmd.value.setNormal
      when(rs1Zero) {
        packPort.cmd.value.setZero
      }
    }
    buildBefore.release()
    unpacker.build()
  }
}
