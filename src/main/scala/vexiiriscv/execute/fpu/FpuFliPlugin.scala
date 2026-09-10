package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.execute.fpu.FpuUtils.FORMAT
import vexiiriscv.riscv._

class FpuFliPlugin(val layer : LaneLayer,
                   var readAt : Int = 0,
                   var wbAt : Int = 2) extends FiberPlugin {
  val p = FpuUtils

  val SEL = Payload(Bool())

  assert(wbAt >= readAt + 2)

  val logic = during setup new Area {
    val fwbp = host.find[WriteBackPlugin](p => p.lane == layer.lane && p.rf == FloatRegFile)
    val buildBefore = retains(layer.lane.pipelineLock)
    val uopLock = retains(layer.lane.uopLock, fwbp.elaborationLock)
    awaitBuild()

    val fwb = fwbp.createPort(wbAt)

    layer.lane.setDecodingDefault(SEL, False)
    def add(uop: MicroOp, decodings: (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(SEL -> True)
      spec.addDecoding(decodings)
      spec.setCompletion(wbAt)
      fwbp.addMicroOp(fwb, spec)
    }

    add(Rvfd.FLI_S, FORMAT -> FpuFormat.FLOAT)
    if (Riscv.RVD) add(Rvfd.FLI_D, FORMAT -> FpuFormat.DOUBLE)
    if (Riscv.RVZfh) add(Rvfd.FLI_H, FORMAT -> FpuFormat.HALF)
    if (Riscv.RVQ) add(Rvfd.FLI_Q, FORMAT -> FpuFormat.QUAD)

    uopLock.release()

    val onRead = new layer.Execute(readAt) {
      val address   = Decode.UOP(Const.rs1Range).asUInt
      val SIGN      = insert(address === 0)
      val HALF_SUBNORMAL = Riscv.RVZfh.get generate insert(address(4 downto 1) === 1)
      val HALF_SUBNORMAL_TYPE = Riscv.RVZfh.get generate insert(address(0) ## !address(0))
    }

    val onData = new layer.Execute(readAt + 1) {
      val rom = new Area {
        val storage = Mem(Bits(8 bits), 32) initBigInt (table.map(BigInt(_)))
        val data = storage.readSync(onRead.address, isReady)
        val mantissa = data(0, 2 bits)
        val exponent = data(2, 6 bits)
      }
      val MANTISSA = insert(rom.mantissa)
      val EXPONENT = insert(rom.exponent.asSInt)

      val SPECIAL = insert(!EXPONENT(5) && EXPONENT(4))
      /* Only -1/1 is need, just a record */
      val SPECIAL_EXPONENT = insert(Mux(EXPONENT(0), S(-1, 2 bits), S(1, 2 bits)))
    }

    val onWb = new layer.Execute(wbAt) {
      fwb.valid := SEL
      val value = fwb.payload.getAllTrue
      // For simplicity, let allow override this when XLEN = FLEN
      value.allowOverride()

      def buildExponent(size: Int) = {
        val exponent = onData.EXPONENT.resize(size).asBits ^ B(size bits, size - 1 -> True, default -> False)
        val special = onData.SPECIAL_EXPONENT.resize(size).asBits
        Mux(onData.SPECIAL, special, exponent)
      }

      p.whenFormat(FORMAT) {
        case FpuFormat.FLOAT => {
          value(0, FpuConst.f32.manWidth bits) := this(onData.MANTISSA) ## B(0, FpuConst.f32.manWidth - 2 bits)
          value(FpuConst.f32.manWidth, FpuConst.f32.expWidth bits) := buildExponent(FpuConst.f32.expWidth)
          value(31) := onRead.SIGN
        }
        case FpuFormat.DOUBLE => {
          value(0, FpuConst.f64.manWidth bits) := this(onData.MANTISSA) ## B(0, FpuConst.f64.manWidth - 2 bits)
          value(FpuConst.f64.manWidth, FpuConst.f64.expWidth bits) := buildExponent(FpuConst.f64.expWidth)
          value(63) := onRead.SIGN
        }
        case FpuFormat.HALF => {
          value(0, FpuConst.f16.manWidth bits) := Mux(onRead.HALF_SUBNORMAL, this(onRead.HALF_SUBNORMAL_TYPE), onData.MANTISSA.asBits) ## B(0, FpuConst.f16.manWidth - 2 bits)
          value(FpuConst.f16.manWidth, FpuConst.f16.expWidth bits) := buildExponent(FpuConst.f16.expWidth).andMask(!onRead.HALF_SUBNORMAL)
          value(15) := onRead.SIGN
        }
        case FpuFormat.QUAD => {
          value(0, FpuConst.f128.manWidth bits) := this(onData.MANTISSA) ## B(0, FpuConst.f128.manWidth - 2 bits)
          value(FpuConst.f128.manWidth, FpuConst.f128.expWidth bits) := buildExponent(FpuConst.f128.expWidth)
          value(127) := onRead.SIGN
        }
      }

      fwb.payload := value
    }

    buildBefore.release()
  }

  /* Table format
   * [7:2] exponent - 1 // code - bias = exponent -2^E
   * [1:0] mantissa[-1:-2]
   *
   * Note exponent[5:4] = "01" means it is a special value.
   */
  def table = Seq(
    0xFC, //  0: -1.0
    0x40, //  1: minimum positive normal

    0xBC, //  2: 2^-16
    0xC0, //  3: 2^-15
    0xDC, //  4: 2^-8
    0xE0, //  5: 2^-7
    0xEC, //  6: 2^-4
    0xF0, //  7: 2^-3

    0xF4, //  8: 0.25
    0xF5, //  9: 0.3125
    0xF6, // 10: 0.375
    0xF7, // 11: 0.4375

    0xF8, // 12: 0.5
    0xF9, // 13: 0.625
    0xFA, // 14: 0.75
    0xFB, // 15: 0.875

    0xFC, // 16: 1.0
    0xFD, // 17: 1.25
    0xFE, // 18: 1.5
    0xFF, // 19: 1.75

    0x00, // 20: 2.0
    0x01, // 21: 2.5
    0x02, // 22: 3.0

    0x04, // 23: 4.0
    0x08, // 24: 8.0
    0x0C, // 25: 16.0
    0x18, // 26: 2^7
    0x1C, // 27: 2^8
    0x38, // 28: 2^15
    0x3C, // 29: 2^16

    0x44, // 30: +Inf
    0x46  // 31: canonical NaN
  )
}
