package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.riscv.Riscv
import vexiiriscv.riscv.Riscv.XLEN

object FpuFormatEncoding {
  def FLOAT = U(FpuUtils.FpuEncoding(FpuFormat.FLOAT), FpuUtils.formatWidth bits)
  def DOUBLE = U(FpuUtils.FpuEncoding(FpuFormat.DOUBLE), FpuUtils.formatWidth bits)
  def QUAD = U(FpuUtils.FpuEncoding(FpuFormat.QUAD), FpuUtils.formatWidth bits)
  def HALF = U(FpuUtils.FpuEncoding(FpuFormat.HALF), FpuUtils.formatWidth bits)
  def BHALF = U(FpuUtils.FpuEncoding(FpuFormat.BHALF), FpuUtils.formatWidth bits)
}

case class FpuConst(
  bits: Int,
  expWidth: Int,
  manWidth: Int,
  expSubnormal: Int,
  expMax: Int,
  expOne: Int,
) {
}

object FpuConst {
  val f16 = FpuConst(
    bits = 16,
    expWidth = 5,
    manWidth = 10,
    expSubnormal = -15,
    expMax = 15,
    expOne = 15,
  )

  val f32 = FpuConst(
    bits = 32,
    expWidth = 8,
    manWidth = 23,
    expSubnormal = -127,
    expMax = 127,
    expOne = 127,
  )

  val f64 = FpuConst(
    bits = 64,
    expWidth = 11,
    manWidth = 52,
    expSubnormal = -1023,
    expMax = 1023,
    expOne = 1023,
  )

  val f128 = FpuConst(
    bits = 128,
    expWidth = 15,
    manWidth = 112,
    expSubnormal = -16383,
    expMax = 16383,
    expOne = 16383,
  )

  val bf16 = FpuConst(
    bits = 16,
    expWidth = 8,
    manWidth = 7,
    expSubnormal = -127,
    expMax = 127,
    expOne = 127,
  )

  val supported = Map[FpuFormatTrait, FpuConst](
    FpuFormat.FLOAT  -> f32,
    FpuFormat.DOUBLE -> f64,
    FpuFormat.QUAD   -> f128,
    FpuFormat.HALF   -> f16,
    FpuFormat.BHALF  -> bf16,
  )
}

object FpuUtils extends AreaObject {
  def rsFloatWidth = (FpuConst.supported.filter { case (format, _) => supported.contains(format) }.map(_._2.bits) ++ Seq(0)).max
  def rsIntWidth = Riscv.XLEN.get
  def exponentWidth = FpuConst.supported.filter { case (format, _) => supported.contains(format) }.map(_._2.expWidth).max
  def mantissaWidth = FpuConst.supported.filter { case (format, _) => supported.contains(format) }.map(_._2.manWidth).max

  def rvd = Riscv.RVD.get
  def rvf = Riscv.RVF.get
  def rvq = Riscv.RVQ.get
  def rvzfh = Riscv.RVZfh.get
  def rvzfbfmin = Riscv.RVZfbfmin.get
  def rv64 = XLEN.get == 64
  val FORMAT = Payload(UInt(formatWidth bits))
  val ROUNDING = Payload(FpuRoundMode())
  def formatWidth = (supported.size > 0).mux(log2Up(supported.size), 0) max 1

  def supported = Seq[FpuFormatTrait]() ++
    (if (rvq) Seq(FpuFormat.QUAD) else Seq.empty) ++
    (if (rvd) Seq(FpuFormat.DOUBLE) else Seq.empty) ++
    (if (rvf) Seq(FpuFormat.FLOAT) else Seq.empty) ++
    (if (rvzfh) Seq(FpuFormat.HALF) else Seq.empty) ++
    (if (rvzfbfmin) Seq(FpuFormat.BHALF) else Seq.empty)


  def FpuEncoding(format: FpuFormatTrait): Int = supported.indexOf(format)

  def whenFormat(format: UInt)(cases: PartialFunction[FpuFormatTrait, Unit]): Unit = {
    switch(format) {
      for (f <- supported) {
        if (cases.isDefinedAt(f)) is(FpuEncoding(f)) {
          cases(f)
        }
      }

      default { }
    }
  }

  def muxFormat[T <: Data](format: UInt)(cases: PartialFunction[FpuFormatTrait, T]): T = format.muxListDc(supported.filter(cases.isDefinedAt(_)).map(f => FpuEncoding(f) -> cases(f)))

  def muxFormat[T <: Data](format : Bits)(cases: PartialFunction[FpuFormatTrait, T]): T = muxFormat(format.asUInt)(cases)


  def muxRv64[T <: Data](format : Bool)(yes : => T)(no : => T): T ={
    if(rv64) ((format) ? { yes } | { no })
    else no
  }

  def unpackedConfig = FloatUnpackedParam(
    exponentMax = (1 << exponentWidth - 1) - 1,
    exponentMin = -(1 << exponentWidth - 1) + 1 - mantissaWidth,
    mantissaWidth = mantissaWidth
  )
}
