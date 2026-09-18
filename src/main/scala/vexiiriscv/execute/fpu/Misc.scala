package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.riscv.Riscv
import vexiiriscv.riscv.Riscv.XLEN

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

  val supported = Map[FpuFormat.E, FpuConst](
    FpuFormat.FLOAT  -> f32,
    FpuFormat.DOUBLE -> f64,
    FpuFormat.QUAD   -> f128,
    FpuFormat.HALF   -> f16,
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
  def rv64 = XLEN.get == 64
  val FORMAT = Payload(FpuFormat())
  val ROUNDING = Payload(FpuRoundMode())

  lazy val supported = Seq[FpuFormat.E]() ++
    (if (rvq) Seq(FpuFormat.QUAD) else Seq.empty) ++
    (if (rvd) Seq(FpuFormat.DOUBLE) else Seq.empty) ++
    (if (rvf) Seq(FpuFormat.FLOAT) else Seq.empty) ++
    (if (rvzfh) Seq(FpuFormat.HALF) else Seq.empty)

  def whenFormat(format: FpuFormat.C)(cases: PartialFunction[FpuFormat.E, Unit]): Unit = {
    switch(format) {
      for (f <- supported) {
        if (cases.isDefinedAt(f)) is(f) {
          cases(f)
        }
      }

      default { }
    }
  }

  def muxFormat[T <: Data](format : FpuFormat.C)(cases: PartialFunction[FpuFormat.E, T]): T = format.muxListDc(supported.filter(cases.isDefinedAt(_)).map(f => f -> cases(f)))

  def muxFormat[T <: Data](format : Bits)(cases: PartialFunction[FpuFormat.E, T]): T ={
    val tmp = FpuFormat()
    tmp.assignFromBits(format)
    muxFormat(tmp)(cases)
  }

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
