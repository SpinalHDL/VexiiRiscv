package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.riscv.Riscv
import vexiiriscv.riscv.Riscv.XLEN

object FpuUtils extends AreaObject {
  def rsFloatWidth = 32 + Riscv.RVD.get.toInt*32
  def rsIntWidth = Riscv.XLEN.get
  def exponentWidth = if(Riscv.RVD) 11 else 8
  def mantissaWidth = if(Riscv.RVD) 52 else 23
  def rvd = Riscv.RVD.get
  def rvf = Riscv.RVF.get
  def rvq = Riscv.RVQ.get
  def rvzfh = Riscv.RVZfh.get
  def rv64 = XLEN.get == 64
  val exponentF32One = 127
  val exponentF64One = 1023
  val FORMAT = Payload(FpuFormat())
  val ROUNDING = Payload(FpuRoundMode())

  lazy val supported = Seq[FpuFormat.E]() ++
    (if (rvd) Seq(FpuFormat.DOUBLE) else Seq.empty) ++
    (if (rvf) Seq(FpuFormat.FLOAT) else Seq.empty)

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
    exponentMin = -(1 << exponentWidth - 1) + 1 - Riscv.fpuMantissaWidth,
    mantissaWidth = Riscv.fpuMantissaWidth
  )
}
