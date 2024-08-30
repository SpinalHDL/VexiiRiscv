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
  def rv64 = XLEN.get == 64
  val exponentF32One = 127
  val exponentF64One = 1023
  val FORMAT = Payload(FpuFormat())
  val ROUNDING = Payload(FpuRoundMode())

  def whenDouble(format : FpuFormat.C)(yes : => Unit)(no : => Unit): Unit ={
    if(rvd) when(format === FpuFormat.DOUBLE) { yes } otherwise{ no }
    if(!rvd) no
  }

  def muxDouble[T <: Data](format : FpuFormat.C)(yes : => T)(no : => T): T ={
    if(rvd) ((format === FpuFormat.DOUBLE) ? { yes } | { no })
    else no
  }
  def muxDouble[T <: Data](format : Bool)(yes : => T)(no : => T): T ={
    if(rvd) ((format) ? { yes } | { no })
    else no
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
