package vexiiriscv.riscv
import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._


object Riscv extends AreaObject {
  val XLEN = blocking[Int]
  val FLEN = blocking[Int]
  val LSLEN = blocking[Int]
  val RVC, RVM, RVD, RVF, RVA, RVZba, RVZbb, RVZbc, RVZbs = blocking[Boolean]
  def withFpu = RVF || RVD

  def fpuExponentWidth = if (RVD) 11 else if (RVF) 8 else 0
  def fpuMantissaWidth = if (RVD) 52 else if (RVF) 23 else 0
}