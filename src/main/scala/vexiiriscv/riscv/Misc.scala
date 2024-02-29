package vexiiriscv.riscv
import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._


object Riscv extends AreaObject {
  val XLEN = blocking[Int]
  val FLEN = blocking[Int]
  val LSLEN = blocking[Int]
  val RVC, RVM, RVD, RVF, RVA = blocking[Boolean]
}