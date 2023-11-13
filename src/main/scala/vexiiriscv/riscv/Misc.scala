package vexiiriscv.riscv
import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._


object Riscv{
  val XLEN = blocking[Int]
  val RVC = blocking[Boolean]
}