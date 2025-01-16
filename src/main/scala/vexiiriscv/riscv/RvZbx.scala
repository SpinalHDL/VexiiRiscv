package vexiiriscv.riscv

import spinal.core._

import scala.collection.mutable

/**
 * Specifies the RISC-V bit-manip instructions
 */
object RvZbx {
  import IntRegFile._

  val ADD_UW  = TypeR(M"0000100----------000-----0111011")
  val ANDN    = TypeR(M"0100000----------111-----0110011")
  val BCLR    = TypeR(M"0100100----------001-----0110011")
  val BCLRI   = TypeI(M"010010-----------001-----0010011")
  val BEXT    = TypeR(M"0100100----------101-----0110011")
  val BEXTI   = TypeI(M"010010-----------101-----0010011")
  val BINV    = TypeR(M"0110100----------001-----0110011")
  val BINVI   = TypeI(M"011010-----------001-----0010011")
  val BSET    = TypeR(M"0010100----------001-----0110011")
  val BSETI   = TypeI(M"001010-----------001-----0010011")
  val CLMUL   = TypeR(M"0000101----------001-----0110011")
  val CLMULH  = TypeR(M"0000101----------011-----0110011")
  val CLMULR  = TypeR(M"0000101----------010-----0110011")
  val CLZ     = TypeI(M"011000000000-----001-----0010011")
  val CLZW    = TypeI(M"011000000000-----001-----0011011")
  val CPOP    = TypeI(M"011000000010-----001-----0010011")
  val CPOPW   = TypeI(M"011000000010-----001-----0011011")
  val CTZ     = TypeI(M"011000000001-----001-----0010011")
  val CTZW    = TypeI(M"011000000001-----001-----0011011")
  val MAX     = TypeR(M"0000101----------110-----0110011")
  val MAXU    = TypeR(M"0000101----------111-----0110011")
  val MIN     = TypeR(M"0000101----------100-----0110011")
  val MINU    = TypeR(M"0000101----------101-----0110011")
  val ORC_B   = TypeI(M"001010000111-----101-----0010011")
  val ORN     = TypeR(M"0100000----------110-----0110011")
  val REV8_32 = TypeI(M"011010011000-----101-----0010011")
  val REV8_64 = TypeI(M"011010111000-----101-----0010011")
  val ROL     = TypeR(M"0110000----------001-----0110011")
  val ROLW    = TypeR(M"0110000----------001-----0111011")
  val ROR     = TypeR(M"0110000----------101-----0110011")
  val RORI    = TypeI(M"011000-----------101-----0010011")
  val RORIW   = TypeI(M"0110000----------101-----0011011")
  val RORW    = TypeR(M"0110000----------101-----0111011")
  val SEXTB   = TypeI(M"011000000100-----001-----0010011")
  val SEXTH   = TypeI(M"011000000101-----001-----0010011")
  val SH1ADD    = TypeR(M"0010000----------010-----0110011")
  val SH1ADD_UW = TypeR(M"0010000----------010-----0111011")
  val SH2ADD    = TypeR(M"0010000----------100-----0110011")
  val SH2ADD_UW = TypeR(M"0010000----------100-----0111011")
  val SH3ADD    = TypeR(M"0010000----------110-----0110011")
  val SH3ADD_UW = TypeR(M"0010000----------110-----0111011")
  val SLLI_UW   = TypeI(M"000010-----------001-----0011011")
  val XNOR      = TypeR(M"0100000----------100-----0110011")
  val ZEXTH_32  = TypeI(M"000010000000-----100-----0110011")
  val ZEXTH_64  = TypeI(M"000010000000-----100-----0111011")

}
