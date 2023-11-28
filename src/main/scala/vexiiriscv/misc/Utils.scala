package vexiiriscv.misc

import spinal.core._

object AddressToMask{
  def apply(address : UInt, size : UInt, width : Int) : Bits ={
    size.muxListDc((0 to log2Up(width)).map(i => U(i) -> B((1 << (1 << i)) -1, width bits))) |<< address(log2Up(width)-1 downto 0)
  }
}