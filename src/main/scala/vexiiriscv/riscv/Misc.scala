package vexiiriscv.riscv
import spinal.core._
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._


object Riscv extends AreaObject {
  val XLEN = blocking[Int]
  val FLEN = blocking[Int]
  val LSLEN = blocking[Int]
  val RVC, RVM, RVD, RVF, RVA, RVZba, RVZbb, RVZbc, RVZbs, RVZcbm = blocking[Boolean]
  def withFpu = RVF || RVD

  def fpuExponentWidth = if (RVD) 11 else if (RVF) 8 else 0
  def fpuMantissaWidth = if (RVD) 52 else if (RVF) 23 else 0

  /**
   * This is a simple bootloader, which will initialize registers to match opensbi needs (a0 a1 a2).
   * @param dtb Physical memory address where the device tree binary is preloaded
   * @param opensbi Physical memory address where opensbi is preloaded
   * @return instructions of the bootloader (to be loaded on the CPU reset vector)
   */
  def bootToOpensbi(dtb : Long, opensbi : Long) = {
    //   li a0, 0
    //   la a1, data
    //   lw a1, 0(a1)
    //   li a2, 0;
    //   la t0, data
    //   lw t0, 4(t0)
    //   jr t0
    // data:
    // .word dts
    // .word opensbi
    val instructions = List(
      0x00000513, 0x00000597, 0x02058593, 0x0005a583, 0x00000613, 0x00000297, 0x01028293, 0x0042a283, 0x00028067, dtb.toInt, opensbi.toInt
    )
    val bytes = instructions.flatMap(e => List(0, 8, 16, 24).map(s => (e >> s).toByte)).toArray
    bytes
  }
}