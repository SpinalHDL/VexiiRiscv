package vexiiriscv.memory

import org.scalatest.funsuite.AnyFunSuite

class GuestPhysicalWidthTest extends AnyFunSuite {
  test("Sv32 supports 34-bit guest physical addresses") {
    assert(MmuSpec.sv32Physical34.physicalWidth == 34)
    assert(MmuSpec.sv32Physical34.levels.last.physicalWidth == 12)
    assert(MmuSpec.sv32x4.virtualWidth == 34)
    assert(MmuSpec.sv32x4.levels.last.virtualWidth == 12)
  }

  test("Sv39x4 accepts 41-bit guest physical addresses") {
    assert(MmuSpec.sv39x4.virtualWidth == 41)
    assert(MmuSpec.sv39x4.levels.last.virtualWidth == 11)
  }
}
