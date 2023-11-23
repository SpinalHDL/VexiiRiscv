package vexiiriscv.memory

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import vexiiriscv.Global._

trait AddressTranslationService extends Area {
  def createTranslationPort(): Unit
}

class StaticTranslationPlugin(var physicalWidth: Int) extends FiberPlugin with AddressTranslationService {
  during setup {
    PHYSICAL_WIDTH.set(physicalWidth)
    VIRTUAL_WIDTH.set(physicalWidth)
    MIXED_WIDTH.set(physicalWidth)
    PC_WIDTH.set(physicalWidth)
    TVAL_WIDTH.set(physicalWidth)
  }

  override def createTranslationPort(): Unit = ???

  val logic = during build new Area {

  }
}