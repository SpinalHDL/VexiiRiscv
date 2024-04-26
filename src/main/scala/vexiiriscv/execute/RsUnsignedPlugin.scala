package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.riscv._
import vexiiriscv.riscv.Riscv._

object RsUnsignedPlugin extends AreaObject {
  val IS_W = Payload(Bool())
  val RS1_SIGNED, RS2_SIGNED = Payload(Bool())
  val RS1_REVERT, RS2_REVERT = Payload(Bool())
  val RS1_FORMATED, RS2_FORMATED = Payload(Bits(XLEN bits))
  val RS1_UNSIGNED, RS2_UNSIGNED = Payload(UInt(XLEN bits))
}

class RsUnsignedPlugin(val laneName : String, executeAt : Int = 0) extends FiberPlugin{
  import RsUnsignedPlugin._
  withPrefix(laneName)

  val elaborationLock = Retainer()

  def addUop(uop : UopLayerSpec, rs1Signed : Boolean, rs2Signed : Boolean) : Unit = {
    uop.addDecoding(RS1_SIGNED -> Bool(rs1Signed) , RS2_SIGNED -> Bool(rs2Signed))
    uop.addRsSpec(RS1, executeAt)
    uop.addRsSpec(RS2, executeAt)
  }

  def addUop(uop: UopLayerSpec, rs1Signed: Boolean): Unit = {
    uop.addDecoding(RS1_SIGNED -> Bool(rs1Signed))
    uop.addRsSpec(RS1, executeAt)
  }

  val logic = during setup new Area{
    val elp = host.find[ExecuteLanePlugin](_.laneName == laneName)
    val buildBefore = retains(elp.pipelineLock)
    awaitBuild()

    val onExecute = new elp.Execute(executeAt) {
      val rs1 = up(elp(IntRegFile, RS1))
      val rs2 = up(elp(IntRegFile, RS2))

      RS1_FORMATED := CombInit(rs1)
      RS2_FORMATED := CombInit(rs2)

      if (XLEN.get == 64) when(IS_W) {
        RS1_FORMATED(63 downto 32) := (default -> (RS1_SIGNED && rs1(31)))
        RS2_FORMATED(63 downto 32) := (default -> (RS2_SIGNED && rs2(31)))
      }

      RS1_REVERT := RS1_SIGNED && RS1_FORMATED.msb
      RS2_REVERT := RS2_SIGNED && RS2_FORMATED.msb

      def twoComplement(that: Bits, enable: Bool): UInt = (Mux(enable, ~that, that).asUInt + enable.asUInt)

      RS1_UNSIGNED := twoComplement(RS1_FORMATED, RS1_REVERT)
      RS2_UNSIGNED := twoComplement(RS2_FORMATED, RS2_REVERT)
    }
    buildBefore.release()
  }
}