package vexiiriscv.execute

import spinal.core._
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{Riscv, Rvk}

class Sm3ZkshPlugin(
  val layer : LaneLayer,
  val executeAt : Int = 0,
  val writeBackAt : Int = 0
) extends ExecutionUnitElementSimple(layer) {

  val mapping = new {
    def PI = 20
  }

  val logic = during setup new Logic {
    awaitBuild()
    assert(Riscv.XLEN.get == 32 || Riscv.XLEN.get == 64)
    import SrcKeys._

    val wb = newWriteback(ifp, writeBackAt)
    for (op <- List(Rvk.SM3P0, Rvk.SM3P1)) {
      add(op).srcs(SRC1.RF)
      ifp.signExtend(wb, layer(op), 32)
    }
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val rs1 = srcp.SRC1.asBits(31 downto 0)

      val sm3P0 = rs1 ^ rs1.rotateLeft(9)  ^ rs1.rotateLeft(17)
      val sm3P1 = rs1 ^ rs1.rotateLeft(15) ^ rs1.rotateLeft(23)

      val RESULT = insert((Decode.UOP(mapping.PI) ? sm3P1 | sm3P0))
    }

    val format = new el.Execute(writeBackAt) {
      wb.valid := SEL
      wb.payload := execute.RESULT.resized
    }
  }
}
