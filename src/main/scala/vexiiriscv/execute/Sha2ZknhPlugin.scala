package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{Riscv, Rvk}

import scala.collection.mutable.ArrayBuffer

object ZknhPlugin {
  def make(layer: LaneLayer,
           xlen: Int,
           executeAt : Int = 0,
           writeBackAt : Int = 0) = {
    val plugins = ArrayBuffer[FiberPlugin]()

    plugins += new Sha256ZknhPlugin(layer, executeAt, writeBackAt)

    if (xlen == 32) plugins += new Sha512Rv32ZknhPlugin(layer, executeAt, writeBackAt)
    if (xlen == 64) plugins += new Sha512Rv64ZknhPlugin(layer, executeAt, writeBackAt)

    plugins
  }
}

class Sha256ZknhPlugin(
  val layer : LaneLayer,
  val executeAt : Int = 0,
  val writeBackAt : Int = 0
) extends ExecutionUnitElementSimple(layer) {

  val mapping = new {
    def OP = 21 downto 20
  }

  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    val wb = newWriteback(ifp, writeBackAt)
    for (op <- List(Rvk.SHA256SUM0, Rvk.SHA256SUM1, Rvk.SHA256SIG0, Rvk.SHA256SIG1)) {
      add(op).srcs(SRC1.RF)
      ifp.signExtend(wb, layer(op), 32)
    }
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val rs1 = srcp.SRC1.asBits(31 downto 0)

      val sum0 = rs1.rotateRight(2)  ^ rs1.rotateRight(13) ^ rs1.rotateRight(22)
      val sum1 = rs1.rotateRight(6)  ^ rs1.rotateRight(11) ^ rs1.rotateRight(25)
      val sig0 = rs1.rotateRight(7)  ^ rs1.rotateRight(18) ^ (rs1 |>> 3)
      val sig1 = rs1.rotateRight(17) ^ rs1.rotateRight(19) ^ (rs1 |>> 10)

      val result = Decode.UOP(mapping.OP).muxDc(
        0 -> sum0,
        1 -> sum1,
        2 -> sig0,
        3 -> sig1
      )
      val RESULT = insert(result)
    }

    val format = new el.Execute(writeBackAt) {
      wb.valid := SEL
      wb.payload := execute.RESULT.resized
    }
  }
}

class Sha512Rv32ZknhPlugin(
  val layer : LaneLayer,
  val executeAt : Int = 0,
  val writeBackAt : Int = 0
) extends ExecutionUnitElementSimple(layer) {

  val mapping = new {
    def OP = 27 downto 25
  }

  val logic = during setup new Logic {
    awaitBuild()
    assert(Riscv.XLEN.get == 32)
    import SrcKeys._

    val wb = newWriteback(ifp, writeBackAt)
    for (op <- List(Rvk.SHA512SUM0R, Rvk.SHA512SUM1R, Rvk.SHA512SIG0L, Rvk.SHA512SIG1L, Rvk.SHA512SIG0H, Rvk.SHA512SIG1H)) {
      add(op).srcs(SRC1.RF, SRC2.RF)
      ifp.signExtend(wb, layer(op), 32)
    }
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val rs1 = srcp.SRC1.asBits(31 downto 0)
      val rs2 = srcp.SRC2.asBits(31 downto 0)

      val sum0r = (rs1 |<< 25) ^ (rs1 |<< 30) ^ (rs1 |>> 28) ^ (rs2 |>> 7)  ^ (rs2 |>> 2)  ^ (rs2 |<< 4)
      val sum1r = (rs1 |<< 23) ^ (rs1 |>> 14) ^ (rs1 |>> 18) ^ (rs2 |>> 9)  ^ (rs2 |<< 18) ^ (rs2 |<< 14)
      val sig0l = (rs1 |>> 1)  ^ (rs1 |>> 7)  ^ (rs1 |>> 8)  ^ (rs2 |<< 31) ^ (rs2 |<< 25) ^ (rs2 |<< 24)
      val sig1l = (rs1 |<< 3)  ^ (rs1 |>> 6)  ^ (rs1 |>> 19) ^ (rs2 |>> 29) ^ (rs2 |<< 26) ^ (rs2 |<< 13)
      val sig0h = (rs1 |>> 1)  ^ (rs1 |>> 7)  ^ (rs1 |>> 8)  ^ (rs2 |<< 31) ^ (rs2 |<< 24)
      val sig1h = (rs1 |<< 3)  ^ (rs1 |>> 6)  ^ (rs1 |>> 19) ^ (rs2 |>> 29) ^ (rs2 |<< 13)

      val result = Decode.UOP(mapping.OP).muxDc(
        0 -> sum0r,
        1 -> sum1r,
        2 -> sig0l,
        3 -> sig1l,
        6 -> sig0h,
        7 -> sig1h
      )
      val RESULT = insert(result)
    }

    val format = new el.Execute(writeBackAt) {
      wb.valid := SEL
      wb.payload := execute.RESULT.resized
    }
  }
}

class Sha512Rv64ZknhPlugin(
  val layer : LaneLayer,
  val executeAt : Int = 0,
  val writeBackAt : Int = 0
) extends ExecutionUnitElementSimple(layer) {

  val mapping = new {
    def OP = 21 downto 20
  }

  val logic = during setup new Logic {
    awaitBuild()
    assert(Riscv.XLEN.get == 64)
    import SrcKeys._

    val wb = newWriteback(ifp, writeBackAt)
    for (op <- List(Rvk.SHA512SUM0, Rvk.SHA512SUM1, Rvk.SHA512SIG0, Rvk.SHA512SIG1)) {
      add(op).srcs(SRC1.RF)
    }
    uopRetainer.release()

    val execute = new el.Execute(executeAt) {
      val rs1 = srcp.SRC1.asBits

      val sum0 = rs1.rotateRight(28) ^ rs1.rotateRight(34) ^ rs1.rotateRight(39)
      val sum1 = rs1.rotateRight(14) ^ rs1.rotateRight(18) ^ rs1.rotateRight(41)
      val sig0 = rs1.rotateRight(1)  ^ rs1.rotateRight(8)  ^ (rs1 |>> 7)
      val sig1 = rs1.rotateRight(19) ^ rs1.rotateRight(61) ^ (rs1 |>> 6)

      val result = Decode.UOP(mapping.OP).muxDc(
        0 -> sum0,
        1 -> sum1,
        2 -> sig0,
        3 -> sig1
      )
      val RESULT = insert(result)
    }

    val format = new el.Execute(writeBackAt) {
      wb.valid := SEL
      wb.payload := execute.RESULT.resized
    }
  }
}
