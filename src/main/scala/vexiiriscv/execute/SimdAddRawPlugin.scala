package vexiiriscv.execute

import spinal.core._
import spinal.core.sim.SpinalSimConfig
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.Generate.args
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.riscv.{IntRegFile, RS1, RS2, Riscv}
import vexiiriscv.tester.TestOptions



object SimdAddRaw{
  val ADD4 = IntRegFile.TypeR(M"0000000----------000-----0001011")
}


class SimdAddRawPlugin(val layer : LaneLayer) extends FiberPlugin  {
  import SimdAddRaw._
  val logic = during setup new Area {
    val wbp = host.find[WriteBackPlugin](p => p.rf == IntRegFile && p.lane == layer.lane)
    val earlyLock = retains(layer.lane.uopLock, wbp.elaborationLock)
    val lateLock = retains(layer.lane.pipelineLock)
    awaitBuild()

    val add4 = layer.add(ADD4)

    add4.addRsSpec(RS1, executeAt = 0)
    add4.addRsSpec(RS2, executeAt = 0)
    add4.setCompletion(0)
//    add4.mayFlushUpTo(0)
//    add4.dontFlushFrom(0)
//    add4.reserve(something, at=0)

    val wb = wbp.createPort(at = 0)
    wbp.addMicroOp(wb, add4)

    val SEL = Payload(Bool())
    layer.lane.setDecodingDefault(SEL, False)
    add4.addDecoding(SEL -> True)

    earlyLock.release()

    //Let's define some logic in the execute lane [0]
    val process = new layer.Execute(id = 0) {
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = layer.lane(IntRegFile, RS1).asUInt
      val rs2 = layer.lane(IntRegFile, RS2).asUInt

      //Do some computation
      val rd = UInt(32 bits)
      rd( 7 downto  0) := rs1( 7 downto  0) + rs2( 7 downto  0)
      rd(16 downto  8) := rs1(16 downto  8) + rs2(16 downto  8)
      rd(23 downto 16) := rs1(23 downto 16) + rs2(23 downto 16)
      rd(31 downto 24) := rs1(31 downto 24) + rs2(31 downto 24)

      //Provide the computation value for the writeback
      wb.valid := isValid && SEL
      wb.payload := rd.asBits
    }
    lateLock.release()
  }
}
