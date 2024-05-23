package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.{FiberPlugin, PluginHost}
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{Const, MicroOp, Rvfd, Rvi}
import vexiiriscv.riscv.Riscv._
import vexiiriscv.execute._

import scala.collection.mutable.ArrayBuffer

object AguPlugin extends AreaObject{
  val SEL = Payload(Bool())
//  val AMO = Payload(Bool())
//  val SC = Payload(Bool())
//  val LR = Payload(Bool())
  val LOAD  = Payload(Bool())
  val STORE = Payload(Bool())
  val ATOMIC   = Payload(Bool())
  val SIZE = Payload(UInt(2 bits))
  val FLOAT = Payload(Bool())
}

class AguFrontend(
    layer: LaneLayer,
    host: PluginHost
  ) extends ExecuteUnitElementSimple.Api(
    layer,
    host.find[SrcPlugin](_.layer == layer),
    AguPlugin.SEL
  ) with Area {
  import AguPlugin._
  val sk = SrcKeys

  layer.lane.setDecodingDefault(SEL, False)

  val writingRf = ArrayBuffer[MicroOp](Rvi.LB, Rvi.LH, Rvi.LW, Rvi.LBU, Rvi.LHU)
  if (XLEN.get == 64) writingRf ++= List(Rvi.LD, Rvi.LWU)

  val writeRfFloat = ArrayBuffer[MicroOp]()
  if (RVF) writeRfFloat ++= List(Rvfd.FLW)
  if (RVD) writeRfFloat ++= List(Rvfd.FLD)
  writingRf ++= writeRfFloat
  for (op <- writingRf) add(op).srcs(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.I).decode(LOAD -> True, STORE -> False, ATOMIC -> False, FLOAT -> Bool(writeRfFloat.contains(op)))

  // Store stuff
  val storeOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.S)
  val writingMem = ArrayBuffer[MicroOp](Rvi.SB, Rvi.SH, Rvi.SW)
  if (XLEN.get == 64) writingMem ++= List(Rvi.SD)
  for (store <- writingMem) add(store).srcs(storeOps).decode(LOAD -> False, STORE -> True, ATOMIC -> False, FLOAT -> False)
  if (RVF) writingMem += add(Rvfd.FSW).srcs(storeOps).decode(LOAD -> False, STORE -> True, ATOMIC -> False, FLOAT -> True).uop
  if (RVD) writingMem += add(Rvfd.FSD).srcs(storeOps).decode(LOAD -> False, STORE -> True, ATOMIC -> False, FLOAT -> True).uop

  // Atomic stuff
  val amos = RVA.get generate new Area {
    val uops = ArrayBuffer[MicroOp]()
    uops ++= List(
      Rvi.AMOSWAPW, Rvi.AMOADDW, Rvi.AMOXORW, Rvi.AMOANDW, Rvi.AMOORW,
      Rvi.AMOMINW, Rvi.AMOMAXW, Rvi.AMOMINUW, Rvi.AMOMAXUW
    )
    if(XLEN.get == 64) uops ++= List(
      Rvi.AMOSWAPD, Rvi.AMOADDD, Rvi.AMOXORD, Rvi.AMOANDD, Rvi.AMOORD,
      Rvi.AMOMIND, Rvi.AMOMAXD, Rvi.AMOMINUD, Rvi.AMOMAXUD
    )
    for (amo <- uops) add(amo).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(LOAD -> True, STORE -> True, ATOMIC -> True, FLOAT -> False)
    writingMem += add(Rvi.SCW).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(LOAD -> False, STORE -> True, ATOMIC -> True, FLOAT -> False).uop
    writingRf += Rvi.SCW
    writingRf  += add(Rvi.LRW).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(LOAD -> True, STORE -> False, ATOMIC -> True, FLOAT -> False).uop
    if(XLEN.get == 64){
      writingMem += add(Rvi.SCD).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(LOAD -> False, STORE -> True, ATOMIC -> True, FLOAT -> False).uop
      writingRf += Rvi.SCD
      writingRf += add(Rvi.LRD).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(LOAD -> True, STORE -> False, ATOMIC -> True, FLOAT -> False).uop
    }
  }
}