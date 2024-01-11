package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.{FiberPlugin, PluginHost}
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{Const, MicroOp, Rvfd, Rvi}
import vexiiriscv.riscv.Riscv._

import scala.collection.mutable.ArrayBuffer

object AguPlugin extends AreaObject{
  val SEL = Payload(Bool())
  val AMO = Payload(Bool())
  val SC = Payload(Bool())
  val LR = Payload(Bool())
  val LOAD = Payload(Bool())
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

  layer.el.setDecodingDefault(SEL, False)

  val loads = ArrayBuffer[MicroOp](Rvi.LB, Rvi.LH, Rvi.LW, Rvi.LBU, Rvi.LHU)
  if (XLEN.get == 64) loads ++= List(Rvi.LD, Rvi.LWU)
  if (RVF) loads ++= List(Rvfd.FLW)
  if (RVD) loads ++= List(Rvfd.FLD)
  for (op <- loads) add(op).srcs(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.I).decode(LR -> False, LOAD -> True)

  // Store stuff
  val storeOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.S)
  val stores = ArrayBuffer[MicroOp](Rvi.SB, Rvi.SH, Rvi.SW)
  if (XLEN.get == 64) stores ++= List(Rvi.SD)
  for (store <- stores) add(store).srcs(storeOps).decode(AMO -> False, SC -> False, LOAD -> False, FLOAT -> False)
  if (RVF) add(Rvfd.FSW).srcs(storeOps).decode(AMO -> False, SC -> False, LOAD -> False, FLOAT -> True)
  if (RVD) add(Rvfd.FSD).srcs(storeOps).decode(AMO -> False, SC -> False, LOAD -> False, FLOAT -> True)

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
    for (amo <- uops) add(amo).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(AMO -> True, SC -> False, LOAD -> False, FLOAT -> False)
    stores += add(Rvi.SCW).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(AMO -> False, SC -> True, LOAD -> False, FLOAT -> False).uop
    loads += Rvi.SCW
    loads  += add(Rvi.LRW).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(LR -> True, LOAD -> True).uop
    if(XLEN.get == 64){
      stores += add(Rvi.SCD).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(AMO -> False, SC -> True, LOAD -> False, FLOAT -> False).uop
      loads += Rvi.SCD
      loads += add(Rvi.LRD).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(LR -> True, LOAD -> True).uop
    }
//    assert(false, "Rvi.LR and atomic may need reformat info, CachelessPlugin may use loads list for it, need to add to loads. Also store completion need to be handled")
  }
}