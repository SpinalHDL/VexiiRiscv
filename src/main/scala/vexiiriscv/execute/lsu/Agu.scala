package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.{FiberPlugin, PluginHost}
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{Const, MicroOp, Riscv, Rvfd, Rvi}
import vexiiriscv.riscv.Riscv._
import vexiiriscv.execute._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AguPlugin extends AreaObject {
  val SEL = Payload(Bool())
  val LOAD  = Payload(Bool())
  val STORE = Payload(Bool())
  val ATOMIC = Payload(Bool()) // LR => ATOMIC && LOAD && !STORE, SC => ATOMIC && !LOAD && STORE, AMO => ATOMIC && LOAD && STORE
  val SIZE = Payload(UInt(2 bits)) // bytes = 1 << SIZE
  val FLOAT = Payload(Bool())
  val CLEAN, INVALIDATE = Payload(Bool())
}

/**
 * The AguFrontend provide an Area which define all RISC-V memory load/store/atomic instruction/microops as well as a set
 * of decodings (see AguPlugin object above)
 * It can be used by various LSU implementations as a base.
 */
class AguFrontend(
    layer: LaneLayer,
    host: PluginHost,
    withRvcbm : Boolean = false
  ) extends ExecuteUnitElementSimple.Api(
    layer,
    host.find[SrcPlugin](_.layer == layer),
    AguPlugin.SEL
  ) with Area {
  import AguPlugin._
  val sk = SrcKeys

  val defaultsDecodings = mutable.LinkedHashMap(LOAD -> False, STORE -> False, ATOMIC -> False, FLOAT -> False, CLEAN -> False, INVALIDATE -> False)
  def dec(changed : (Payload[_ <: BaseType], Any)*) = {
    val ret =  mutable.LinkedHashMap[Payload[_ <: BaseType], Any]()
    ret ++= defaultsDecodings
    ret ++= changed
    ret.toList
  }

  layer.lane.setDecodingDefault(SEL, False)

  val writingRf = ArrayBuffer[MicroOp](Rvi.LB, Rvi.LH, Rvi.LW, Rvi.LBU, Rvi.LHU)
  if (XLEN.get == 64) writingRf ++= List(Rvi.LD, Rvi.LWU)

  val writeRfFloat = ArrayBuffer[MicroOp]()
  if (RVF) writeRfFloat ++= List(Rvfd.FLW)
  if (RVD) writeRfFloat ++= List(Rvfd.FLD)
  writingRf ++= writeRfFloat
  for (op <- writingRf) add(op).srcs(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.I).decode(dec(LOAD -> True, FLOAT -> Bool(writeRfFloat.contains(op))))

  // Store stuff
  val storeOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.S)
  val writingMem = ArrayBuffer[MicroOp](Rvi.SB, Rvi.SH, Rvi.SW)
  if (XLEN.get == 64) writingMem ++= List(Rvi.SD)
  for (store <- writingMem) add(store).srcs(storeOps).decode(dec(STORE -> True))
  if (RVF) writingMem += add(Rvfd.FSW).srcs(storeOps).decode(dec(STORE -> True, FLOAT -> True)).uop
  if (RVD) writingMem += add(Rvfd.FSD).srcs(storeOps).decode(dec(STORE -> True, FLOAT -> True)).uop

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
    for (amo <- uops) add(amo).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(dec(LOAD -> True, STORE -> True, ATOMIC -> True))
    writingMem += add(Rvi.SCW).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(dec(STORE -> True, ATOMIC -> True)).uop
    writingRf += Rvi.SCW
    writingRf  += add(Rvi.LRW).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(dec(LOAD -> True, ATOMIC -> True)).uop
    if(XLEN.get == 64){
      writingMem += add(Rvi.SCD).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(dec(STORE -> True, ATOMIC -> True)).uop
      writingRf += Rvi.SCD
      writingRf += add(Rvi.LRD).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(dec(LOAD -> True, ATOMIC -> True)).uop
    }
  }

  val cbms = ArrayBuffer[MicroOp]()
  Riscv.RVZcbm.set(withRvcbm)
  val cbm = withRvcbm generate new Area{
    cbms += add(Rvi.CBM_CLEAN).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(dec(CLEAN -> True)).uop
    cbms += add(Rvi.CBM_FLUSH).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(dec(CLEAN -> True, INVALIDATE -> True)).uop
    cbms += add(Rvi.CBM_INVALIDATE).srcs(sk.Op.SRC1, sk.SRC1.RF).decode(dec(INVALIDATE -> True)).uop
  }
}