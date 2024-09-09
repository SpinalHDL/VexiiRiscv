package vexiiriscv.execute

import scala.collection.mutable
import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{IntRegFile, RS1, RS2, Riscv, Rvi}
import Riscv._
import RsUnsignedPlugin._
import vexiiriscv.misc.{AdderAggregator, MulSpliter}

import scala.collection.mutable.ArrayBuffer

object MulPlugin extends AreaObject {
  val HIGH = Payload(Bool())
  val RESULT_IS_SIGNED = Payload(Bool())
}

trait MulReuse{
  def cmdAt : Int
  def inject(a : Bits, b : Bits) : Unit
  def rspAt : Int
  def rsp : Bits
}

class MulPlugin(val layer : LaneLayer,
                var cmdAt : Int = 0,
                var mulAt: Int = 0,
                var sumAt: Int = 1,
                var sumsSpec: List[(Int, Int)] = List((44, 8), (1000, 1000)),
                var untilOffsetS0: Int = Integer.MAX_VALUE,
                var writebackAt : Int = 2,
                var splitWidthA : Int = 17,
                var splitWidthB : Int = 17,
                var useRsUnsignedPlugin : Boolean = false,
                var keepMulSrc : Boolean = false,
                var bufferedHigh : Option[Boolean] = None) extends ExecutionUnitElementSimple(layer) with MulReuse {
  import MulPlugin._


  override def inject(src1: Bits, src2: Bits): Unit = {
    cmdAt match {
      case 0 =>
        logic.src(logic.keys.MUL_SRC1) := src1
        logic.src(logic.keys.MUL_SRC2) := src2
      case -1 =>
        logic.mul.bypass(logic.keys.MUL_SRC1) := src1
        logic.mul.bypass(logic.keys.MUL_SRC2) := src2
    }
  }

  override def rspAt: Int = writebackAt
  override def rsp: Bits = logic.writeback.result.asBits

  val logic = during setup new Logic {
    if(Riscv.RVM.isEmpty) Riscv.RVM.set(true)
    awaitBuild()
    import SrcKeys._

    if (bufferedHigh == None) bufferedHigh = Some(Riscv.XLEN >= 64 || mulAt > 0)
    if (bufferedHigh.get) {
      el.setDecodingDefault(HIGH, False)
    }

    val formatBus = newWriteback(ifp, writebackAt)

    add(Rvi.MUL   ).decode(HIGH -> False).rsUnsigned(true , true , useRsUnsignedPlugin)
    add(Rvi.MULH  ).decode(HIGH -> True ).rsUnsigned(true , true , useRsUnsignedPlugin)
    add(Rvi.MULHSU).decode(HIGH -> True ).rsUnsigned(true , false, useRsUnsignedPlugin)
    add(Rvi.MULHU ).decode(HIGH -> True ).rsUnsigned(false, false, useRsUnsignedPlugin)

    if(!useRsUnsignedPlugin){
      for(uop <- List(Rvi.MUL,Rvi.MULH, Rvi.MULHSU, Rvi.MULHU); spec = layer(uop)){
        spec.addRsSpec(RS1, cmdAt)
        spec.addRsSpec(RS2, cmdAt)
      }
    }

    if (XLEN.get == 64) {
      add(Rvi.MULW).decode(HIGH -> False).rsUnsigned(true , true, useRsUnsignedPlugin)
      for (op <- List(Rvi.MULW); spec = layer(op)) {
        ifp.signExtend(formatBus, layer(op), 32)
        if (!useRsUnsignedPlugin) {
          spec.addRsSpec(RS1, cmdAt)
          spec.addRsSpec(RS2, cmdAt)
        }
      }
    }
    uopRetainer.release()

    val finalWidth = XLEN*2
    val SRC_WIDTH = XLEN.get + (!useRsUnsignedPlugin).toInt
    val keys = new AreaRoot{
      val MUL_SRC1 = Payload(Bits(SRC_WIDTH bits))
      val MUL_SRC2 = Payload(Bits(SRC_WIDTH bits))
    }
    import keys._

    val src = new el.Execute(cmdAt) {
      val node = (cmdAt == -1).mux(down, up)
      val rs1 = node(el(IntRegFile, RS1))
      val rs2 = node(el(IntRegFile, RS2))
      useRsUnsignedPlugin match {
        case false => {
          MUL_SRC1 := (RS1_SIGNED && rs1.msb) ## (rs1)
          MUL_SRC2 := (RS2_SIGNED && rs2.msb) ## (rs2)
          if(keepMulSrc) {
            KeepAttribute(apply(MUL_SRC1))
            KeepAttribute(apply(MUL_SRC2))
          }
        }
        case true => {
          MUL_SRC1 := RS1_UNSIGNED.asBits
          MUL_SRC2 := RS2_UNSIGNED.asBits
          RESULT_IS_SIGNED := RS1_REVERT ^ RS2_REVERT
        }
      }
    }

    // Generate all the partial multiplications
    val mul = new el.Execute(mulAt) {
      // MulSpliter.splits Will generate a data model of all partial multiplications
      val splits = MulSpliter(SRC_WIDTH, SRC_WIDTH, splitWidthA, splitWidthB, !useRsUnsignedPlugin, !useRsUnsignedPlugin)
      // Generate the partial multiplications from the splits data model
      val VALUES = splits.map(s => insert(s.toMulU(MUL_SRC1, MUL_SRC2, finalWidth)))
//      VALUES.foreach(e => KeepAttribute(stage(e)))
    }

    // sourcesSpec will track the partial sum positions
    var sourcesSpec = mul.splits.map(s => AdderAggregator.Source(s, finalWidth)).toList
    // sourceToSignal will allow to retrieve the hardware signal from a sourcesSpec element
    val sourceToSignal = mutable.LinkedHashMap[AdderAggregator.Source, Payload[UInt]]()
    for((s, m) <- (sourcesSpec, mul.VALUES).zipped) sourceToSignal(s) = m

    // revertResult is the elaboration states which allows to resign the result of the unsigned multiplier
    // along the steps (instead of doing the whole resign on the last stage)
    val revertResult = useRsUnsignedPlugin generate new Area {
      val chunk = ArrayBuffer[Payload[UInt]]()
      var carry = RESULT_IS_SIGNED
      var ptr = 0
    }

    val steps = for(stepId <- sumsSpec.indices) yield new el.Execute(sumAt + stepId) {
      val (stepWidth, stepLanes) = sumsSpec(stepId)
      // Generate the specification for ever adders of the current step
      val addersSpec = AdderAggregator(
        sourcesSpec,
        stepWidth,
        stepLanes,
        untilOffset = if(stepId == 0) untilOffsetS0 else Integer.MAX_VALUE
      )
      // Generate the hardware corresponding to every addersSpec
      val adders = addersSpec.map(_.craft(sourceToSignal.mapValues(this(_)))).map(insert(_))

      // Setup the iteration variables for the next step
      sourcesSpec = addersSpec.map(_.toSource()).toList
      for ((s, m) <- (sourcesSpec, adders).zipped) sourceToSignal(s) = m
      if(splitWidthB == 1){
        println(addersSpec.mkString("\n"))
        println("------------")
      }


      val revert = useRsUnsignedPlugin generate new Area{
        import revertResult._
        val range = ptr until (if(sourcesSpec.size == 1) sourcesSpec(0).offsetNext else Math.min(sourcesSpec(0).offsetNext, sourcesSpec(1).offset))
        val value = sourceToSignal(sourcesSpec(0))(range)
        val patched = value.xorMask(RESULT_IS_SIGNED) +^ U(revertResult.carry)
        val carry = insert(patched.msb)
        revertResult.carry = carry
        chunk += insert(patched.resize(range.size))
        ptr += range.size
      }
    }

    val writeback = new el.Execute(writebackAt) {
      assert(sourcesSpec.size == 1)
      val result = useRsUnsignedPlugin match {
        case false => apply(sourceToSignal(sourcesSpec.head))
        case true => Cat(revertResult.chunk.map(apply(_))).asUInt
//        case true => stage(sourceToSignal(sourcesSpec.head)).twoComplement(RESULT_IS_SIGNED)
      }
      val buffer = bufferedHigh.get generate new Area{
        val valid = RegNext(False) init (False) setWhen (isValid && !isReady && !isCancel)
        val data = RegNext(result(XLEN, XLEN bits))
        el.freezeWhen(isValid && HIGH && !valid)
      }

      formatBus.valid := SEL
      formatBus.payload := B(HIGH ? (if(bufferedHigh.get) buffer.data else result(XLEN, XLEN bits)) otherwise result(0, XLEN bits))
    }
  }
}
