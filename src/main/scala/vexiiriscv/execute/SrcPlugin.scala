// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.Global._
import vexiiriscv.decode.Decode
import vexiiriscv.riscv.{IMM, IntRegFile, MicroOp, RS1, RS2, RfRead, Riscv}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object SrcStageables extends AreaObject {
  val REVERT, ZERO, UNSIGNED = Payload(Bool())
}

class SrcKeys
class Src1Keys extends SrcKeys
class Src2Keys extends SrcKeys
class OpKeys   extends SrcKeys
object SrcKeys extends AreaObject {
  val Op = new Area{
    val ADD = new OpKeys
    val SUB = new OpKeys
    val SRC1 = new OpKeys
    val LESS = new OpKeys
    val LESS_U = new OpKeys
  }
  val SRC1 = new Area{
    val RF = new Src1Keys
    val U  = new Src1Keys
  }
  val SRC2 = new Area{
    val RF = new Src2Keys
    val I  = new Src2Keys
    val S  = new Src2Keys
    val PC = new Src2Keys
  }
}

class SrcPlugin(val layer : LaneLayer,
                var executeAt : Int,
                var relaxedRs: Boolean,
                var splitedAddSub : Boolean = false) extends FiberPlugin{
  val elaborationLock = Retainer()
  withPrefix(layer.name)

  val spec = mutable.LinkedHashMap[UopLayerSpec, mutable.LinkedHashSet[SrcKeys]]()
  def specify(impl : UopLayerSpec, keys: Seq[SrcKeys]) = {
    val e = spec.getOrElseUpdate(impl, mutable.LinkedHashSet[SrcKeys]())
    for(k <- keys){
      assert(!e.contains(k))
      k match {
        case SrcKeys.SRC1.RF => impl.addRsSpec(RS1, executeAt)
        case SrcKeys.SRC2.RF => impl.addRsSpec(RS2, executeAt)
        case _ =>
      }
      e += k
    }
  }

  def specify(impl : UopLayerSpec, head: SrcKeys, tail : SrcKeys*) : Unit = specify(impl, head +: tail)

  val SRC1, SRC2 = Payload(SInt(Riscv.XLEN bits))
  val ADD_SUB = Payload(SInt(Riscv.XLEN bits))
  val LESS = Payload(Bool())

  val logic = during setup new Area{
    val eu = host.find[ExecuteLanePlugin](_.laneName == layer.laneName)
    val buildBefore = retains(eu.pipelineLock)
    awaitBuild()

    elaborationLock.await()
    val ss = SrcStageables
    val sk = SrcKeys

    assert(executeAt >= 0)

    val keys = spec.flatMap(_._2).toSeq.distinctLinked
    val opKeys   = keys.filter(_.isInstanceOf[OpKeys]).toSeq
    val src1Keys = keys.filter(_.isInstanceOf[Src1Keys]).toSeq
    val src2Keys = keys.filter(_.isInstanceOf[Src2Keys]).toSeq

    val SRC1_CTRL = Payload(Bits(log2Up(src1Keys.size) bits))
    val SRC2_CTRL = Payload(Bits(log2Up(src2Keys.size) bits))

    val src1ToEnum = src1Keys.zipWithIndex.map{case(k,i) => k -> B(i, widthOf(SRC1_CTRL) bits)}.toMap
    val src2ToEnum = src2Keys.zipWithIndex.map{case(k,i) => k -> B(i, widthOf(SRC2_CTRL) bits)}.toMap

    def has(keys : SrcKeys*) = keys.exists(keys.contains)

    for((impl, keys) <- spec){
      val REVERT, ZERO = Payload(Bool())
      impl.addDecoding(
        keys.toSeq.flatMap{
          case sk.Op.SRC1     => List(ss.REVERT -> False, ss.ZERO   -> True)
          case sk.Op.ADD      => List(ss.REVERT -> False, ss.ZERO   -> False)
          case sk.Op.SUB      => List(ss.REVERT -> True,  ss.ZERO   -> False)
          case sk.Op.LESS     => List(ss.REVERT -> True,  ss.ZERO   -> False, ss.UNSIGNED -> False)
          case sk.Op.LESS_U   => List(ss.REVERT -> True,  ss.ZERO   -> False, ss.UNSIGNED -> True)
          case key : Src1Keys => List(SRC1_CTRL -> src1ToEnum(key))
          case key : Src2Keys => List(SRC2_CTRL -> src2ToEnum(key))
        }
      )
    }

    val src = new eu.Execute(executeAt-relaxedRs.toInt){
      def get(rs : RfRead) = relaxedRs match {
        case false => up(eu(IntRegFile, rs))
        case true  => down(eu(IntRegFile, rs))
      }
      val imm = new IMM(Decode.UOP)
      if(src1Keys.nonEmpty) SRC1 := SRC1_CTRL.muxListDc[SInt](src1Keys.map {
        case sk.SRC1.RF => src1ToEnum(sk.SRC1.RF) -> S(get(RS1))
        case sk.SRC1.U  => src1ToEnum(sk.SRC1.U ) -> S(imm.u).resize(Riscv.XLEN)
      })

      val pcExtended = PHYSICAL_WIDTH.get < VIRTUAL_WIDTH.get
      if(src2Keys.nonEmpty) SRC2 := SRC2_CTRL.muxListDc[SInt](src2Keys.map {
        case sk.SRC2.RF => src2ToEnum(sk.SRC2.RF) -> S(get(RS2))
        case sk.SRC2.I  => src2ToEnum(sk.SRC2.I ) -> imm.i_sext
        case sk.SRC2.S  => src2ToEnum(sk.SRC2.S ) -> imm.s_sext
        case sk.SRC2.PC => src2ToEnum(sk.SRC2.PC) -> pcExtended.mux(S(this(Global.PC)).resize(Riscv.XLEN), S(this(Global.PC).resize(Riscv.XLEN)))
      })
    }


    val addsub = opKeys.nonEmpty generate new eu.Execute(executeAt){
      val alwaysAdd = !has(sk.Op.SUB, sk.Op.LESS, sk.Op.LESS_U)
      val alwaysSub = !has(sk.Op.ADD)
      val withRevert = !alwaysAdd && !alwaysSub
      def carryIn(that: SInt) =
        if      (alwaysSub)  that + 1
        else if (withRevert) that + S(U(ss.REVERT, Riscv.XLEN bits))
        else                 that

      def ifElseMap[T](cond : Boolean)(value : T)(body : T => T) : T = if(cond) value else body(value)

      val combined = !splitedAddSub generate new Area{
        val rs2Patched =  CombInit(ifElseMap(!alwaysSub)(this(SRC2))(~_))
        if(withRevert) when(ss.REVERT){ rs2Patched :=  ~SRC2  }
        if(has(sk.Op.SRC1)) when(ss.ZERO){ rs2Patched := 0 }
        ADD_SUB := carryIn(SRC1 + rs2Patched)
      }

      assert(!splitedAddSub)

      // SLT, SLTU, branches
      if(has(sk.Op.LESS, sk.Op.LESS_U)) {
        LESS := (SRC1.msb === SRC2.msb) ? ADD_SUB.msb | Mux(ss.UNSIGNED, SRC2.msb, SRC1.msb)
      }
    }
    buildBefore.release()
  }
}
