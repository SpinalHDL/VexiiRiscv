// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._

//https://slideplayer.fr/slide/13631868/83/images/49/e%3D0+et+m%E2%89%A00+%EF%83%A8+N+est+d%C3%A9normalis%C3%A9.jpga
object FloatMode extends SpinalEnum{
  val ZERO, INF, NAN, NORMAL = newElement()
}

case class FloatUnpackedParam(exponentMax   : Int,
                              exponentMin   : Int,
                              mantissaWidth : Int){
  def union(other : FloatUnpackedParam) = FloatUnpackedParam(
    exponentMax max other.exponentMax,
    exponentMin min other.exponentMin,
    mantissaWidth max other.mantissaWidth
  )
}

object FloatUnpacked{
  def apply(exponentMax: Int,
            exponentMin: Int,
            mantissaWidth: Int) : FloatUnpacked = FloatUnpacked(FloatUnpackedParam(exponentMax, exponentMin, mantissaWidth))
}

/**
 * This is the floating format used in the FPU ALUs
 * Unlike ieee 754, it doesn't support subnormals, so, to convert from ieee 754 to FloatUnpacked accurately on subnormals,
 * the FloatUnpacked need to have an extended exponent field, allowing to cover the subnormal numbers as if they were normal.
 */
case class FloatUnpacked(p : FloatUnpackedParam) extends Bundle{
  def exponentMax = p.exponentMax
  def exponentMin = p.exponentMin
  def mantissaWidth = p.mantissaWidth
  val mode = FloatMode()
  val quiet = Bool() // if mode is NAN
  val sign = Bool()
  val exponent = new AFix(exponentMax, exponentMin, 0)
  val mantissa = AFix.U(0 exp, -mantissaWidth exp)

  def isNan = mode === FloatMode.NAN
  def isNormal = mode === FloatMode.NORMAL
  def isZero = mode === FloatMode.ZERO
  def isInfinity = mode === FloatMode.INF
  def isNanSignaling = isNan && !quiet

  def setNormal    = mode := FloatMode.NORMAL
  def setZero      = mode := FloatMode.ZERO
  def setInfinity  = mode := FloatMode.INF
  def setNan       = { mode := FloatMode.NAN; quiet := False }
  def setNanQuiet  = { mode := FloatMode.NAN; quiet := True }

  def invert(enable : Bool) = {
    val ret = cloneOf(this)
    ret := this
    ret.sign.removeAssignments() := this.sign ^ enable
    ret
  }

  def to(p2 : FloatUnpackedParam): FloatUnpacked = {
    val v = FloatUnpacked(p2)
    v := this
    v
  }
}

object FpuFormat extends SpinalEnum{
  val FLOAT, DOUBLE = newElement()
}

object FpuRoundMode extends SpinalEnum(){
  val RNE, RTZ, RDN, RUP, RMM = newElement()
  defaultEncoding = SpinalEnumEncoding("opt")(
    RNE -> 0,
    RTZ -> 1,
    RDN -> 2,
    RUP -> 3,
    RMM -> 4
  )
}

case class FpuFlags() extends Bundle{
  val NX,  UF,  OF,  DZ,  NV = Bool()
  def clear(): Unit ={
    List(NX,  UF,  OF,  DZ,  NV).foreach(_ := False)
  }

  def assign(NX : Bool = False,  UF : Bool = False,  OF : Bool = False,  DZ : Bool = False,  NV : Bool = False) = {
    this.NX := NX
    this.UF := UF
    this.OF := OF
    this.DZ := DZ
    this.NV := NV
    this
  }

  def |(that : FpuFlags) = {
    val ret = FpuFlags()
    ret.NX := this.NX | that.NX
    ret.UF := this.UF | that.UF
    ret.OF := this.OF | that.OF
    ret.DZ := this.DZ | that.DZ
    ret.NV := this.NV | that.NV
    ret
  }

  def andMask(that : Bool) = {
    val ret = FpuFlags()
    ret.NX := this.NX && that
    ret.UF := this.UF && that
    ret.OF := this.OF && that
    ret.DZ := this.DZ && that
    ret.NV := this.NV && that
    ret
  }
}



