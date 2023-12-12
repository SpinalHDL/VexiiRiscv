package vexiiriscv.scratchpad

import spinal.core._

class HardMapPlay {
  case class MyBus(things : Seq[NamedType[_ <: Data]]) extends Bundle{
    val hm = new HardMap()
    things.foreach(e => hm.add(e))
  }

  val CTX_A = NamedType(Bool())
  val CTX_B = NamedType(Bool())

  val bus = MyBus(Seq(CTX_A, CTX_B))
}


