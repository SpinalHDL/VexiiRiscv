package vexiiriscv.sandbox

import spinal.core._
import spinal.lib.graphic.Rgb

object PlayScope extends App{
  SpinalVerilog(new Component{
    val a = U(1)
    val b = U(2)
    val c = U(3)
    when(True){
      b := 42
      ContextSwapper.outsideCondScopeData(Rgb(5,6,7).setName("miaou"))
      c := 66
    }
    val x = U(1)
    val y = U(2)
    val z = U(3)
  })
}
