package vexiiriscv.scratchpad

import spinal.core._
import vexiiriscv._
import vexiiriscv.VexiiRiscv

object Play1 extends App {
  SpinalVerilog {
    val param = new ParamSimple()
    VexiiRiscv(param.plugins())
  }
}
