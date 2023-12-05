package vexiiriscv.scratchpad

import spinal.core._
import vexiiriscv._
import vexiiriscv.VexiiRiscv
import vexiiriscv.compat.MultiPortWritesSymplifier

object Play1 extends App {
  val sc = SpinalConfig()
  sc.addTransformationPhase(new MultiPortWritesSymplifier)
  sc.generateVerilog {
    val param = new ParamSimple()
    VexiiRiscv(param.plugins())
  }
}

