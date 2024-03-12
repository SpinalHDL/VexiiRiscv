package vexiiriscv.scratchpad
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench._

import scala.collection.mutable

object SyntOther extends App{

  class Pipeline {
    //Define the pipeline data model
        val specs = mutable.LinkedHashMap[NamedType[Data], mutable.LinkedHashMap[Int, Data]]()
        val enable = mutable.LinkedHashMap[Int, Bool]()

    //Define how we can access the pipeline
    def apply[T <: Data](what: NamedType[T], stageId: Int) = {
      val spec = specs.getOrElseUpdate(what.asInstanceOf[NamedType[Data]], new mutable.LinkedHashMap[Int, Data])
      spec.getOrElseUpdate(stageId, what().setName(what.getName + "_" + stageId)).asInstanceOf[T]
    }

    //Connect all those handsomes together
    def build(): Unit = {
      for ((what, nodes) <- specs) {
        for (i <- nodes.keys.min until nodes.keys.max) {
//          apply(what, i + 1) := RegNext(apply(what, i))
          apply(what, i + 1) := RegNextWhen(apply(what, i), enable.getOrElseUpdate(i, Delay(in(Bool()), 4)))
        }
      }
    }
  }

  val retim = Rtl(SpinalVerilog(new Component {
    setDefinitionName("retim")
    val width = 256
    val sel = in UInt(log2Up(width) bits)
    val a,b = in UInt (width bits)
    val result = out UInt(width bits)

    val pip = new Pipeline()
    val SEL = NamedType(UInt(log2Up(width) bits))
    val A, RESULT = NamedType(UInt(width bits))
    pip(A,0) := a
//    pip(B, 0) := b
    pip(SEL, 0) := sel
//    pip(RESULT, 4) := pip(A, 4) + pip(B, 4)
    pip(RESULT, 4) := pip(A, 4) >> pip(SEL, 4)
    result := pip(RESULT,8)
    pip.build()
  }))



  val rtls = List(retim)

  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}
