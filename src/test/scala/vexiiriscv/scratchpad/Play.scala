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

object Play2 extends App{
//  val miaou = scala.collection.mutable.SortedSet[Int]()
//  for(i<- 0 until 20){
//    miaou += i
//  }
//  miaou ++= 9 to 23
//  miaou += 1
//  miaou += 3
//  miaou.remove(1)
// println(miaou)

  import scala.math.Ordering.Implicits._
  val miaou = collection.mutable.PriorityQueue(1, 2, 5, 3, 7)

  miaou.enqueue(-3)
  miaou.enqueue(10)

  println(miaou)
}
