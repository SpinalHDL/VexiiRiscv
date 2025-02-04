package vexiiriscv.misc

import vexiiriscv.VexiiRiscv
import vexiiriscv.execute.ExecuteLaneService
import vexiiriscv.riscv.{RS1, SingleDecoding}

object Reporter {
  def model(vexii : VexiiRiscv): Unit = {
    val plugins = vexii.host.services
    val lanes = plugins.collect{case l : ExecuteLaneService => l}
    for(lane <- lanes){
      println(s"Execute lane : ${lane.laneName}")
      for(layer <- lane.getLayers()){
        println(s"- Layer : ${layer.name}")
        for((_, spec) <- layer.uops){
          println(s"  - instruction : ${spec.uop match {
            case uop : SingleDecoding => uop.getName()
          }}")
          for((_, rs) <- spec.rs) {
            println(s"    - read  ${rs.rf.getName()}[${rs.rs.getName}], stage ${rs.from-lane.executeAt}")
          }
          for(rd <- spec.rd) {
            println(s"    - write ${rd.rf.getName()}[RD], stage ${rd.broadcastedFrom-lane.executeAt}")
          }
          for(compl <- spec.completion) println(s"    - completion stage ${compl-lane.executeAt}")
          for(stage <- spec.mayFlushUpTo) println(s"    - may flush up to stage ${stage-lane.executeAt}")
          for(stage <- spec.dontFlushFrom) println(s"    - dont flush from stage ${stage-lane.executeAt}")
          for(reserve <- spec.reservations) println(s"    - reserve ${reserve._1.getName()} at stage ${reserve._2.toList.map(_-lane.executeAt).map(_.toString).mkString(" / ")}")
          println(s"    - decodes ${spec.decodings.map(_._1.getName()).mkString(" / ")}")
        }
      }
    }
  }
}
