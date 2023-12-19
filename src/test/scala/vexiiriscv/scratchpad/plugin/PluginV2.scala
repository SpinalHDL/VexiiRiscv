package vexiiriscv.scratchpad.plugin

import scala.collection._
import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._



object FiberPlay extends App{
  SpinalVerilog(new Component{

    val logic = new Area {
      var value = 0
      val xRetainer = Retainer()

      val x = Fiber setup new Area {
        //      AsyncThread.current.setName("Xthread")
        println("setup x")
        Fiber.awaitBuild()
        println("build x")
        val miaou = Bool()
        xRetainer.await()
        println(value)
        assert(value == 42)
      }

      val y = Fiber setup new Area {
        println("setup y")
        val xLock = xRetainer()
        Fiber.awaitBuild()
        println("build y")
        value = 42
        xLock.release()
      }

      val z = Fiber build new Area {
        println("build z")
      }
    }
  })
}
