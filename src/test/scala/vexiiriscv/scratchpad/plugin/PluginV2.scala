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


object Rawrawr extends App{

  import spinal.core._
  import spinal.lib.misc.plugin._

  // Let's define a Component with a PluginHost instance
  class SubComponent extends Component {
    val host = new PluginHost()
  }

  // Let's define a plugin which create a register
  class StatePlugin extends FiberPlugin {
    // during build new Area { body } will run the body of code in the Fiber build phase, in the context of the PluginHost
    val logic = during build new Area {
      val signal = Reg(UInt(32 bits))
    }
  }

  // Let's define a plugin which will make the StatePlugin's register increment
  class DriverPlugin extends FiberPlugin {
    val logic = during build new Area {
      // Find the single instance of StatePlugin in the host and get its logic
      val sp = host[StatePlugin].logic.get

      // Generate the increment hardware
      sp.signal := sp.signal + 1
    }
  }

  class TopLevel extends Component {
    val sub = new SubComponent()

    // Here we create plugins and embed them in sub.host
    new DriverPlugin().setHost(sub.host)
    new StatePlugin().setHost(sub.host)
  }

  SpinalVerilog(new TopLevel)
}


object Rawrawr2 extends App{

  import spinal.core._
  import spinal.lib.misc.plugin._
  import spinal.core.fiber._

  class SubComponent extends Component {
    val host = new PluginHost()
  }

  class StatePlugin extends FiberPlugin {
    val logic = during build new Area {
      val signal = Reg(UInt(32 bits))
    }
  }

  class DriverPlugin extends FiberPlugin {
    // incrementBy will be set by others plugin at elaboration time
    var incrementBy = 0
    // retainer allows other plugins to create locks, on which this plugin will wait before using incrementBy
    val retainer = Retainer()

    val logic = during build new Area {
      val sp = host[StatePlugin].logic.get
      retainer.await()

      // Generate the incrementer hardware
      sp.signal := sp.signal + incrementBy
    }
  }

  // Let's define a plugin which will modify the DriverPlugin.incrementBy variable because letting it elaborate its hardware
  class SetupPlugin extends FiberPlugin {
    // during setup { body } will spawn the body of code in the Fiber setup phase (it is before the Fiber build phase)
    val logic = during setup new Area {
      // *** Setup phase code ***
      val dp = host[DriverPlugin]
      // Prevent the DriverPlugin from executing its build's body (until release() is called)
      val lock = dp.retainer()
      // Wait until the fiber phase reached build phase
      awaitBuild()

      // *** Build phase code ***
      // Let's mutate DriverPlugin.incrementBy
      dp.incrementBy += 1

      // Allows the DriverPlugin to execute its build's body
      lock.release()
    }
  }

  class TopLevel extends Component {
    val sub = new SubComponent()

    sub.host.asHostOf(
      new DriverPlugin(),
      new StatePlugin(),
      new SetupPlugin(),
      new SetupPlugin() //Let's add a second SetupPlugin, because we can
    )
  }


  SpinalVerilog(new TopLevel)
}