package vexiiriscv.scratchpad.doc



object Miaouuuu extends App{
//  class VexiiRiscv extends Component{
//    val host = new PluginHost
//  }

  import spinal.core._
  import spinal.lib.misc.plugin._
  import vexiiriscv._
  import scala.collection.mutable.ArrayBuffer

  // Define a new plugin kind
  class FixedOutputPlugin extends FiberPlugin{
    // Define a build phase elaboration thread
    val logic = during build new Area{
      val port = out UInt(8 bits)
      port := 42
    }
  }

  // Generate the verilog
  SpinalVerilog{
    val plugins = ArrayBuffer[FiberPlugin]()
    plugins += new FixedOutputPlugin()
    VexiiRiscv(plugins)
  }

}

object Miaouuuu2 extends App{
  //  class VexiiRiscv extends Component{
  //    val host = new PluginHost
  //  }

  import spinal.core._
  import spinal.core.fiber.Retainer
  import spinal.lib.misc.plugin._
  import spinal.lib.CountOne
  import vexiiriscv._
  import scala.collection.mutable.ArrayBuffer

  class EventCounterPlugin extends FiberPlugin{
    val hostLockX = Retainer() // Will allow other plugins to block the elaboration of "logic" thread
    val events = ArrayBuffer[Bool]() // Will allow other plugins to add event sources
    val logic = during build new Area{
      hostLockX.await() // Active blocking
      val counter = Reg(UInt(32 bits)) init(0)
      counter := counter + CountOne(events)
    }
  }


  //For the demo we want to be able to instanciate this plugin multiple times, so we add a prefix parameter
  class EventSourcePlugin(prefix : String) extends FiberPlugin{
    withPrefix(prefix)

    // Create a thread starting from the setup phase (this allow to run some code before the build phase, and so lock some other plugins retainers)
    val logic = during setup new Area{
      val ecp = host[EventCounterPlugin] // Search for the single instance of EventCounterPlugin in the plugin pool
      // Generate a lock to prevent the EventCounterPlugin elaboration until we release it.
      // this will allow us to add our localEvent to the ecp.events list
      val ecpLocker = ecp.hostLockX()

      // Wait for the build phase before generating any hardware
      awaitBuild()

      // Here the local event is a input of the VexiiRiscv toplevel (just for the demo)
      val localEvent = in Bool()
      ecp.events += localEvent

      // As everything is done, we now allow the ecp to elaborate itself
      ecpLocker.release()
    }
  }

  SpinalVerilog{
    val plugins = ArrayBuffer[FiberPlugin]()
    plugins += new EventCounterPlugin()
    plugins += new EventSourcePlugin("lane0")
    plugins += new EventSourcePlugin("lane1")
    VexiiRiscv(plugins)
  }

}


object Miaouuuu3 extends App{
  import spinal.core._
  import spinal.lib.misc.plugin._
  import spinal.lib.misc.database.Database.blocking
  import vexiiriscv._
  import scala.collection.mutable.ArrayBuffer

  object Global extends AreaObject{
    val VIRTUAL_WIDTH = blocking[Int]
  }

  class LoadStorePlugin extends FiberPlugin{
    val logic = during build new Area{
      val register = Reg(UInt(Global.VIRTUAL_WIDTH bits))
    }
  }

  class MmuPlugin extends FiberPlugin{
    val logic = during build new Area{
      Global.VIRTUAL_WIDTH.set(39)
    }
  }

  SpinalVerilog{
    val plugins = ArrayBuffer[FiberPlugin]()
    plugins += new LoadStorePlugin()
    plugins += new MmuPlugin()
    VexiiRiscv(plugins)
  }

}
