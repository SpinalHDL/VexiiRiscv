package vexiiriscv.sandbox

import spinal.core._
import spinal.lib.graphic.Rgb
import spinal.lib.misc.database.Database
import spinal.lib.misc.database.Database.blocking

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




object Miaouuuu9 extends App{
  // class VexiiRiscv extends Component{
  // val host = new PluginHost
  // }

  import spinal.core._
  import spinal.core.fiber._
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
    new Module{
      val mod = VexiiRiscv(plugins)

//      val patch = Fiber build mod.rework(new Area {
        val modCnt = mod.host[EventCounterPlugin].logic.get
        val cnt = out(modCnt.counter.resize(8))
//      })
    }.setName("TestComb")
  }
}

object Miaouuuu10 extends App{

  import spinal.core._
  import spinal.core.fiber._
  import spinal.lib.misc.plugin._
  import spinal.lib.CountOne
  import vexiiriscv._
  import scala.collection.mutable.ArrayBuffer

  case class AlphaPlugin_P(
                            a :Int ,
                            b :Int
                          )
  object AlphaPlugin_P extends AreaObject{
    val P = Database.blocking[AlphaPlugin_P]
    val P2 = Database.blocking[AlphaPlugin_P]
  }
  class AlphaPlugin(var p:AlphaPlugin_P) extends FiberPlugin{
    import AlphaPlugin_P._

    val logic = during setup new Area{
      awaitBuild()

      P.set(p)
      host.get[BetaPlugin].map{b =>
        P2.set(P.copy(a = b.p.a))
      }

      val ain = in UInt(P.a bits)
      val bin = in UInt(P.b bits)
      val cout= out (ain + bin)
    }
  }
  case class BetaPlugin_P(
                           a :Int ,
                           b :Int
                         )
  object BetaPlugin_P extends AreaObject{
    val P = blocking[BetaPlugin_P]
    val P2 = blocking[BetaPlugin_P]
  }
  class BetaPlugin(var p:BetaPlugin_P) extends FiberPlugin{
    import BetaPlugin_P._
    val sp = during setup new Area{
      awaitBuild()
      P.set(p)
      host.get[AlphaPlugin].map{a =>
        P2.set(P.copy(b = a.p.b + 2))
      }

      val ain = in UInt(P.a bits)
      val bin = in UInt(P.b bits)
      val cout= out (ain + bin)
    }
  }

  SpinalVerilog{
    val plugins = ArrayBuffer[FiberPlugin]()
    plugins += new AlphaPlugin((AlphaPlugin_P(a = 8, b = 8)))
    plugins += new BetaPlugin((BetaPlugin_P(a = 12, b = 12)))
    VexiiRiscv(plugins)
  }
}