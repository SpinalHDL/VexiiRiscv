package vexiiriscv.soc.demo

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus._
import spinal.lib.bus.tilelink.BusParameter
import spinal.lib.misc.InterruptNode

import scala.collection.mutable.ArrayBuffer

// This object provide a set of utilities
object PeripheralDemo{
  // This function will be used during the tilelink parameter negotiation
  // This will translate what the masters propose into what is actualy supported by the peripheral .
  // In this implementation, we don't realy care about what is proposed. Instead we enforce 12 bits addres, 32 bits data ...
  def getTilelinkSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = 12,
    dataWidth = 32,
    allowBurst = false,
    proposed = proposed
  )
}

// This class will carry most parameters related to our peripheral.
// It doesn't include the tilelink bus parameter, as those are toplevel negotiation related, and so, not realy user related
class PeripheralDemoParam(val ledWidth : Int)

// In this demo, the peripheral will :
// - Implement a tilelink slave interface to provide access to its functionalities
// - Output a LED bus
// - Output an interrupt when the LED bus is assigned with a few specific values (for the demo)
// PeripheralDemoComponent will be a pure SpinalHDL Component which define hardware explicitly. No Fiber yet.
class PeripheralDemoComponent(p : PeripheralDemoParam, busParameter : BusParameter) extends Component{
  //Let's define all our input / output / busses
  val io = new Bundle{
    val bus = slave(tilelink.Bus(busParameter))
    val led = out Bits(p.ledWidth bits)
    val interrupt = out Bool()
  }

  // tilelink.SlaveFactory is a utility which allow to easily generate the peripheral register mapping from a tilelink bus
  val mapper = new tilelink.SlaveFactory(io.bus, allowBurst = false)

  // Create a register at address 0x00 that we can read and write. That register is assigned to the io.led
  val ledReg = mapper.driveAndRead(io.led, address = 0x00) init(0)

  // Here we will handle all our interrupt logic.
  // val interrupts = new Area { ... } create a namespace for all the contained hardware
  val interrupts = new Area{
    // This will collect all the interrupt generated, for a later aggregation.
    val sources = ArrayBuffer[Bool]()

    // Instead of implementing each interrupt by hand each time, we put in place a tool to easily generate one.
    def createInterrupt(value : Bool, id : Int) = new Area{
      val enable = mapper.createReadAndWrite(Bool(), address = 0x10, bitOffset = id) init(False)
      val pending = mapper.read(value, address = 0x14, bitOffset = id)
      val interrupt = enable && pending
      sources += interrupt
    }

    // Let's use that tool to define a few interrupts sources
    val onLedCleared = createInterrupt(ledReg.norR, 0)
    val onLedSet = createInterrupt(ledReg.andR, 1)

    // Now that all the interrupts are defined, we can drive the io.interrupt by aggregating all the interrupt registred in sources
    io.interrupt := sources.orR
  }
}

// PeripheralDemoFiber will be kinda a integration layer for our peripheral into a SoC.
// It will :
// - Handle the tilelink parameter negotiation
// - Instantiate our PeripheralDemoComponent component
// - Bind things together
// - Export the PeripheralDemoComponent.io.led to the toplevel of our SoC (so we don't have to do it manually later on)
case class PeripheralDemoFiber(p : PeripheralDemoParam) extends Area{
  val node = tilelink.fabric.Node.slave()
  val interrupt = InterruptNode.master()

  // Here we define a elaboration thread which will start to run once the build phase (of the Fiber eco system) is reached
  val logic = Fiber build new Area{
    // Let's handle the tilelink bus parameter negotiation
    // m2s are general requests negotiation (get, put, acquire, ..)
    node.m2s.supported.load(PeripheralDemo.getTilelinkSupport(node.m2s.proposed))
    // s2m are for memory coherent requests negotiation (probe, ...).
    // This is memory coherency related stuff, and we don't want to deal with that at all ^^
    node.s2m.none()

    // Let instantiate our hardware and bind it
    val core = new PeripheralDemoComponent(p, node.bus.p)
    core.io.bus <> node.bus
    core.io.interrupt <> interrupt.flag

    // Let export the core.io.led as io of the toplevel.
    val led = core.io.led.toIo()
  }
}