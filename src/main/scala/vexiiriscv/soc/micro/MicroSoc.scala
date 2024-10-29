package vexiiriscv.soc.micro

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.com.uart.TilelinkUartFiber
import spinal.lib.misc.{Elf, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import vexiiriscv.soc.TilelinkVexiiRiscvFiber


// Lets define our SoC toplevel
class MicroSoc(p : MicroSocParam) extends Component {
  // socCtrl will provide clocking, reset controllers and debugModule (through jtag) to our SoC
  val socCtrl = new SocCtrl(p.socCtrl)

  val system = new ClockingArea(socCtrl.system.cd) {
    // Let's define our main tilelink bus on which the CPU, RAM and peripheral "portal" will be plugged later.
    val mainBus = tilelink.fabric.Node()

    val cpu = new TilelinkVexiiRiscvFiber(p.vexii.plugins())
    if(p.socCtrl.withDebug) socCtrl.debugModule.bindHart(cpu)
    mainBus << cpu.buses
    cpu.dBus.setDownConnection(a = StreamPipe.S2M) // Let's add a bit of pipelining on the cpu.dBus to increase FMax

    val ram = new tilelink.fabric.RamFiber(p.ramBytes)
    ram.up at 0x80000000l of mainBus

    // Handle all the IO / Peripheral things
    val peripheral = new Area {
      // Some peripheral may require to have an access as big as the CPU XLEN, so, lets define a bus which ensure it.
      val busXlen = Node()
      busXlen.forceDataWidth(p.vexii.xlen)
      busXlen << mainBus
      busXlen.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)

      // Most peripheral will work with a 32 bits data bus.
      val bus32 = Node()
      bus32.forceDataWidth(32)
      bus32 << busXlen

      // The clint is a regular RISC-V timer peripheral
      val clint = new TilelinkClintFiber()
      clint.node at 0x10010000 of busXlen

      // The clint is a regular RISC-V interrupt controller
      val plic = new TilelinkPlicFiber()
      plic.node at 0x10C00000 of bus32

      val uart = new TilelinkUartFiber()
      uart.node at 0x10001000 of bus32
      plic.mapUpInterrupt(1, uart.interrupt)

      val demo = p.demoPeripheral.map(new PeripheralDemoFiber(_){
        node at 0x10002000 of bus32
        plic.mapUpInterrupt(2, interrupt)
      })

      val cpuPlic = cpu.bind(plic)
      val cpuClint = cpu.bind(clint)
    }

    val patcher = Fiber patch new Area{
      p.ramElf.foreach(new Elf(_, p.vexii.xlen).init(ram.thread.logic.mem, 0x80000000l))
    }
  }
}