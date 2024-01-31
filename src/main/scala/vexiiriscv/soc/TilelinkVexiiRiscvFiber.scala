package vexiiriscv.soc

import net.fornwall.jelf.{ElfFile, ElfSection, ElfSectionHeader}
import spinal.core
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.coherent.{Hub, HubFiber}
import spinal.lib.bus.tilelink.sim.{Checker, Endpoint, MemoryAgent, Monitor, MonitorSubscriber, SlaveDriver, TransactionA, TransactionC, TransactionD}
import spinal.lib.bus.tilelink.{M2sSupport, M2sTransfers, Opcode, S2mSupport, SizeRange, fabric}
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug.DebugHartBus
import spinal.lib.misc.plic.InterruptCtrlFiber
import spinal.lib.misc.plugin.Hostable
import spinal.lib.misc.{ClintPort, Elf, InterruptCtrl, InterruptNode, TilelinkClintFiber}
import spinal.lib.sim.SparseMemory
import spinal.sim.{Signal, SimManagerContext}
import vexiiriscv.{ParamSimple, VexiiRiscv}
import vexiiriscv.execute.lsu.{LsuCachelessPlugin, LsuCachelessTileLinkPlugin}
import vexiiriscv.fetch.{FetchCachelessPlugin, FetchCachelessTileLinkPlugin}
import vexiiriscv.misc.PrivilegedPlugin

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer

class TilelinkVexiiRiscvFiber extends Area{
  val iBus = Node.master()
  val dBus = Node.master()
  val plugins = ArrayBuffer[Hostable]()

  val icfs = ArrayBuffer[InterruptCtrlFiber]()
  def bind(ctrl : InterruptCtrlFiber) = {
    icfs += ctrl
    ctrl.retain()
  }

  val clint = Handle[TilelinkClintFiber]
  def bind(clint: TilelinkClintFiber): Unit = {
    clint.lock.retain()
    this.clint load clint
  }

  val param = new ParamSimple()
  plugins ++= param.plugins()


  // Add the plugins to bridge the CPU toward Tilelink
  plugins.foreach {
    case p: FetchCachelessPlugin => plugins += new FetchCachelessTileLinkPlugin(iBus)
    case p: LsuCachelessPlugin => plugins += new LsuCachelessTileLinkPlugin(dBus)
    case _ =>
  }



  val logic = Fiber setup new Area{
    val core = VexiiRiscv(plugins)

    // Map the external interrupt controllers
    val privPlugin = plugins.collectFirst { case p: PrivilegedPlugin => p }.get
    val intIdPerHart = 1 + privPlugin.p.withSupervisor.toInt
    val mei = new Area {
      val node = InterruptNode.slave()
      val drivers = icfs.map(_.createInterruptMaster(privPlugin.hartIds(0) * intIdPerHart))
      drivers.foreach(node << _)
    }
    val sei = privPlugin.p.withSupervisor generate new Area {
      val node = InterruptNode.slave()
      val drivers = icfs.map(_.createInterruptMaster(privPlugin.hartIds(0) * intIdPerHart + 1))
      drivers.foreach(node << _)
    }
    icfs.foreach(_.release())

    val clintPort = clint.createPort(privPlugin.hartIds(0))
    val mti, msi = InterruptNode.slave()
    mti << clintPort.mti
    msi << clintPort.msi
    clint.lock.release()


    Fiber.awaitBuild()


    plugins.foreach {
//      case p: LsuCachelessPlugin => dBus.bus << p.logic.bus.toTilelink()
      case p: PrivilegedPlugin => {
        val hart = p.logic.harts(0)
        hart.int.m.timer := mti.flag
        hart.int.m.software := msi.flag
        hart.int.m.external := mei.node.flag
        if (p.p.withSupervisor) hart.int.s.external := sei.node.flag
        if (p.p.withRdTime) p.logic.rdtime := clint.thread.core.io.time
      }
      case _ =>
    }
  }
}
