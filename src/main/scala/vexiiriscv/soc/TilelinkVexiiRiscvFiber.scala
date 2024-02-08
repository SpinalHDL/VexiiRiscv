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
import spinal.lib.system.tag.{MemoryConnection, PMA, PmaRegion}
import spinal.sim.{Signal, SimManagerContext}
import vexiiriscv.{ParamSimple, VexiiRiscv}
import vexiiriscv.execute.lsu.{LsuCachelessPlugin, LsuCachelessTileLinkPlugin, LsuL1Plugin, LsuL1TileLinkPlugin, LsuPlugin, LsuTileLinkPlugin}
import vexiiriscv.fetch.{FetchCachelessPlugin, FetchCachelessTileLinkPlugin, FetchFetchL1TileLinkPlugin, FetchL1Plugin}
import vexiiriscv.memory.AddressTranslationService
import vexiiriscv.misc.PrivilegedPlugin

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer

class TilelinkVexiiRiscvFiber(plugins : ArrayBuffer[Hostable]) extends Area{
  val iBus = Node.down()
  val dBus = Node.down()
  val lsuL1Bus = plugins.exists(_.isInstanceOf[LsuL1Plugin]) generate Node.down()

  def buses = List(iBus, dBus) ++ lsuL1Bus.nullOption

  val priv = plugins.collectFirst {
    case p: PrivilegedPlugin => new Area {
      val plugin = p
      val mti, msi, mei = InterruptNode.slave()
      val sei = p.p.withSupervisor generate InterruptNode.slave()
    }
  }

  def bind(ctrl: InterruptCtrlFiber) = priv match {
    case Some(priv) => new Area {
      val pp = priv.plugin
      val intIdBase = pp.hartIds(0) * (1 + pp.p.withSupervisor.toInt)
      ctrl.mapDownInterrupt(intIdBase, priv.mei)
      if(pp.p.withSupervisor) ctrl.mapDownInterrupt(intIdBase + 1, priv.sei)
    }
  }

  var clint = Option.empty[TilelinkClintFiber]
  def bind(clint: TilelinkClintFiber): Unit = priv match {
    case Some(priv) => new Area {
      val pp = priv.plugin
      val up = clint.createPort(pp.hartIds(0))
      priv.mti << up.mti
      priv.msi << up.msi
      TilelinkVexiiRiscvFiber.this.clint = Some(clint)
    }
  }


  // Add the plugins to bridge the CPU toward Tilelink
  plugins.foreach {
    case p: FetchCachelessPlugin => plugins += new FetchCachelessTileLinkPlugin(iBus)
    case p: FetchL1Plugin => plugins += new FetchFetchL1TileLinkPlugin(iBus)
    case p: LsuCachelessPlugin => plugins += new LsuCachelessTileLinkPlugin(dBus)
    case p: LsuPlugin => plugins += new LsuTileLinkPlugin(dBus)
    case p: LsuL1Plugin => plugins += new LsuL1TileLinkPlugin(lsuL1Bus)
    case _ =>
  }


  val logic = Fiber setup new Area{
    val core = VexiiRiscv(plugins)
    Fiber.awaitBuild()

    def getRegion(node : Node) = MemoryConnection.getMemoryTransfers(node).asInstanceOf[ArrayBuffer[PmaRegion]]
    plugins.foreach {
      case p: FetchCachelessPlugin => p.regions.load(getRegion(iBus))
      case p: FetchL1Plugin => p.regions.load(getRegion(iBus))
      case p: LsuCachelessPlugin => p.regions.load(getRegion(dBus))
      case p: LsuPlugin => p.ioRegions.load(getRegion(dBus))
      case p: LsuL1Plugin => p.regions.load(getRegion(lsuL1Bus))
      case _ =>
    }

    //Connect stuff
    plugins.foreach {
      case p: PrivilegedPlugin => {
        val hart = p.logic.harts(0)
        hart.int.m.timer := priv.get.mti.flag
        hart.int.m.software := priv.get.msi.flag
        hart.int.m.external := priv.get.mei.flag
        if (p.p.withSupervisor) hart.int.s.external := priv.get.sei.flag
        if (p.p.withRdTime) p.logic.rdtime := clint.get.thread.core.io.time
      }
      case _ =>
    }
  }
}