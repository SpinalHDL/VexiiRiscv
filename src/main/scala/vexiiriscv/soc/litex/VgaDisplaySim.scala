package vexiiriscv.soc.litex

import rvls.spinal.{FileBackend, RvlsBackend}
import spinal.core.fiber.{Fiber, hardFork}
import spinal.lib._
import spinal.core._
import spinal.core.blackboxByteEnables.generateUnblackboxableError
import spinal.core.internals.{MemTopology, PhaseContext, PhaseNetlist}
import spinal.core.sim.{SimDataPimper, killRandom}
import spinal.lib.bus.amba4.axi.sim.{Axi4ReadOnlyMonitor, Axi4ReadOnlySlaveAgent, Axi4WriteOnlyMonitor, Axi4WriteOnlySlaveAgent}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer, Axi4ToTilelinkFiber}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.amba4.axilite.sim.{AxiLite4ReadOnlySlaveAgent, AxiLite4WriteOnlySlaveAgent}
import spinal.lib.bus.misc.args.{PeriphSpecs, PeriphTilelinkFiber}
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber, SelfFLush}
import spinal.lib.bus.tilelink.{coherent, fabric}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.com.eth.sg.{MacSgFiber, MacSgFiberSpec, MacSgParam}
import spinal.lib.com.jtag.sim.JtagRemote
import spinal.lib.cpu.riscv.debug.{DebugModuleFiber, DebugModuleSocFiber}
import spinal.lib.eda.bench.{Bench, Rtl}
import spinal.lib.graphic.YcbcrConfig
import spinal.lib.graphic.vga.{TilelinkVgaCtrlFiber, TilelinkVgaCtrlSpec, Vga, VgaRgbToYcbcr, VgaYcbcrPix2}
import spinal.lib.misc.{Elf, PathTracer, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.misc.test.DualSimTracer
import spinal.lib.sim.SparseMemory
import spinal.lib.{AnalysisUtils, Delay, Flow, ResetCtrlFiber, StreamPipe, master, memPimped, slave, traversableOncePimped}
import spinal.lib.system.tag.{MemoryConnection, MemoryEndpoint, MemoryEndpointTag, MemoryTransferTag, MemoryTransfers, PMA, VirtualEndpoint}
import vexiiriscv.{Global, ParamSimple}
import vexiiriscv.compat.{EnforceSyncRamPhase, MultiPortWritesSymplifier}
import vexiiriscv.execute.ExecuteLanePlugin
import vexiiriscv.execute.lsu.LsuL1Plugin
import vexiiriscv.fetch.{Fetch, FetchL1Plugin, FetchPipelinePlugin, PcPlugin}
import vexiiriscv.misc.{PrivilegedPlugin, TrapPlugin}
import vexiiriscv.prediction.GSharePlugin
import vexiiriscv.riscv.Riscv
import vexiiriscv.schedule.DispatchPlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.soc.micro.MicroSocSim.{elfFile, traceKonata, withRvlsCheck}
import vexiiriscv.test.{VexiiRiscvProbe, WhiteboxerPlugin}
import vexiiriscv.tester.{FsmHal, FsmHalGen, FsmOption, FsmTask}

import java.awt.{Dimension, Graphics}
import java.awt.image.BufferedImage
import java.io.File
import javax.swing.{JFrame, JPanel, WindowConstants}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Simulation monitor which scan a VGA output and display its image in a GUI
 */
object VgaDisplaySim{
  import spinal.core.sim._
  def apply(vga : Vga, cd : ClockDomain): Unit ={

    var width = 800
    var height = 600
    var scale = 1
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_BGR);

    val frame = new JFrame{
      setPreferredSize(new Dimension(800, 600));

      add(new JPanel{
        this.setPreferredSize(new Dimension(width, height))
        override def paintComponent(g : Graphics) : Unit = {
          g.drawImage(image, 0, 0, width*scale,height*scale, null)
        }
      })

      pack();
      setVisible(true);
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    }

    var overflow = false
    var x,y = 0
    cd.onSamplings{
      val vsync = vga.vSync.toBoolean
      val hsync = vga.hSync.toBoolean
      val colorEn = vga.colorEn.toBoolean
      if(colorEn) {
        //        val color = vga.color.r.toInt << (16 + 8 - vga.rgbConfig.rWidth) | vga.color.g.toInt << (8 + 8 - vga.rgbConfig.gWidth) | vga.color.b.toInt << (0 + 8 - vga.rgbConfig.bWidth)
        val color = vga.color.r.toInt << (16 + 8 - vga.rgbConfig.rWidth) | vga.color.g.toInt << (8 + 8 - vga.rgbConfig.gWidth) | vga.color.b.toInt << (0 + 8 - vga.rgbConfig.bWidth)
        if(x < width && y < height) {
          image.setRGB(x, y, color)
        }
        x+=1
      }
      if(!vsync){
        y = 0
        //        for(y <- 0 until height; x <- 0 until width) image.setRGB(x,y,0)
      }
      if(!hsync){
        if(x != 0){
          y+=1
          frame.repaint()
        }
        x = 0
      }
    }
  }
}