package vexiiriscv.misc
import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.cpu.riscv.debug.{DebugModule, DebugModuleCpuConfig, DebugModuleParameter, DebugTransportModuleJtagTap, DebugTransportModuleJtagTapWithTunnel, DebugTransportModuleParameter, DebugTransportModuleTunneled}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.riscv.Riscv._

class EmbeddedRiscvJtag(var p : DebugTransportModuleParameter,
                        var debugCd: ClockDomain = null,
                        var noTapCd: ClockDomain = null,
                        var withTap : Boolean = true,
                        var withTunneling : Boolean = false
                       ) extends FiberPlugin {


  def setDebugCd(cd: ClockDomain): this.type = {
    debugCd = cd; this
  }

  val logic = during build new Area {
    val jtag = withTap generate slave(Jtag())
    val jtagInstruction = !withTap generate slave(JtagTapInstructionCtrl())
    val ndmreset = out(Bool())
    assert(debugCd != null, "You need to set the debugCd of the VexRiscv EmbeddedRiscvJtag.")
    val onDebugCd = debugCd on new Area {
      val dm = DebugModule(
        DebugModuleParameter(
          version = p.version + 1,
          harts = 1,
          progBufSize = 2,
          datacount = (XLEN.get max FLEN.get) / 32,
          hartsConfig = List(DebugModuleCpuConfig(
            xlen = XLEN,
            flen = FLEN,
            withFpuRegAccess = false //FLEN.get == 64 //TODO FLOAT
          ))
        )
      )

      ndmreset := dm.io.ndmreset

      val dmiDirect = if (withTap && !withTunneling) new Area {
        val logic = DebugTransportModuleJtagTap(
          p.copy(addressWidth = 7),
          debugCd = ClockDomain.current
        )
        dm.io.ctrl <> logic.io.bus
        logic.io.jtag <> jtag
      }
      val dmiTunneled = if (withTap && withTunneling) new Area {
        val logic = DebugTransportModuleJtagTapWithTunnel(
          p.copy(addressWidth = 7),
          debugCd = ClockDomain.current
        )
        dm.io.ctrl <> logic.io.bus
        logic.io.jtag <> jtag
      }
      val dmiNoTap = if (!withTap) new Area {
        val logic = DebugTransportModuleTunneled(
          p = p,
          jtagCd = noTapCd,
          debugCd = ClockDomain.current
        )
        jtagInstruction <> logic.io.instruction
        dm.io.ctrl <> logic.io.bus
      }

      assert(Global.HART_COUNT.get == 1)
      val privBus = host[PrivilegedPlugin].logic.harts(0).debug.bus.setAsDirectionLess()
      privBus <> dm.io.harts(0)
      privBus.dmToHart.removeAssignments() <-< dm.io.harts(0).dmToHart
    }
  }
}

/*
src/openocd -f $VEXIIRISCV/src/main/tcl/openocd/vexiiriscv_sim.tcl
openocd -f src/main/tcl/openocd/vexiiriscv_sim.tcl

src/openocd -f $VEXIIRISCV/src/main/tcl/openocd/vexiiriscv_sim.tcl "sleep 5000" -c "reg pc 0x80000000" -c "exit" -d3

-c "sleep 5000" -c "reg a0 0x12345678" -c "exit" -d3

mdw 0x1000 16
mww 0x1000 0x12345678
mdw 0x1000 16

load_image /media/data/open/VexRiscv/src/test/resources/hex/dhrystoneO3.hex 0 ihex
mdw 0x80000000 16
reg pc 0x80000000
bp 0x80000114 4
resume


mdw 0x1000 16
mww 0x1000 0x12345678
mdw 0x1000 16

#load_image /media/data/open/VexRiscv/src/test/resources/hex/dhrystoneO3.hex 0 ihex
mww 0x80000000 0x13
mww 0x80000004 0x13
reg pc 0x80000000
step; reg pc
*/
