package vexiiriscv.memory;

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamService}
import vexiiriscv.misc.{PrivilegedPlugin, TrapReason}
import vexiiriscv.riscv.{CSR, PrivilegeMode, Riscv}
import vexiiriscv.Global

import scala.collection.mutable.ArrayBuffer

case class MmuCtrlParameters(
  physicalWidth: Int,
  asidWidth: Int,
  vmidWidth: Int,
)

class MmuCtrlPlugin(val p: MmuCtrlParameters, supportSatpMode: Boolean, supportHgatpMode: Boolean) extends FiberPlugin {
  import p._

  def satpSpec = supportSatpMode match {
    case true  => if (Riscv.XLEN.get == 32) MmuSpec.sv32 else MmuSpec.sv39
    case false => MmuSpec.svbare(physicalWidth)
  }

  def hgatpSpec = supportHgatpMode match {
    case true  => if (Riscv.XLEN.get == 32) MmuSpec.sv32 else MmuSpec.sv39
    case false => MmuSpec.svbare(physicalWidth)
  }

  val logic = during setup new Area {
    val priv = host[PrivilegedPlugin]
    val csr = host[CsrAccessPlugin]

    val csrLock = retains(csr.csrLock)

    awaitBuild()

    assert(Global.HART_COUNT.get == 1)

    val satp = new csr.Csr(CSR.SATP) {
      val physicalPpnWidth = physicalWidth - 12
      val (modeOffset, modeWidth, ppnWidth, asidOffset, asidWidthMax) = Riscv.XLEN.get match {
        case 32 => (31, 1, 20 min physicalPpnWidth, 22, 9) //20 instead of 22 to avoid 34 physical bits, and it should not exceed the physical range
        case 64 => (60, 4, 44 min physicalPpnWidth, 44, 16)
      }
      assert(asidWidth <= asidWidthMax, "asidWidth is too big")
      val mode = Reg(Bits(modeWidth bits)) init(0)
      val asid = Reg(Bits(asidWidth bits)) init(0)
      val ppn =  Reg(UInt(ppnWidth bits))  init(0)

      if (satpSpec.satpMode != 0) readWrite(modeOffset -> mode, asidOffset -> asid, 0 -> ppn)

      // TODO: No update when no SATP mode is supported
      val satpModeWrite = csr.bus.write.bits(modeOffset, modeWidth bits)
      if (satpSpec.satpMode != 0) writeCancel(satpModeWrite =/= 0 && satpModeWrite =/= satpSpec.satpMode)

      allowCsr(!priv.logic.harts(0).m.status.tvm || priv.isMachine(0))
      csr.trapNextOnWrite += CsrListFilter(List(CSR.SATP))
    }

    val status = new Area {
      val mxr  = RegInit(False)
      val sum  = RegInit(False)

      for(offset <- List(CSR.MSTATUS, CSR.SSTATUS)) csr.readWrite(offset, 19 -> mxr, 18 -> sum)
    }

    val vsatp = priv.implementHypervisor generate new csr.Csr(CSR.VSATP) {
      val mode = Reg(Bits(satp.modeWidth bits)) init(0)
      val asid = Reg(Bits(asidWidth bits)) init(0)
      val ppn =  Reg(UInt(satp.ppnWidth bits))  init(0)

      if (satpSpec.satpMode != 0) readWrite(satp.modeOffset -> mode, satp.asidOffset -> asid, 0 -> ppn)

      val vsatpModeWrite = csr.bus.write.bits(satp.modeOffset, satp.modeWidth bits)
      if (satpSpec.satpMode != 0) writeCancel(vsatpModeWrite =/= 0 && vsatpModeWrite =/= satpSpec.satpMode)

      allowCsr(!priv.logic.harts(0).h.status.vtvm || priv.getPrivilege(0) =/= PrivilegeMode.VS)

      csr.trapNextOnWrite += CsrListFilter(List(CSR.VSATP))
      csr.remapWhen(CSR.SATP, CSR.VSATP, PrivilegeMode.isGuest(priv.getPrivilege(0)))
    }

    val vsstatus = priv.implementHypervisor generate new Area{
      val mxr  = RegInit(False)
      val sum  = RegInit(False)

      csr.readWrite(CSR.VSSTATUS, 19 -> mxr, 18 -> sum)
    }

    val hgatp = priv.implementHypervisor generate new csr.Csr(CSR.HGATP) {
      val physicalPpnWidth = physicalWidth - 12
      val (modeOffset, modeWidth, ppnWidth, vmidOffset, vmidWidthMax) = Riscv.XLEN.get match {
        case 32 => (31, 1, 20 min physicalPpnWidth, 22, 7) //20 instead of 22 to avoid 34 physical bits, and it should not exceed the physical range
        case 64 => (60, 4, 44 min physicalPpnWidth, 44, 14)
      }
      assert(vmidWidth <= vmidWidthMax, "vmidWidth is too big")
      val mode = Reg(Bits(modeWidth bits)) init(0)
      val vmid = Reg(Bits(vmidWidth bits)) init(0)
      val ppn =  Reg(UInt(ppnWidth bits))  init(0)

      if (hgatpSpec.satpMode != 0) readWrite(vmidOffset -> vmid, 0 -> ppn)
      if (hgatpSpec.satpMode != 0) read(modeOffset -> mode)
      allowHostCsr(!priv.logic.harts(0).m.status.tvm || priv.isMachine(0))

      if (hgatpSpec.satpMode != 0) onWrite(true) {
        val hgatpModeWrite = csr.bus.write.bits(modeOffset, modeWidth bits)
        when(hgatpModeWrite === 0) {
          mode := 0
        } elsewhen (hgatpModeWrite === hgatpSpec.satpMode) {
          mode := hgatpSpec.satpMode
        }
      }
    }

    csrLock.release()
  }
}
