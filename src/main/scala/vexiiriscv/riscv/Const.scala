// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.riscv

import spinal.core._
import spinal.lib._

object Const {
  def funct7Range = 31 downto 25
  def rdRange = 11 downto 7
  def funct3Range = 14 downto 12
  def rs2Range = 24 downto 20
  def rs1Range = 19 downto 15
  def rs3Range = 31 downto 27
  def csrRange = 31 downto 20
  def rsRange(id : Int) = List(rs1Range, rs2Range,rs3Range)(id)
}

/**
 * Decode immediate values from a RISC-V instruction
 */
case class IMM(instruction  : Bits) extends Area{
  // immediates
  def i = instruction(31 downto 20)
  def h = instruction(31 downto 24)
  def s = instruction(31 downto 25) ## instruction(11 downto 7)
  def b = instruction(31) ## instruction(7) ## instruction(30 downto 25) ## instruction(11 downto 8)
  def u = instruction(31 downto 12) ## U"x000"
  def j = instruction(31) ## instruction(19 downto 12) ## instruction(20) ## instruction(30 downto 21)
  def z = instruction(19 downto 15)

  // sign-extend immediates
  def i_sext = S(i).resize(Riscv.XLEN)
  def h_sext = S(h).resize(Riscv.XLEN)
  def s_sext = S(s).resize(Riscv.XLEN)
  def b_sext = S(b ## False).resize(Riscv.XLEN)
  def j_sext = S(j ## False).resize(Riscv.XLEN)
}

object PrivilegeMode {
  val M = 3
  val S = 1
  val U = 0
  val VS = -3
  val VU = -4

  val TYPE = HardType(SInt(3 bits))
  val PRIVILEGE_MASK = 0x3

  def isGuest(privilege: SInt): Bool = privilege(2)

  def apply(isGuest: Bool, privilege: Bits): SInt = {
    val mode = TYPE()

    assert(widthOf(privilege) == 2)

    mode(2).assignFromBits((privilege === PrivilegeMode.M).mux(False, isGuest).asBits)
    mode(1 downto 0).assignFromBits(privilege.asBits)

    mode
  }

  def apply(isGuest: Bool, privilege: UInt): SInt = apply(isGuest, privilege.asBits)

  def apply(mode: Int): SInt = spinal.core.S(mode, 3 bit)
}

object CSR {
  val MCAUSE_ENUM = new {
    val FETCH_MISSALIGNED = 0

    val STORE_PAGE_FAULT = 15
    val STORE_MISALIGNED = 6
    val STORE_ACCESS_FAULT = 7

    val LOAD_PAGE_FAULT = 13
    val LOAD_MISALIGNED = 4
    val LOAD_ACCESS_FAULT = 5

    val ILLEGAL_INSTRUCTION = 2
    val BREAKPOINT = 3
    val ECALL_USER = 8
    val ECALL_SUPERVISOR = 9
    val ECALL_HYPERVISOR = 10
    val ECALL_MACHINE = 11

    val INSTRUCTION_ACCESS_FAULT = 1
    val INSTRUCTION_PAGE_FAULT = 12

    val VIRTUAL_INSTRUCTION = 22
    val INSTRUCTION_GUEST_PAGE_FAULT = 20
    val LOAD_GUEST_PAGE_FAULT = 21
    val STORE_GUEST_PAGE_FAULT = 23

    def isPageFault(code : UInt) : Bool = List(INSTRUCTION_PAGE_FAULT, LOAD_PAGE_FAULT, STORE_PAGE_FAULT).map(code === U(_)).orR
  }

  def misaExt(char: Char) = {
    val c = char.toUpper
    assert(c >= 'A' && c <= 'Z')
    1 << c-'A'
  }

  val DCSR      = 0x7B0
  val DPC       = 0x7B1
  val TSELECT   = 0x7A0
  val TDATA1    = 0x7A1
  val TDATA2    = 0x7A2
  val TINFO     = 0x7a4
  val TCONTROL  = 0x7A5

  def MVENDORID = 0xF11 // MRO Vendor ID.
  def MARCHID   = 0xF12 // MRO Architecture ID.
  def MIMPID    = 0xF13 // MRO Implementation ID.
  def MHARTID   = 0xF14 // MRO Hardware thread ID.Machine Trap Setup
  def MSTATUS   = 0x300 // MRW Machine status register.
  def MISA      = 0x301 // MRW ISA and extensions
  def MEDELEG   = 0x302 // MRW Machine exception delegation register.
  def MIDELEG   = 0x303 // MRW Machine interrupt delegation register.
  def MIE       = 0x304 // MRW Machine interrupt-enable register.
  def MTVEC     = 0x305 // MRW Machine trap-handler base address. Machine Trap Handling
  def MVIEN     = 0x308 // MRW Machine virtual interrupt enables.
  def MVIP      = 0x309 // MRW Machine virtual interrupt-pending bits.
  def MSTATUSH  = 0x310 // MRW Upper 32 bits of mstatus, RV32I only.
  def MIDELEGH  = 0x313 // MRW Upper 32 bits of mideleg, RV32I only.
  def MIEH      = 0x314 // Upper 32 bits of mie, RV32I only.
  def MVIENH    = 0x318 // Upper 32 bits of mvien, RV32I only.
  def MVIPH     = 0x319 // Upper 32 bits of mvip, RV32I only.
  def MSCRATCH  = 0x340 // MRW Scratch register for machine trap handlers.
  def MEPC      = 0x341 // MRW Machine exception program counter.
  def MCAUSE    = 0x342 // MRW Machine trap cause.
  def MTVAL     = 0x343 // MRW Machine bad address.
  def MIP       = 0x344 // MRW Machine interrupt pending.
  def MTINST    = 0x34A // MRW Machine trap instruction (transformed).
  def MTVAL2    = 0x34B // MRW Machine second trap value.
  def MIPH      = 0x354 // Upper 32 bits of mip, RV32I only.
  def MISELECT  = 0x350 // MRW Machine indirect register select.
  def MIREG     = 0x351 // MRW Machine indirect register alias.
  def MIREG2    = 0x352 // MRW Machine indirect register alias 2.
  def MIREG3    = 0x353 // MRW Machine indirect register alias 3.
  def MIREG4    = 0x355 // MRW Machine indirect register alias 4.
  def MIREG5    = 0x356 // MRW Machine indirect register alias 5.
  def MIREG6    = 0x357 // MRW Machine indirect register alias 6.
  def MTOPEI    = 0x35C // MRW Machine top external interrupt.
  def MENVCFG   = 0x30A // MRW Machine environment configuration register.
  def MENVCFGH  = 0x31A // MRW Machine environment configuration register.
  def MBASE     = 0x380 // MRW Base register.
  def MBOUND    = 0x381 // MRW Bound register.
  def MIBASE    = 0x382 // MRW Instruction base register.
  def MIBOUND   = 0x383 // MRW Instruction bound register.
  def MDBASE    = 0x384 // MRW Data base register.
  def MDBOUND   = 0x385 // MRW Data bound register.
  def MCYCLE    = 0xB00 // MRW Machine cycle counter.
  def MINSTRET  = 0xB02 // MRW Machine instructions-retired counter.
  def MCYCLEH   = 0xB80 // MRW Upper 32 bits of mcycle, RV32I only.
  def MINSTRETH = 0xB82 // MRW Upper 32 bits of minstret, RV32I only.
  def MHPMCOUNTER0 = 0xB00 // MRW Machine instructions-retired counter.
  def MHPMCOUNTER3 = 0xB03 // MRW Machine instructions-retired counter.
  def MHPMCOUNTER0H = 0xB80 // MRW Machine instructions-retired counter.
  def MHPMCOUNTER3H = 0xB83 // MRW Machine instructions-retired counter.
  def MHPMEVENT0 = 0x320 // MRW Machine instructions-retired counter.
  def MHPMEVENT3 = 0x323 // MRW Machine instructions-retired counter.
  val MCOUNTEREN  = 0x306
  val MCOUNTINHIBIT = 0x320
  def MHPMEVENT0H = 0x720 // MRW Machine instructions-retired counter.
  def MTOPI     = 0xFB0 // MRO Machine top interrupt.

  val PMPCFG = 0x3a0
  val PMPADDR = 0x3b0

  val SSTATUS     = 0x100
  val SIE         = 0x104
  val STVEC       = 0x105
  val SCOUNTEREN  = 0x106
  val SSCRATCH    = 0x140
  val SEPC        = 0x141
  val SCAUSE      = 0x142
  val STVAL       = 0x143
  val SIP         = 0x144
  val STIMECMP    = 0x14D
  val SISELECT    = 0x150
  val SIREG       = 0x151
  val SIREG2      = 0x152
  val SIREG3      = 0x153
  val SIREG4      = 0x154
  val SIREG5      = 0x155
  val SIREG6      = 0x156
  val STOPEI      = 0x15C
  val STIMECMPH   = 0x15D
  val SATP        = 0x180
  val SCOUNTOVF   = 0xDA0
  val STOPI       = 0xDB0

  def HSTATUS     = 0x600 // HRW Hypervisor status register.
  def HEDELEG     = 0x602 // HRW Hypervisor exception delegation register.
  def HIDELEG     = 0x603 // HRW Hypervisor interrupt delegation register.
  def HIE         = 0x604 // HRW Hypervisor interrupt-enable register.
  def HCOUNTEREN  = 0x606 // HRW Hypervisor counter enable.
  def HGEIE       = 0x607 // HRW Hypervisor guest external interrupt-enable register.
  def HEDELEGH    = 0x612 // HRW Upper 32 bits of hedeleg, RV32 only.
  def HTVAL       = 0x643 // HRW Hypervisor trap value.
  def HIP         = 0x644 // HRW Hypervisor interrupt pending.
  def HVIP        = 0x645 // HRW Hypervisor virtual interrupt pending.
  def HTINST      = 0x645 // HRW Hypervisor trap instruction (transformed).
  def HGEIP       = 0xE12 // HRO Hypervisor guest external interrupt pending.
  def HENVCFG     = 0x60A // HRW Hypervisor environment configuration register.
  def HENVCFGH    = 0x61A // HRW Upper 32 bits of henvcfg, RV32 only.
  def HGATP       = 0x680 // HRW Hypervisor guest address translation and protection.
  def HTIMEDELTA  = 0x605 // HRW Delta for VS/VU-mode timer.
  def HTIMEDELTAH = 0x615 // HRW Upper 32 bits of htimedelta, RV32 only.

  val VSSTATUS    = 0x200
  val VSIE        = 0x204
  val VSTVEC      = 0x205
  val VSSCRATCH   = 0x240
  val VSEPC       = 0x241
  val VSCAUSE     = 0x242
  val VSTVAL      = 0x243
  val VSIP        = 0x244
  val VSISELECT   = 0x250
  val VSATP       = 0x280
  val VSIREG      = 0x251
  val VSIREG2     = 0x252
  val VSIREG3     = 0x253
  val VSIREG4     = 0x254
  val VSIREG5     = 0x255
  val VSIREG6     = 0x256
  val VSTIMECMP   = 0x24D
  val VSTIMECMPH  = 0x25D
  val VSTOPEI     = 0x25C
  val VSTOPI      = 0xEB0

  def UCYCLE   = 0xC00 // UR Machine ucycle counter.
  def UCYCLEH  = 0xC80
  def UTIME    = 0xC01 // rdtime
  def UTIMEH   = 0xC81
  def UINSTRET  = 0xC02 // UR Machine instructions-retired counter.
  def UINSTRETH = 0xC82 // UR Upper 32 bits of minstret, RV32I only.

  def UHPMCOUNTER3 = 0xC03
  def UHPMCOUNTER3H = 0xC83

  def UHPMCOUNTER0 = 0xC00
  def UHPMCOUNTER0H = 0xC80

  val USTATUS = 0x0
  val UIE = 0x4
  val UTVEC = 0x5
  val VSTART = 0x8
  val VXSAT = 0x9
  val VXRM = 0xa
  val VCSR = 0xf
  val USCRATCH = 0x40
  val UEPC = 0x41
  val UCAUSE = 0x42
  val UTVAL = 0x43
  val UIP = 0x44

  val FFLAGS = 0x1
  val FRM = 0x2
  val FCSR = 0x3
}

object InterruptInfo {
  val defaultOrder = List[Int] (
    11, 3, 7,   // Machine interrupts: external, software, timer
    9, 1, 5,    // Supervisor interrupts: external, software, timer
    12,         // Supervisor guest external interrupt
    10, 2, 6,   // VS interrupts: external, software, timer
    13          // Local interrupt: counter overflow
  )
}

object IndirectCSR{
  val eidelivery  = 0x70
  val eithreshold = 0x72
  val eip0        = 0x80
  val eie0        = 0xC0
}
