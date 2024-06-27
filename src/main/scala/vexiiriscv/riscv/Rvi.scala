// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.riscv

import spinal.core._

import scala.collection.mutable

object Rvi extends AreaObject {
  import IntRegFile._

  val ADD                = TypeR(M"0000000----------000-----0110011")
  val SUB                = TypeR(M"0100000----------000-----0110011")
  val SLL                = TypeR(M"0000000----------001-----0110011")
  val SLT                = TypeR(M"0000000----------010-----0110011")
  val SLTU               = TypeR(M"0000000----------011-----0110011")
  val XOR                = TypeR(M"0000000----------100-----0110011")
  val SRL                = TypeR(M"0000000----------101-----0110011")
  val SRA                = TypeR(M"0100000----------101-----0110011")
  val OR                 = TypeR(M"0000000----------110-----0110011")
  val AND                = TypeR(M"0000000----------111-----0110011")


  val ADDI               = TypeI(M"-----------------000-----0010011")
  val SLLI               = TypeI(M"000000-----------001-----0010011")
  val SLTI               = TypeI(M"-----------------010-----0010011")
  val SLTIU              = TypeI(M"-----------------011-----0010011")
  val XORI               = TypeI(M"-----------------100-----0010011")
  val SRLI               = TypeI(M"000000-----------101-----0010011")
  val SRAI               = TypeI(M"010000-----------101-----0010011")
  val ORI_FULL           = TypeI(M"-----------------110-----0010011")
  def ORI(withLsuPrefetch : Boolean) : SingleDecoding = {
    if(!withLsuPrefetch) return ORI_FULL
    TypeI(List(
      M"-----------------1101----0010011",
      M"-----------------110-1---0010011",
      M"-----------------110--1--0010011",
      M"-----------------110---1-0010011",
      M"-----------------110----10010011",
      M"-------1---------110000000010011",
      M"--------1--------110000000010011",
      M"---------1-------110000000010011",
      M"-------000-0-----110000000010011",
    ))
  }

  val PREFETCH_R = TypeCmoPrefetch(M"-------00001-----110000000010011")
  val PREFETCH_W = TypeCmoPrefetch(M"-------00011-----110000000010011")

  val ANDI               = TypeI(M"-----------------111-----0010011")


  val ADDW               = TypeR(M"0000000----------000-----0111011")
  val SUBW               = TypeR(M"0100000----------000-----0111011")
  val ADDIW              = TypeI(M"-----------------000-----0011011")

  val SLLW                = TypeR(M"0000000----------001-----0111011")
  val SRLW                = TypeR(M"0000000----------101-----0111011")
  val SRAW                = TypeR(M"0100000----------101-----0111011")
  val SLLIW               = TypeI(M"000000-----------001-----0011011")
  val SRLIW               = TypeI(M"000000-----------101-----0011011")
  val SRAIW               = TypeI(M"010000-----------101-----0011011")

  val LUI   = TypeU(M"-------------------------0110111")
  val AUIPC = TypeUPC(M"-------------------------0010111")


  val BEQ  =  TypeB(M"-----------------000-----1100011")
  val BNE  =  TypeB(M"-----------------001-----1100011")
  val BLT  =  TypeB(M"-----------------100-----1100011")
  val BGE  =  TypeB(M"-----------------101-----1100011")
  val BLTU =  TypeB(M"-----------------110-----1100011")
  val BGEU =  TypeB(M"-----------------111-----1100011")
  val JALR =  TypeI(M"-----------------000-----1100111")
  val JAL  =  TypeJ(M"-------------------------1101111")



  val LB                 = TypeILQ(M"-----------------000-----0000011")
  val LH                 = TypeILQ(M"-----------------001-----0000011")
  val LW                 = TypeILQ(M"-----------------010-----0000011")
  val LD                 = TypeILQ(M"-----------------011-----0000011")
  val LBU                = TypeILQ(M"-----------------100-----0000011")
  val LHU                = TypeILQ(M"-----------------101-----0000011")
  val LWU                = TypeILQ(M"-----------------110-----0000011")

  val SB                 = TypeSSQ(M"-----------------000-----0100011")
  val SH                 = TypeSSQ(M"-----------------001-----0100011")
  val SW                 = TypeSSQ(M"-----------------010-----0100011")
  val SD                 = TypeSSQ(M"-----------------011-----0100011")

  val LRW                = TypeILQ(M"00010--00000-----010-----0101111")
  val SCW                = TypeASQ(M"00011------------010-----0101111")

  val LRD                = TypeILQ(M"00010--00000-----011-----0101111")
  val SCD                = TypeASQ(M"00011------------011-----0101111")

  val AMOSWAPW           = TypeASQ(M"00001------------010-----0101111")
  val AMOADDW            = TypeASQ(M"00000------------010-----0101111")
  val AMOXORW            = TypeASQ(M"00100------------010-----0101111")
  val AMOANDW            = TypeASQ(M"01100------------010-----0101111")
  val AMOORW             = TypeASQ(M"01000------------010-----0101111")
  val AMOMINW            = TypeASQ(M"10000------------010-----0101111")
  val AMOMAXW            = TypeASQ(M"10100------------010-----0101111")
  val AMOMINUW           = TypeASQ(M"11000------------010-----0101111")
  val AMOMAXUW           = TypeASQ(M"11100------------010-----0101111")


  val AMOSWAPD           = TypeASQ(M"00001------------011-----0101111")
  val AMOADDD            = TypeASQ(M"00000------------011-----0101111")
  val AMOXORD            = TypeASQ(M"00100------------011-----0101111")
  val AMOANDD            = TypeASQ(M"01100------------011-----0101111")
  val AMOORD             = TypeASQ(M"01000------------011-----0101111")
  val AMOMIND            = TypeASQ(M"10000------------011-----0101111")
  val AMOMAXD            = TypeASQ(M"10100------------011-----0101111")
  val AMOMINUD           = TypeASQ(M"11000------------011-----0101111")
  val AMOMAXUD           = TypeASQ(M"11100------------011-----0101111")

  val MUL                = TypeR(M"0000001----------000-----0110011")
  val MULH               = TypeR(M"0000001----------001-----0110011")
  val MULHSU             = TypeR(M"0000001----------010-----0110011")
  val MULHU              = TypeR(M"0000001----------011-----0110011")


  val DIV                = TypeR(M"0000001----------100-----0110011")
  val DIVU               = TypeR(M"0000001----------101-----0110011")
  val REM                = TypeR(M"0000001----------110-----0110011")
  val REMU               = TypeR(M"0000001----------111-----0110011")

  val MULW               = TypeR(M"0000001----------000-----0111011")
  val DIVW               = TypeR(M"0000001----------100-----0111011")
  val DIVUW              = TypeR(M"0000001----------101-----0111011")
  val REMW               = TypeR(M"0000001----------110-----0111011")
  val REMUW              = TypeR(M"0000001----------111-----0111011")

  val CSRRW              = TypeI (M"-----------------001-----1110011")
  val CSRRS              = TypeI (M"-----------------010-----1110011")
  val CSRRC              = TypeI (M"-----------------011-----1110011")
  val CSRRWI             = TypeIC(M"-----------------101-----1110011")
  val CSRRSI             = TypeIC(M"-----------------110-----1110011")
  val CSRRCI             = TypeIC(M"-----------------111-----1110011")

  val EBREAK             = TypeNone(M"00000000000100000000000001110011")
  val ECALL              = TypeNone(M"00000000000000000000000001110011")
  val MRET               = TypeNone(M"00110000001000000000000001110011")
  val SRET               = TypeNone(M"00010000001000000000000001110011")
  val URET               = TypeNone(M"00000000001000000000000001110011")
  val FENCEI             = TypeNone(M"00000000000000000001000000001111")
  val WFI                = TypeNone(M"00010000010100000000000001110011")

  val FENCE              = TypeNone(M"-----------------000-----0001111")
  val FENCE_I            = TypeNone(M"-----------------001-----0001111")
  val SFENCE_VMA         = TypeNone(M"0001001----------000000001110011")

  val FLUSH_DATA         = TypeNone(M"-------00000-----101-----0001111")

  case class LoadSpec(width: Int, signed: Boolean)
  val loadSpec = mutable.LinkedHashMap[MicroOp, LoadSpec]()

  loadSpec(LB)  = LoadSpec( 8,  true)
  loadSpec(LH)  = LoadSpec(16,  true)
  loadSpec(LW)  = LoadSpec(32,  true)
  loadSpec(LD)  = LoadSpec(64,  true)
  loadSpec(LBU) = LoadSpec( 8, false)
  loadSpec(LHU) = LoadSpec(16, false)
  loadSpec(LWU) = LoadSpec(32, false)

  loadSpec(LRW) = LoadSpec(32, true)
  loadSpec(SCW) = LoadSpec(8, true) //8 bits to allow sc to only write the 8 lsb (less logic)

  loadSpec(LRD) = LoadSpec(64, true)
  loadSpec(SCD) = LoadSpec(8, true)

  loadSpec(AMOSWAPW) = LoadSpec(32, true)
  loadSpec(AMOADDW) = LoadSpec(32, true)
  loadSpec(AMOXORW) = LoadSpec(32, true)
  loadSpec(AMOANDW) = LoadSpec(32, true)
  loadSpec(AMOORW) = LoadSpec(32, true)
  loadSpec(AMOMINW) = LoadSpec(32, true)
  loadSpec(AMOMAXW) = LoadSpec(32, true)
  loadSpec(AMOMINUW) = LoadSpec(32, true)
  loadSpec(AMOMAXUW) = LoadSpec(32, true)

  loadSpec(AMOSWAPD) = LoadSpec(64, true)
  loadSpec(AMOADDD) = LoadSpec(64, true)
  loadSpec(AMOXORD) = LoadSpec(64, true)
  loadSpec(AMOANDD) = LoadSpec(64, true)
  loadSpec(AMOORD) = LoadSpec(64, true)
  loadSpec(AMOMIND) = LoadSpec(64, true)
  loadSpec(AMOMAXD) = LoadSpec(64, true)
  loadSpec(AMOMINUD) = LoadSpec(64, true)
  loadSpec(AMOMAXUD) = LoadSpec(64, true)
}



class AtomicAlu(op : Bits,
                swap : Bool,
                mem : Bits,
                rf : Bits,
                isWord : Bool) extends Area{
  def lessFunc(bitId : Int) : Bool = Mux(rf(bitId) === mem(bitId), addSub(bitId), Mux(unsigned, mem(bitId), rf(bitId)))
  val compare  = op.msb
  val unsigned = op(1)
  val addSub   = (rf.asSInt + Mux(compare, ~mem, mem).asSInt + Mux(compare, S(1), S(0))).asBits
  val less     = lessFunc(widthOf(rf)-1)
  val selectRf = swap ? True | (op.lsb ^ less)

  val raw = (op | (swap ## B"00")).mux(
    B"000"  -> addSub,
    B"001"  -> (rf ^ mem),
    B"010"  -> (rf | mem),
    B"011"  -> (rf & mem),
    default -> (selectRf ? rf | mem)
  )
  val result = CombInit(raw)

  if(widthOf(rf) == 64){
    when(isWord){
      less := lessFunc(31)
      result(63 downto 32) := (default -> raw(31))
    }
  }
}