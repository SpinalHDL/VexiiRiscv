package vexiiriscv.riscv

import spinal.core._
import spinal.lib._

case class DecompressedInstruction() extends Bundle{
  val inst = Bits(32 bits)
  val illegal = Bool()
}

object RvcDecompressor{
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Component{
      out(Delay((apply(Delay(in Bits(16 bits),2), false, false, 32)),2))
    }.setDefinitionName("Decompressor"))
  }

  def apply(i : Bits, rvf : Boolean, rvd : Boolean, xlen : Int): DecompressedInstruction ={
    val ret = DecompressedInstruction()
    ret.inst.assignDontCare()
    ret.illegal := False

    val rch = B"01" ## i(9 downto 7)
    val rcl = B"01" ## i(4 downto 2)

    val addi5spnImm = B"00" ## i(10 downto 7) ## i(12 downto 11) ## i(5) ## i(6) ## B"00"
    val lwImm = B"00000" ## i(5) ## i(12 downto 10)  ## i(6) ## B"00"
    def swImm = lwImm
    val ldImm = B"0000" ## i(6 downto 5) ## i(12 downto 10) ## B"000"
    def sdImm = ldImm
    val addImm = B((11 downto 5) -> i(12), (4 downto 0) -> i(6 downto 2))
    def lImm = addImm
    val jalImm = B((9 downto 0) -> i(12)) ## i(8) ## i(10 downto 9) ## i(6) ## i(7) ## i(2) ## i(11) ## i(5 downto 3) ## B"0"
    val luiImm = B((14 downto 0) -> i(12)) ## i(6 downto 2) ## B"0000_0000_0000"
    val shiftImm = i(6 downto 2)
    val addi16spImm = B((2 downto 0) -> i(12)) ## i(4 downto 3) ## i(5) ## i(2) ## i(6) ## B"0000"
    val jImm = B((9 downto 0) -> i(12)) ## i(8) ## i(10 downto 9) ## i(6) ## i(7) ## i(2) ## i(11) ## i(5 downto 3) ## B"0"
    val bImm = B((4 downto 0) -> i(12)) ## i(6 downto 5) ## i(2) ## i(11 downto 10) ## i(4 downto 3) ## B"0"
    def shamt = (i(12) ## i(6 downto 2)).resize(12 bits)

    def lwspImm = B"0000" ## i(3 downto 2) ## i(12) ## i(6 downto 4) ## B"00"
    def swspImm = B"0000" ## i(8 downto 7) ## i(12 downto 9) ## B"00"
    def ldspImm = B"000" ## i(4 downto 2) ## i(12) ## i(6 downto 5) ## B"000"
    def sdspImm = B"000" ## i(9 downto 7) ## i(12 downto 10) ## B"000"


    val x0 = B"00000"
    val x1 = B"00001"
    val x2 = B"00010"


    switch(i(1 downto 0) ## i(15 downto 13)){
      default{ ret.illegal := True }
      is(0){
        ret.inst := addi5spnImm ## B"00010" ## B"000" ## rcl ## B"0010011"
        ret.illegal setWhen(i(12 downto 5) === 0)//imm!=0
      } //C.ADDI4SPN -> addi rd0, x2, nzuimm[9:2].
      if(rvd) is(1){ret.inst := ldImm ## rch ##  B"011" ## rcl ## B"0000111"} // C.FLD
      is(2){ret.inst := lwImm ## rch ## B"010" ## rcl ## B"0000011"} //C.LW -> lw rd', offset[6:2](rs1')
      if(xlen == 32 && rvf) is(3){ret.inst := lwImm ## rch ##  B"010" ## rcl ## B"0000111"} // C.FLW
      if(xlen == 64) is(3) {ret.inst := ldImm ## rch ##  B"011" ## rcl ## B"0000011"} // C.LD
      is(5){ret.inst := sdImm(11 downto 5) ## rcl  ## rch ## B"011" ## sdImm(4 downto 0) ## B"0100111"} // C.FSD
      is(6){ret.inst := swImm(11 downto 5) ## rcl  ## rch ## B"010" ## swImm(4 downto 0) ## B"0100011"} //C.SW -> sw rs2',offset[6:2](rs1')
      if(xlen == 32 && rvf) is(7){ret.inst := swImm(11 downto 5) ## rcl  ## rch ## B"010" ## swImm(4 downto 0) ## B"0100111"} // C.FSW
      if(xlen == 64) is(7){ret.inst := sdImm(11 downto 5) ## rcl  ## rch ## B"011" ## sdImm(4 downto 0) ## B"0100011"} // C.SD
      is(8){ret.inst := addImm ## i(11 downto 7) ## B"000" ## i(11 downto 7) ## B"0010011"} //C.ADDI -> addi rd, rd, nzimm[5:0].
      if(xlen == 32) is(9){ret.inst := jalImm(20) ## jalImm(10 downto 1) ## jalImm(11) ## jalImm(19 downto 12) ## x1 ## B"1101111"} //C.JAL -> jalr x1, rs1, 0.
      if (xlen == 64) is(9) { //C.ADDIW -> addiw rd, rd, nzimm[5:0].
        ret.inst := addImm ## i(11 downto 7) ## B"000" ## i(11 downto 7) ## B"0011011"
        ret.illegal setWhen (i(11 downto 7) === 0) //rd!=0
      }
      is(10){ret.inst := lImm ## B"00000" ## B"000" ## i(11 downto 7) ## B"0010011"} //C.LI -> addi rd, x0, imm[5:0].
      is(11){  //C.ADDI16SP    C.LUI ->
        val addi16sp =  addi16spImm ## i(11 downto 7) ## B"000" ## i(11 downto 7) ## B"0010011"
        val lui      =  luiImm(31 downto 12) ## i(11 downto 7) ## B"0110111"
        ret.inst := (i(11 downto 7) === 2) ? addi16sp | lui
        ret.illegal setWhen(i(6 downto 2) === 0 && i(12) === False) //imm!=0
      }
      is(12){
        val srli = shamt ## rch ## B"101" ## rch ## B"0010011"
        val srai = srli | (1 << 30)
        val andi = addImm ## rch ## B"111" ## rch ## B"0010011"
        val rtype = {
          val funct = Seq(0, 4, 6, 7, 0, 0, 2, 3).map(U(_, 3 bits)).read(U(i(12) ## i(6 downto 5)))
          val sub = B(1 << 30, 32 bits).andMask(i(6 downto 5) === 0)
          val opc = Mux(i(12), B"0111011", B"0110011")
          (B"0000000" ## rcl ## rch ## funct ## rch ## opc) | sub
        }
        ret.inst := Seq(srli, srai, andi, rtype).read(U(i(11 downto 10)))
      }
      is(13){ ret.inst := jImm(20) ## jImm(10 downto 1) ## jImm(11) ## jImm(19 downto 12) ## x0 ## B"1101111"}
      is(14){ ret.inst := bImm(12) ## bImm(10 downto 5) ## x0 ## rch ## B"000" ## bImm(4 downto 1) ## bImm(11) ## B"1100011" }
      is(15){ ret.inst := bImm(12) ## bImm(10 downto 5) ## x0 ## rch ## B"001" ## bImm(4 downto 1) ## bImm(11) ## B"1100011" }
      is(16){ ret.inst := B"000000" ## i(12) ## i(6 downto 2) ## i(11 downto 7) ## B"001" ## i(11 downto 7) ## B"0010011"   }
      if(rvd) is(17){ret.inst := ldspImm ## x2 ## B"011" ## i(11 downto 7) ## B"0000111"}// C.FLDSP
      is(18){ // C.LWSP
        ret.inst := lwspImm ## x2 ## B"010" ## i(11 downto 7) ## B"0000011"
        ret.illegal setWhen(i(11 downto 7) === 0) //rd!=0
      }
      if(xlen == 32 && rvf) is(19){ret.inst := lwspImm ## x2 ## B"010" ## i(11 downto 7) ## B"0000111" } // C.FLWSP
      if (xlen == 64) is(19) { // C.LDSP
        ret.inst := ldspImm ## x2 ## B"011" ## i(11 downto 7) ## B"0000011"
        ret.illegal setWhen (i(11 downto 7) === 0) //rd!=0
      }
      is(20) {
        val add = B"000_0000" ## i(6 downto 2) ## (i(12) ? i(11 downto 7) | x0) ## B"000" ## i(11 downto 7) ## B"0110011"   //add => add rd, rd, rs2  mv => add rd, x0, rs2
        val j =  B"0000_0000_0000" ## i(11 downto 7) ## B"000" ## (i(12) ? x1 | x0)  ## B"1100111"  //jr => jalr x0, rs1, 0.    jalr => jalr x1, rs1, 0.
        val ebreak = B"000000000001_00000_000_00000_1110011" //EBREAK
        val addJ = (i(6 downto 2) === 0) ? j | add
        ret.inst := (i(12 downto 2) === B"100_0000_0000") ? ebreak | addJ
        ret.illegal setWhen(i(11 downto 7) === 0 && i(6 downto 2) === 0 && i(12) === False) //rs!=0 for C.JR
      }

      if(rvd) is(21){ret.inst := sdspImm(11 downto 5) ## i(6 downto 2)  ## x2 ## B"011" ## sdspImm(4 downto 0) ## B"0100111" } // C.FSDSP
      is(22){ ret.inst := swspImm(11 downto 5) ## i(6 downto 2)  ## x2 ## B"010" ## swspImm(4 downto 0) ## B"0100011" }
      if(xlen == 32 && rvf) is(23){ret.inst := swspImm(11 downto 5) ## i(6 downto 2)  ## x2 ## B"010" ## swspImm(4 downto 0) ## B"0100111" } // C.FSwSP
      if(xlen == 64) is(23){ret.inst := sdspImm(11 downto 5) ## i(6 downto 2)  ## x2 ## B"011" ## sdspImm(4 downto 0) ## B"0100011" } // C.SDSP
    }
    ret
  }
}