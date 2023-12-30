package vexiiriscv.execute


import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable
import vexiiriscv.Generate.args
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.riscv.{IntRegFile, RS1, RS2, Riscv}

//This plugin example will add a new instruction named SIMD_ADD which do the following :
//
//RD : Regfile Destination, RS : Regfile Source
//RD( 7 downto  0) = RS1( 7 downto  0) + RS2( 7 downto  0)
//RD(16 downto  8) = RS1(16 downto  8) + RS2(16 downto  8)
//RD(23 downto 16) = RS1(23 downto 16) + RS2(23 downto 16)
//RD(31 downto 24) = RS1(31 downto 24) + RS2(31 downto 24)
//
//Instruction encoding :
//0000000----------000-----0001011   <- Custom0 func3=0 func7=0
//       |RS2||RS1|   |RD |
//
//Note :  RS1, RS2, RD positions follow the RISC-V spec and are common for all instruction of the ISA


object SimdAddPlugin{
  //Define the instruction type and encoding that we wll use
  val ADD4 = IntRegFile.TypeR(M"0000000----------000-----0001011")
}

//ExecutionUnitElementSimple Is a base class which will be coupled to the pipeline provided by a ExecutionUnitBase with
//the same euId. It provide quite a few utilities to ease the implementation of custom instruction.
//Here we will implement a plugin which provide SIMD add on the register file.
//staticLatency=true specify that our plugin will never halt the pipeling, allowing the issue queue to statically
//wake up instruction which depend on its result.
class SimdAddPlugin(val layer : LaneLayer) extends ExecutionUnitElementSimple(layer)  {

  //The setup code is by plugins to specify things to each others before it is too late
  //create early blockOfCode will
  val logic = during setup new Logic{
    //Let's assume we only support RV32 for now
    awaitBuild()

    assert(Riscv.XLEN.get == 32)

    val wb = ifp.access(0)
    implicit val _ = ifp -> wb
    
    //Specify that the current plugin will implement the ADD4 instruction
    add(SimdAddPlugin.ADD4)
    layer(SimdAddPlugin.ADD4).addRsSpec(RS1, 0)
    layer(SimdAddPlugin.ADD4).addRsSpec(RS2, 0)


    uopRetainer.release()

    val process = new eu.Execute(id = 0) {
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = eu(IntRegFile, RS1).asUInt
      val rs2 = eu(IntRegFile, RS2).asUInt

      //Do some computation
      val rd = UInt(32 bits)
      rd( 7 downto  0) := rs1( 7 downto  0) + rs2( 7 downto  0)
      rd(16 downto  8) := rs1(16 downto  8) + rs2(16 downto  8)
      rd(23 downto 16) := rs1(23 downto 16) + rs2(23 downto 16)
      rd(31 downto 24) := rs1(31 downto 24) + rs2(31 downto 24)

      //Provide the computation value for the writeback
      wb.valid := SEL
      wb.payload := rd.asBits
    }
  }
}



object Generate extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    param.addOptions(this)
  }.parse(args, Unit).nonEmpty)

  sc.addTransformationPhase(new MultiPortWritesSymplifier)
  val report = sc.generateVerilog {
    val pa = param.pluginsArea()
    pa.plugins += new SimdAddPlugin(pa.early0)
    VexiiRiscv(pa.plugins)
  }
}

