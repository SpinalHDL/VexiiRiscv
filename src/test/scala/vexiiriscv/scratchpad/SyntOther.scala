package vexiiriscv.scratchpad
import spinal.core.{in, _}
import spinal.lib._
import spinal.lib.eda.bench._

import scala.collection.mutable

object SyntOther extends App{

//  class Pipeline {
//    //Define the pipeline data model
//        val specs = mutable.LinkedHashMap[NamedType[Data], mutable.LinkedHashMap[Int, Data]]()
//        val enable = mutable.LinkedHashMap[Int, Bool]()
//
//    //Define how we can access the pipeline
//    def apply[T <: Data](what: NamedType[T], stageId: Int) = {
//      val spec = specs.getOrElseUpdate(what.asInstanceOf[NamedType[Data]], new mutable.LinkedHashMap[Int, Data])
//      spec.getOrElseUpdate(stageId, what().setName(what.getName + "_" + stageId)).asInstanceOf[T]
//    }
//
//    //Connect all those handsomes together
//    def build(): Unit = {
//      for ((what, nodes) <- specs) {
//        for (i <- nodes.keys.min until nodes.keys.max) {
////          apply(what, i + 1) := RegNext(apply(what, i))
//          apply(what, i + 1) := RegNextWhen(apply(what, i), enable.getOrElseUpdate(i, Delay(in(Bool()), 4)))
//        }
//      }
//    }
//  }
//
//  val retim = Rtl(SpinalVerilog(new Component {
//    setDefinitionName("retim")
//    val width = 256
//    val sel = in UInt(log2Up(width) bits)
//    val a,b = in UInt (width bits)
//    val result = out UInt(width bits)
//
//    val pip = new Pipeline()
//    val SEL = NamedType(UInt(log2Up(width) bits))
//    val A, RESULT = NamedType(UInt(width bits))
//    pip(A,0) := a
////    pip(B, 0) := b
//    pip(SEL, 0) := sel
////    pip(RESULT, 4) := pip(A, 4) + pip(B, 4)
//    pip(RESULT, 4) := pip(A, 4) >> pip(SEL, 4)
//    result := pip(RESULT,8)
//    pip.build()
//  }))
//
//
//  val shift = Rtl(SpinalVerilog(Rtl.ffIo(new Component {
//    setDefinitionName("shift")
//    val dataIn = in Bits(64 bits)
//    val sel = in UInt(6 bits)
//    val dataOut = out(dataIn >> sel)
//  })))
//
//  val shiftScrap = Rtl(SpinalVerilog(Rtl.ffIo(new Component {
//    setDefinitionName("shiftScrap")
//    val dataIn = in Bits (64 bits)
//    val sel = in UInt (6 bits)
//    val dataOut = out(Shift.rightWithScrap(dataIn, sel))
//  })))
class EFX_COMB4(LUTMASK: Int, MODE: String) extends BlackBox {
  val I0, I1, I2, I3 = in Bool()
  val CI = in Bool()
  val CO = out Bool()
  val O, P, FCO = out Bool()

  addGeneric("LUTMASK", LUTMASK)
  addGeneric("MODE", MODE)
}
  def carry(a: UInt, b: UInt, cin: Bool) = new Area {
    val width = widthOf(a) max widthOf(b)
    val blockWidth = 2
    var cInit = BigInt(0)
    var i = 0
    for (x <- 0 until 1 << blockWidth; y <- 0 until 1 << blockWidth) {
      if (x + y >= (1 << blockWidth)) cInit = cInit | (BigInt(1) << i)
      i += 1
    }
    val cRom = B(cInit, 1 << blockWidth * 2 bits)



    def block(a: UInt, b: UInt, cin: Bool) = new Area{
      val cat = a @@ b
      val c = cRom(cat)
      val p = (a.asBools, b.asBools).zipped.map(_ ^ _).andR
//      val cout = (cin && p) | c
      val bb = new EFX_COMB4(0xC2EC, "ARITH")
      bb.I0 := p
      bb.I1 := c
      bb.I2 := False
      bb.I3 := False
      bb.CI := cin
      val cout = Bool()
      cout := bb.CO
    }



    i = 0
    var carry = cin
    val blocks = for(i <- 0 until width by blockWidth) yield {
      val area = block(a(i, blockWidth bits), b(i, blockWidth bits), carry)
      carry = area.cout
      area
    }

//    carry = blocks.last.bb.O

    val ref = (a+^b).msb
  }


  import spinal.core.sim._
  val adderWidth = 64

//  SimConfig.withFstWave.compile(new Module {
//    val a, b = in UInt(adderWidth bits)
//    val result = out UInt(adderWidth bits)
//    val cutAt = 32
//    val ctmp = carry(a(0, cutAt bits), b(0, cutAt bits), False)
//    result := (a(cutAt, adderWidth-cutAt bits) + b(cutAt, adderWidth-cutAt bits) + ctmp.carry.asUInt) @@ (a(0, cutAt bits) + b(0, cutAt bits))
//
//  }).doSim(seed = 32){dut =>
//    for(i <- 0 until 1000){
//      val a = dut.a.randomize()
//      val b = dut.b.randomize()
//      sleep(10)
//      val got = dut.result.toBigInt
//      val ref = (a + b) & ((BigInt(1) << adderWidth) -1)
//      assert(got == ref, f"${got}%x ${ref}%x")
//    }
//  }


  val adder = Rtl(SpinalVerilog(new Component {
    setDefinitionName("shiftScrap")
    val serIn = in Bool()
    val serOut = out Bool()
    val serInBuf = Delay(serIn, 4)

    val a,b = Reg(UInt(adderWidth bits))
    val result = UInt(adderWidth bits)



//    result := a+b
    val cutAt = 48
    val ctmp = carry(a(0, cutAt bits), b(0, cutAt bits), False)

    var cin = False
    val blocks = for (i <- 0 until adderWidth) yield {
      if(i == cutAt) cin = ctmp.carry
      val bb = new EFX_COMB4(0xC2EC, "ARITH") //0xC2EC need to be changed to a real value
      bb.I0 := a(i)
      bb.I1 := b(i)
      bb.I2 := False
      bb.I3 := False
      bb.CI := cin
      cin = bb.CO
      result(i) := bb.O
    }



//    result := (a(cutAt, adderWidth - cutAt bits) + b(cutAt, adderWidth - cutAt bits) + ctmp.carry.asUInt) @@ (a(0, cutAt bits) + b(0, cutAt bits))

    a := (a ## serIn).asUInt.resized
    b := (b ## a.msb).asUInt.resized
    serOut := Delay(result.msb, 4)
  }))


  val ram = Rtl(SpinalVerilog(new Component {
    val ram = Mem.fill(4*8*1024)(Bits(64 bits))
    val write = slave(ram.writePortWithMask(8))
    val read = slave(ram.readSyncPort())
    ram.generateAsBlackBox()
  }))

  val floor = Rtl(SpinalVerilog(new Component {
    val a,b,c,d = in UInt(8 bits)
    val x = out(RegNext(a ^ b))
    val y = out(RegNext(a + c))
    val z = out(RegNext(a & d))
  }))

  val rtls = List(floor)

  val targets = EfinixStdTargets().drop(2)

  Bench(rtls, targets)
}

/*
shift ->
Artix 7 -> 95 Mhz 177 LUT 402 FF
Artix 7 -> 369 Mhz 192 LUT 402 FF
shiftScrap ->
Artix 7 -> 95 Mhz 207 LUT 402 FF
Artix 7 -> 261 Mhz 231 LUT 402 FF
 */