// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.misc

import spinal.core._
import spinal.lib._

case class DivCmd(width : Int, radixBits : Int) extends Bundle{
  val a,b = UInt(width bits)
  val normalized = Bool()
  val iterations = UInt(log2Up(width)+1-radixBits bits)
}

case class DivRsp(width : Int) extends Bundle{
  val result = UInt(width bits)
  val remain = UInt(width bits)
}

case class DivIo(width : Int, radixBits : Int) extends Bundle{
  val flush = in Bool()
  val cmd = slave Stream (DivCmd(width, radixBits))
  val rsp = master Stream (DivRsp(width))
}

class DivComp(val width : Int, val radixBits : Int) extends Component{
  val io = DivIo(width, radixBits)
}

class DivRadix(width: Int, radix: Int) extends DivComp(width, radix match {
  case 2 => 1
  case 4 => 2
}) {

  assert(width % radixBits == 0)

  val iterations = width/radixBits
  val counter = Reg(UInt(log2Up(iterations) bits))
  val busy = RegInit(False) clearWhen(io.rsp.fire)
  val done = RegInit(False) setWhen(busy && counter === iterations-1) clearWhen(io.rsp.fire)

  val shifter = Reg(UInt(width bits))
  val numerator = Reg(UInt(width bits))
  val result = Reg(UInt(width bits))

  val div1, div3 = Reg(UInt(width+radixBits bits))
  val div2 = div1 |<< 1

  val shifted = shifter @@ numerator.takeHigh(radixBits).asUInt
  val sub1 = shifted -^ div1
  val sub2 = (radix >= 4) generate shifted -^ div2
  val sub3 = (radix >= 4) generate shifted -^ div3

  io.rsp.valid := done
  io.rsp.result := result.resized
  io.rsp.remain := shifter.resized
  io.cmd.ready := !busy

  when(!done){
    counter := counter + 1
    val sel = CombInit(shifted)
    result := result |<< radixBits
    when(!sub1.msb){
      sel := sub1.resized
      result(radixBits-1 downto 0) := 1
    }
    if(radix >= 4) {
      when(!sub2.msb) {
        sel := sub2.resized
        result(radixBits-1 downto 0) := 2
      }
      when(!sub3.msb) {
        sel := sub3.resized
        result(radixBits-1 downto 0) := 3
      }
    }
    shifter := sel.resized
    numerator := numerator |<< radixBits
  }

  val sliceCount = 3
  val shiftWidth = width/(sliceCount+1)
  val slices = io.cmd.a.subdivideIn(sliceCount+1 slices).tail
  val slicesZero = slices.map(_ === 0)
  val shiftSel = B((0 until sliceCount).map(i => slicesZero.drop(i).andR))
  val sel = OHToUInt(OHMasking.firstV2(True ## shiftSel))
  val wasBusy = RegNext(busy) init(False)
  when(!busy){
    busy   := io.cmd.valid
    div1   := io.cmd.b.resized
    if(radix >= 4) div3 := io.cmd.b +^ (io.cmd.b << 1)
    result := (default -> (io.cmd.b === 0))
    switch(sel) {
      for (i <- sliceCount downto 0) is(i) {
        val shift = width - ((i + 1) * shiftWidth)
        counter := shift / radixBits
        shifter := U(io.cmd.a.takeHigh(shift)).resized
        numerator := io.cmd.a |<< shift
      }
    }
    when(io.cmd.normalized){
      counter := iterations-1-io.cmd.iterations
      shifter := U(io.cmd.a.dropLow(radixBits)).resized
      numerator := io.cmd.a |<< (width - radixBits)
    }
  }

  when(io.flush){
    done := False
    busy := False
  }
}


object DivRadix4Tester extends App{
  import spinal.core.sim._

  for(radix <- List(4)){
    SimConfig.withFstWave.compile(new DivRadix(56, radix)).doSim(seed = 52){ dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmd.valid #= false
      dut.io.rsp.ready #= true
      dut.io.cmd.normalized #= false
      dut.clockDomain.waitSampling()
      for (i <- 0 until 100) {
        dut.io.cmd.valid #= true
//        val a = dut.io.cmd.a.randomizedBigInt()
//        val b = dut.io.cmd.b.randomizedBigInt()
        val a = 20
        val b = 6
        dut.io.cmd.a #= a
        dut.io.cmd.b #= b
        dut.clockDomain.waitSampling()
        dut.io.cmd.valid #= false
        dut.clockDomain.waitSamplingWhere(dut.io.rsp.valid.toBoolean)
        assert(dut.io.rsp.result.toBigInt == a / b)
        assert(dut.io.rsp.remain.toBigInt == a % b)
        dut.clockDomain.waitSampling()
      }
      simSuccess()
    }
  }
}