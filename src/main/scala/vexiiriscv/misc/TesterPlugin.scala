package vexiiriscv.misc

import spinal.core._
import spinal.lib.{CounterUpDown, Timeout}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.{CsrService, ExecuteLanePlugin, ExecutePipelinePlugin}
import vexiiriscv.execute.lsu.LsuPlugin
import vexiiriscv.memory.{AddressTranslationService, MmuPlugin}

import scala.collection.mutable.ArrayBuffer

class TesterPlugin extends FiberPlugin{
  val logic = during build new Area{
    val l = ArrayBuffer(
      host[TrapPlugin].api.harts(0).holdPrivChange,
      host[CsrService].bus.decode.fence
    )
    host.get[LsuPlugin].map(l += _.logic.onCtrl.fenceTrap.enable)
    val counter = Reg(UInt(12 bits)) init(0)
    counter := counter + 1
    when(counter(6, 2 bits) === 0) {
      l.foreach(_:= True)
    }
  }

  class Lsfr(width : Int = 32, mask : BigInt = BigInt(0xC0000401l), seed : BigInt = BigInt(0xC0000401l)) extends Area{
    val value = Reg(Bits(width bits)) init(seed)
    val enable = Bool()
    when(enable){
      value := ((value & mask).xorR ## value) >> 1
    }
  }

  val ptw = during setup (host.get[MmuPlugin].nonEmpty generate new Area{
    val ats = host[AddressTranslationService]
    val earlyLock = retains(ats.portsLock)

    awaitBuild()

    val ptw = ats.newRefillPort()
    earlyLock.release()

    val random = new Lsfr(seed = 0x12345678)
    random.enable := !ptw.cmd.isStall

    val address = new Lsfr()
    address.enable := !ptw.cmd.isStall

    ptw.cmd.valid := random.value(3, 9 bits) === 0
    ptw.cmd.address := address.value.asSInt.resize(widthOf(ptw.cmd.address)).asUInt
    ptw.cmd.storageId := 0
    ptw.cmd.storageEnable := False

    val pending = CounterUpDown(4, ptw.cmd.fire, ptw.rsp.fire)
    when(pending.mayOverflow){
      ptw.cmd.valid := False
    }

    val timeout = Timeout(10000)
    when(pending === 0 || ptw.rsp.fire){
      timeout.clear()
    }
    val el = host[ExecutePipelinePlugin]
    when(timeout.state){
      el.freezeIt()
    }
  })
}
