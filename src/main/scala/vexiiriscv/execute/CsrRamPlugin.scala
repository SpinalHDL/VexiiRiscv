package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Handle
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.fetch.InitService
import vexiiriscv.riscv.Riscv

import scala.collection.mutable.ArrayBuffer

class CsrRamPlugin extends FiberPlugin with CsrRamService with InitService {
  val allocations = ArrayBuffer[CsrRamAllocation]()
  val reads = ArrayBuffer[Handle[CsrRamRead]]()
  val writes = ArrayBuffer[Handle[CsrRamWrite]]()

  override def ramAllocate(entries: Int = 1) : CsrRamAllocation= allocations.addRet(new CsrRamAllocation(entries))
  override def ramReadPort(priority : Int) : Handle[CsrRamRead] = reads.addRet(Handle(CsrRamRead(setup.addressWidth, Riscv.XLEN.get, priority)))
  override def ramWritePort(priority : Int)  : Handle[CsrRamWrite] = writes.addRet(Handle(CsrRamWrite(setup.addressWidth, Riscv.XLEN.get, priority)))

  override def initHold(): Bool = !logic.flush.done

  val setup = during build new Area{
    allocationLock.await()

    val initPort = ramWritePort(CsrRamService.priority.INIT)

    val sorted = allocations.sortBy(_.entriesLog2).reverse
    var offset = 0
    for(alloc <- sorted){
      alloc.at = offset
      offset += alloc.entriesLog2
    }

    val addressWidth = log2Up(offset)

    for(alloc <- allocations) alloc.addressWidth = addressWidth
  }

  val logic = during build new Area{
    portLock.await()
    val ws = writes.sortBy(_.priority).reverse
    val rs = reads.sortBy(_.priority).reverse
    writes.clear(); writes ++= ws
    reads.clear() ; reads  ++= rs

    val addressWidth = setup.addressWidth
    val mem = Mem.fill(1 << addressWidth)(Bits(Riscv.XLEN bits))

    val writeLogic = new Area{
      val hits = writes.map(_.valid).asBits
      val hit = hits.orR
      val oh = OHMasking.first(hits)
      val port = mem.writePort
      val reader = writes.reader(oh)

      port.valid := hit
      port.address := reader(_.address)
      port.data := reader(_.data)
      (writes, oh.asBools).zipped.foreach(_.ready :=  _)
    }


    val readLogic = reads.nonEmpty generate new Area{
      val hits = reads.map(_.valid).asBits
      val hit = hits.orR
      val oh = OHMasking.first(hits)
      val sel = OHToUInt(oh)
      val port = mem.readSyncPort()
      val ohReg = RegNext(oh.andMask(port.cmd.valid)) init(0)

      val busy = RegNext(port.cmd.valid) init(False)
      port.cmd.valid := oh.orR && !writeLogic.port.valid && !busy
      port.cmd.payload := reads.map(_.address).read(sel)
      (reads, ohReg.asBools).zipped.foreach(_.ready := _)
      reads.foreach(_.data := port.rsp)
    }

    val flush = new Area {
      val counter = Reg(UInt(log2Up(mem.wordCount) + 1 bits)) init (0)
      val done = counter.msb

      setup.initPort.valid := !done
      setup.initPort.address := counter.resized
      setup.initPort.data := 0
      counter := counter + (!done).asUInt
    }
  }
}


//val readLogic = reads.nonEmpty generate new Area {
//  val hits = reads.map(_.valid).asBits
//  val hit = hits.orR
//  val oh = OHMasking.first(hits)
//  val sel = OHToUInt(oh)
//  val cmd = Stream(mem.addressType)
//  val rsp = mem.streamReadSync(cmd, oh.andMask(cmd.valid))
//  val rspBuffer = rsp.toFlow.stage()
//
//  cmd.valid := oh.orR && !writeLogic.port.valid
//  cmd.payload := reads.map(_.address).read(sel)
//  (reads, rspBuffer.linked.asBools).zipped.foreach(_.ready := _)
//  reads.foreach(_.data := rspBuffer.value)
//}