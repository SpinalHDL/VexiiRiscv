package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.Handle
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.fetch.InitService
import vexiiriscv.riscv.Riscv

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CsrRamPlugin extends FiberPlugin with CsrRamService with InitService {

  override def initHold(): Bool = !logic.flush.done
  override def portAddressWidth: Int = setup.addressWidth


  override def holdCsrRead(): Unit = api.holdRead := True
  override def holdCsrWrite(): Unit = api.holdWrite := True
  val api = during build new Area{
    val holdRead = False
    val holdWrite = False
  }

  val csrMapper = during setup new Area{
    val cas = host[CsrAccessPlugin]
    val portRetainer = portLock()
    val casRetainer = cas.csrLock()
    awaitBuild()
    csrLock.await()

    assert(Global.HART_COUNT.get == 1, "In general, all csrram access done by other plugins assume 1 hart, need to be update")

    val read = ramReadPort(CsrRamService.priority.CSR)
    val write = ramWritePort(CsrRamService.priority.CSR)
    portRetainer.release()

    val RAM_ADDRESS = misc.pipeline.Payload(UInt(read.addressWidth bits))
    val ramAddress = RAM_ADDRESS()

    // Decode stuff
    val ramAddressMask = ramAddress.maxValue
    val addressDecoder = new DecodingSpec(RAM_ADDRESS)
    val selDecoder = ArrayBuffer[Int]()
    switch(cas.bus.decode.address) {
      for (e <- csrMappings) {
        e.csrFilter match {
          case filter: CsrListFilter => for (csrId <- filter.mapping) {
            val mask = Masked(csrId, 0xFFF)
            addressDecoder.addNeeds(mask, Masked(e.alloc.at + e.offset, ramAddressMask))
            selDecoder += csrId
          }
          case csrId: Int => {
            is(csrId) {
              val mask = Masked(csrId, 0xFFF)
              addressDecoder.addNeeds(mask, Masked(e.alloc.at + e.offset, ramAddressMask))
              selDecoder += csrId
            }
          }
        }
      }
    }

    ramAddress := addressDecoder.build(cas.bus.decode.address.asBits, Nil)
    val selFilter = CsrListFilter(selDecoder)
    cas.allowCsr(selFilter)

    // Read stuff
    val withRead = False
    cas.onRead(selFilter, false)(withRead := True)
    read.valid := withRead && !api.holdRead
    read.address := ramAddress
    cas.readAlways(read.data.andMask(withRead))
    when (withRead && !read.ready){
      cas.bus.read.doHalt()
    }

    // Write stuff
    val doWrite = False
    cas.onWrite(selFilter, false)(doWrite := True)
    val fired = RegInit(False) setWhen (write.fire) clearWhen (cas.bus.write.moving)
    write.valid := doWrite && !fired && !api.holdWrite
    write.address := ramAddress
    write.data := cas.bus.write.bits
    when ((doWrite && !fired) && !write.ready){
      cas.bus.write.doHalt()
    }

    casRetainer.release()
  }


  override def awaitMapping(): Unit = setup.get
  val setup = during build new Area{
    csrLock.await()
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