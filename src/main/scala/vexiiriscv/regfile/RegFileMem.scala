package vexiiriscv.regfile

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.compat.{RamAsyncMwMux, RamAsyncMwReg, RamMwIo, RamSyncMwMux}
import vexiiriscv.riscv.RegfileSpec

import scala.collection.mutable.ArrayBuffer

case class RegFileReadParameter(withReady : Boolean)
case class RegFileWriteParameter(withReady : Boolean)


/**
 * Implement a register file with multiple read/write ports and different topology to imeplement those ports
 * The bankCount is currently useless, but maybe useful in the future with execution units which can stall
 */
class RegFileMem(rfpp : RegFilePortParam,
                 readsParameter  : scala.collection.Seq[RegFileReadParameter],
                 writesParameter : scala.collection.Seq[RegFileWriteParameter],
                 headZero        : Boolean,
                 syncRead        : Boolean,
                 dualPortRam     : Boolean,
                 regBasedRam     : Boolean,
                 asyncReadBySyncReadRevertedClk : Boolean = false,
                 maskReadDuringWrite: Boolean = true) extends Component {
  import rfpp._

  val io = RegFileIo(rfpp, readsParameter, writesParameter)
  io.reads.foreach(e => assert(!e.withReady))
  io.writes.foreach(e => assert(!e.withReady))

  val conv = RamMwIo(Bits(rfpp.dataWidth bits), 1 << rfpp.addressWidth, writesParameter.size, readsParameter.size).setAsDirectionLess()
  for((to, from) <- (conv.writes, io.writes).zipped){
    to.valid := from.valid
    to.address := from.address
    to.data := from.data
  }
  for ((to, from) <- (conv.read, io.reads).zipped) {
    to.cmd.valid := from.valid
    to.cmd.payload := from.address
    from.data := to.rsp
  }

  val infered = writesParameter.size == 1 || !dualPortRam
  val asMem = (infered && !regBasedRam) generate new Area {
    val ram = Mem.fill((1 << addressWidth))(Bits(dataWidth bits))
    Verilator.public(ram)

    val writes = for ((w, i) <- conv.writes.zipWithIndex) yield new Area {
      val port = ram.writePort()
      port.valid := w.valid
      port.address := w.address
      port.data := w.data
    }

    val reads = for ((r, i) <- conv.read.zipWithIndex) yield new Area {
      val async = !syncRead generate new Area {
        val port = if (asyncReadBySyncReadRevertedClk) ram.readAsyncPortBySyncReadRevertedClk else ram.readAsyncPort
        port.address := r.cmd.payload
        r.rsp := port.data
      }
      val sync = syncRead generate new Area {
        val port = ram.readSyncPort
        port.cmd.valid := r.cmd.valid
        port.cmd.payload := r.cmd.payload
        r.rsp := port.rsp
      }
    }
  }

  val asReg = (infered && regBasedRam) generate new Area {
    assert(!syncRead)
    val ram = Vec.fill((1 << addressWidth))(Reg(Bits(dataWidth bits)))
    Verilator.public(ram)

    for(addr <- ram.indices){
      val oh = conv.writes.map(p => p.valid && p.address === addr)
      val data = OhMux.or(oh, conv.writes.map(_.data))
      when(oh.orR){
        ram(addr) := data
      }
    }

    val reads = for ((r, i) <- conv.read.zipWithIndex) yield new Area {
      val async = !syncRead generate new Area {
        r.rsp := ram.read(r.cmd.payload)
      }
    }
  }

  val asAsyncDp = (!infered && !syncRead) generate {
    assert(!asyncReadBySyncReadRevertedClk)
    val logic = new RamAsyncMwMux(Bits(rfpp.dataWidth bits), 1 << rfpp.addressWidth, writesParameter.size, readsParameter.size)
    logic.io <> conv
    logic.location.ram.foreach(_.randBoot())
  }

  val asSyncDp = (!infered && syncRead) generate {
    assert(!asyncReadBySyncReadRevertedClk)
    val logic = new RamSyncMwMux(Bits(rfpp.dataWidth bits), 1 << rfpp.addressWidth, writesParameter.size, readsParameter.size)
    logic.location.ram.foreach(_.randBoot())
    logic.io <> conv
  }
}