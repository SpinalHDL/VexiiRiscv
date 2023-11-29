package vexiiriscv.regfile

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.riscv.RegfileSpec

import scala.collection.mutable.ArrayBuffer

case class RegFileReadParameter(withReady : Boolean)
case class RegFileWriteParameter(withReady : Boolean)


//The bankCount is currently useless, but maybe useful in the future with execution units which can stall
class RegFileMem(rfpp : RegFilePortParam,
                 readsParameter  : Seq[RegFileReadParameter],
                 writesParameter : Seq[RegFileWriteParameter],
//                 bypassCount     : Int,
                 preferedWritePortForInit : Int,
                 headZero        : Boolean,
                 syncRead        : Boolean,
                 asyncReadBySyncReadRevertedClk : Boolean = false) extends Component {
  import rfpp._

  val io = RegFileIo(rfpp, readsParameter, writesParameter)
  io.reads.foreach(e => assert(!e.withReady))
  io.writes.foreach(e => assert(!e.withReady))
  val ram = Mem.fill((1 << addressWidth))(Bits(dataWidth bits))
  Verilator.public(ram)

  io.initDone := True
  val writes = for ((w, i) <- io.writes.zipWithIndex) yield new Area {
    //    ram.write(w.address, w.data, w.valid)
    val port = ram.writePort()
    port.valid := w.valid
    port.address := w.address
    port.data := w.data

    if (i == preferedWritePortForInit) {
      if (headZero) {
        val counter = Reg(UInt(addressWidth + 1 bits)) init (0)
        val done = counter.msb
        when(!done) {
          port.valid := True
          port.address := counter.resized
          port.data := 0
          counter := counter + 1
          io.initDone := False
        }
      }
    }
  }

  val reads = for ((r, i) <- io.reads.zipWithIndex) yield new Area {
    val async = !syncRead generate new Area {
      val port = if (asyncReadBySyncReadRevertedClk) ram.readAsyncPortBySyncReadRevertedClk else ram.readAsyncPort
      port.address := r.address
      r.data := port.data
    }
    val sync = syncRead generate new Area {
      val port = ram.readSyncPort
      port.cmd.valid := r.valid
      port.cmd.payload := r.address
      r.data := port.rsp
    }
  }
}