package vexiiriscv.tester

import java.io.File
import scala.collection.mutable.ArrayBuffer

/**
 * This is a test argument builder, which is just there to make it easy to build specific testbenches in the regression environnement
 */
class TestArgs{
  val args = ArrayBuffer[String]()
  def dualSim() : this.type = {args ++= List("--dual-sim"); this }
  def withWave() : this.type = {args ++= List("--with-wave"); this }
  def withKonata() : this.type = {args ++= List("--with-konata"); this }
  def withRvlsLog() : this.type = {args ++= List("--with-rvls-log"); this }
  def withSpikeLog() : this.type = {args ++= List("--with-spike-log"); this }
  def printStats() : this.type = {args ++= List("--print-stats"); this }
  def traceAll() : this.type = {args ++= List("--trace-all"); this }
  def noProbe() : this.type = {args ++= List("--no-probe"); this }
  def noRvlsCheck() : this.type = {args ++= List("--no-rvls-check"); this }
  def noStdin() : this.type = {args ++= List("--no-stdin"); this }
  def fsmSuccess() : this.type = {args ++= List("--fsm-success"); this }

  def name(name : String) : this.type = {args ++= List("--name", name); this }
  def failAfter(value : Long) : this.type = {args ++= List("--fail-after", value.toString); this }
  def passAfter(value : Long) : this.type = {args ++= List("--pass-after", value.toString); this }
  def simSpeedPrinter(value : Double) : this.type = {args ++= List("--sim-speed-printer", value.toString); this }
  def loadElf(value : String) : this.type = {args ++= List("--load-elf", value); this }
  def loadElf(value : File) : this.type = loadElf(value.getAbsolutePath)
  def startSymbol(value : String) : this.type = {args ++= List("--start-symbol", value); this }
  def passSymbol(value : String) : this.type = {args ++= List("--pass-symbol", value); this }
  def startSymbolOffset(value : Int) : this.type = {args ++= List("--start-symbol-offset", value.toString); this }
  def fsmGetc(value : String) : this.type = {args ++= List("--fsm-getc", value); this }
  def fsmPutc(value : String) : this.type = {args ++= List("--fsm-putc", value); this }
  def fsmPutcLr() : this.type = {args ++= List("--fsm-putc-lr"); this }
  def fsmSleep(value : Long) : this.type = {args ++= List("--fsm-sleep", value.toString); this }
  def ibusReadyFactor(value : Double) : this.type = {args ++= List("--ibus-ready-factor", value.toString); this }
  def dbusReadyFactor(value : Double) : this.type = {args ++= List("--dbus-ready-factor", value.toString); this }

  def loadBin(address : Long, file : String) : this.type = {args ++= List("--load-bin", f"0x${address.toHexString},$file"); this }
  def loadU32(address : Long, value : Long) : this.type = {args ++= List("--load-u32", f"0x${address.toHexString},0x${value.toHexString}"); this }
}