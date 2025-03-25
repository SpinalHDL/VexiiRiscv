package vexiiriscv.tester

import spinal.core.sim.{delayed, simSuccess}
import vexiiriscv.soc.litex.SocSim.fsmTasksGen

import scala.collection.mutable

/**
 * Here are a set of classes which are used to build directed VexiiRiscv test cases.
 * For instance, to test if VexiiRiscv can boot linux in a simulation, they can be used to encode the expected sequence of
 * terminal stdout/stdin to observe/produce until the sim can be considered a success.
 *
 * For instance, here is a sequence of option to handle buildroot :
 * --fsm-sleep 1000000
 * --fsm-putc root
 * --fsm-putc-lr
 * --fsm-getc #
 * --fsm-putc "cat /proc/cpuinfo"
 * --fsm-putc-lr
 * --fsm-getc #
 * --fsm-success
 */
object FsmOption{
  def apply(parser: scopt.OptionParser[Unit], fsmTasksGen : mutable.Queue[() => FsmTask]): Unit = {
    parser.opt[String]("fsm-putc").unbounded() action { (v, c) => fsmTasksGen += (() => new FsmPutc(v)) }
    parser.opt[Unit]("fsm-putc-lr").unbounded() action { (v, c) => fsmTasksGen += (() => new FsmPutc("\n")) }
    parser.opt[String]("fsm-getc").unbounded() action { (v, c) => fsmTasksGen += (() => new FsmGetc(v)) }
    parser.opt[Long]("fsm-sleep").unbounded() action { (v, c) => fsmTasksGen += (() => new FsmSleep(v)) }
    parser.opt[Unit]("fsm-success").unbounded() action { (v, c) => fsmTasksGen += (() => new FsmSuccess()) }
  }
}

/**
 * Provide a default implementation to be used in testbenches
 * Tasks are builded from a queue of landa function (To allow dual sim)
 *
 * The testbench need to interface with putcQueue and tasks.head.getc
 */
class FsmHalGen(fsmTasksGen : mutable.Queue[() => FsmTask]) extends FsmHal{
  val tasks =  mutable.Queue[FsmTask]()
  for(gen <- fsmTasksGen) tasks += gen()
  val putcQueue = mutable.Queue[Byte]()
  override def next(): Unit = {
    if (tasks.nonEmpty) tasks.dequeue()
    if (tasks.nonEmpty) tasks.head.start(this)
  }
  override def putc(value: String): Unit = putcQueue ++= value.map(_.toByte)
  if (tasks.nonEmpty) tasks.head.start(this)
}

trait FsmHal{
  def putc(value : String) : Unit
  def next() : Unit
}
trait FsmTask{
  def start(hal : FsmHal) : Unit = {}
  def getc(hal : FsmHal, c : Char) : Unit = {}
}
class FsmGetc(value : String) extends FsmTask{
  val buffer = new StringBuilder()
  override def getc(hal : FsmHal, c: Char): Unit = {
    buffer += c
    if(buffer.toString().endsWith(value)){
      hal.next()
    }
  }
}
class FsmPutc(value : String) extends FsmTask{
  override def start(hal: FsmHal): Unit = {
    hal.putc(value)
    hal.next()
  }
}
class FsmSleep(value : Long) extends FsmTask{
  override def start(hal: FsmHal): Unit = {
    delayed(value) {
      hal.next()
    }
  }
}
class FsmSuccess extends FsmTask{
  override def start(hal: FsmHal): Unit = simSuccess()
}