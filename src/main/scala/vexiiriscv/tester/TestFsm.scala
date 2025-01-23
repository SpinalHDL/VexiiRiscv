package vexiiriscv.tester

import spinal.core.sim.{delayed, simSuccess}

/**
 * Here are a set of classes which are used to build directed VexiiRiscv test cases.
 * For instance, to test if VexiiRiscv can boot linux in a simulation, they can be used to encode the expected sequence of
 * terminal stdout/stdin to observe/produce until the sim can be considered a success.
 */
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