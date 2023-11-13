package vexiiriscv.schedule

import spinal.core._
import spinal.lib._
import spinal.core.fiber.Lockable
import vexiiriscv.Global._

case class FlushCmd(priority : Int) extends Bundle{
  val hartId = HART_ID()
}

trait FlusherService extends Lockable{
  def createFlushCmd(priority : Int) : Flow[FlushCmd]
  def getFlushCmds(): Seq[Flow[FlushCmd]]
}
