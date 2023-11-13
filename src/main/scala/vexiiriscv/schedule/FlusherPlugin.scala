package vexiiriscv.schedule

import spinal.core._
import spinal.lib._
import spinal.lib.Flow
import spinal.lib.misc.plugin.FiberPlugin

import scala.collection.mutable.ArrayBuffer

class FlusherPlugin extends FiberPlugin with FlusherService {
  val cmds = ArrayBuffer[Flow[FlushCmd]]()
  override def createFlushCmd(priority: Int): Flow[FlushCmd] = cmds.addRet(Flow(FlushCmd(priority)))
  override def getFlushCmds(): Seq[Flow[FlushCmd]] = {
    lock.await()
    cmds
  }
}
