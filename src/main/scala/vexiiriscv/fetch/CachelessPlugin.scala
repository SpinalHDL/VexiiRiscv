package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.database.Database._
import vexiiriscv._
import vexiiriscv.Global._

case class CachelessParam(addressWidth : Int, dataWidth : Int, idWidth : Int)

case class CachelessCmd(p : CachelessParam) extends Bundle{
  val id = UInt(p.idWidth bits)
  val address = UInt(p.addressWidth bits)
}

case class CachelessRsp(p : CachelessParam) extends Bundle{
  val id = UInt(p.idWidth bits)
  val error = Bool()
  val word  = Bits(p.dataWidth bits)
}

case class CachelessBus(p : CachelessParam) extends Bundle with IMasterSlave {
  var cmd = Stream(CachelessCmd(p))
  var rsp = Flow(CachelessRsp(p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

object CachelessPlugin{
  val ID_WIDTH = blocking[Int]
  val ID = blocking[Int]
}

class CachelessPlugin(var wordWidth : Int,
                      var cmdAt : Int = 0,
                      var rspAt : Int = 1) extends FiberPlugin{
  lazy val pp = host[PipelinePlugin]
  during setup{
    pp.retain()
    FETCH_WORD_WIDTH.set(wordWidth)
  }
  val logic = during build new Area{
    val idWidth = 0
    val p = CachelessParam(MIXED_WIDTH, FETCH_WORD_WIDTH, idWidth)
    val bus = master(CachelessBus(p))
    val x = CombInit(pp.ctrl(2)(Global.PC))
    bus.cmd.setIdle()
    pp.release()
  }
}
