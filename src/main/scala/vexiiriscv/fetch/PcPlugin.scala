package vexiiriscv.fetch

import spinal.core._
import spinal.lib.Flow
import spinal.lib.misc.plugin._
import vexiiriscv.Global._

class PcPlugin(var resetVector : BigInt = 0x80000000l) extends FiberPlugin with HartService{
  lazy val pp = host[PipelinePlugin]
  during setup {
    pp.retain()
  }

  override def createJumpInterface(priority: Int): Flow[JumpCmd] = ???

  val logic = during build new Area{
    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      val pc = Reg(PC) init(resetVector)
      pc := pc  + 1
    }

    assert(HART_COUNT.get() == 1)

    val injectStage = pp.ctrl(0).up
    val inject = new injectStage.Area {
      valid := True
      PC := harts(0).pc
      HART_ID := 0
    }
    pp.release()
  }
}
