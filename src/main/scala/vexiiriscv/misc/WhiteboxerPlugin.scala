package vexiiriscv.misc

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.{Decode, DecodePipelinePlugin, DecoderPlugin}
import vexiiriscv.execute.{CompletionService, ExecuteUnitService, WriteBackPlugin}
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.IntRegFile

import scala.collection.mutable.ArrayBuffer

class WhiteboxerPlugin extends FiberPlugin{
  addLockable(host[PipelineBuilderPlugin])

  val logic = during build new Area{
    def wrap[T <: Data](that : T) : T = CombInit(that).simPublic

    val fpp = host[FetchPipelinePlugin]
    val dpp = host[DecodePipelinePlugin]
    val fetch = new Area {
      val c = fpp.ctrl(0)
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val fetchId = wrap(c(Fetch.ID))
    }

    val decodes =  for(lane <- 0 until Decode.LANES) yield new Area {
      val c = dpp.ctrl(0)
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID, lane))
      val pc = wrap(c(Global.PC, lane))
      val fetchId = wrap(c(Fetch.ID, lane))
      val decodeId = wrap(c(Decode.ID, lane))
    }

    val serializeds = for(lane <- 0 until Decode.LANES) yield new Area {
      val c = dpp.ctrl(host[DecoderPlugin].decodeAt)
      host[DecoderPlugin].logic.await()
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID, lane))
      val decodeId = wrap(c(Decode.ID, lane))
      val microOpId = wrap(c(Decode.MICRO_OP_ID, lane))
      val microOp = wrap(c(Decode.MICRO_OP, lane))
    }

    val dispatches = for (eu <- host.list[ExecuteUnitService]) yield new Area {
      val c = eu.nodeAt(0)
      val fire = wrap(c.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val microOpId = wrap(c(Decode.MICRO_OP_ID))
    }


    val executes = for (eu <- host.list[ExecuteUnitService]) yield new Area {
      val c = eu.nodeAt(eu.executeAt - 1)
      val fire = wrap(c.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val microOpId = wrap(c(Decode.MICRO_OP_ID))
    }

    val rfWrites = new Area{
      val ints = host.find[RegfileService](_.rfSpec == IntRegFile).getWrites().map(b => wrap(b.asWithoutReady()))
    }

    val completions = new Area{
      val ports = host.list[CompletionService].flatMap(cp => cp.getCompletions().map(wrap))
    }
  }
}
