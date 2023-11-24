package vexiiriscv.misc

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.{Decode, DecodePipelinePlugin, DecoderPlugin}
import vexiiriscv.execute.{CompletionService, ExecuteLaneService, WriteBackPlugin}
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.regfile.{RegFileWriterService, RegfileService}
import vexiiriscv.riscv.IntRegFile

import scala.collection.mutable.ArrayBuffer

class WhiteboxerPlugin extends FiberPlugin{
  buildBefore(host[PipelineBuilderPlugin].elaborationLock)

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


    val decodes =  for(lane <- 0 until Decode.LANES) yield new Area{
      val c = dpp.ctrl(0).lane(lane)
      val fire = wrap(c.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val pc = wrap(c(Global.PC))
      val fetchId = wrap(c(Fetch.ID))
      val decodeId = wrap(c(Decode.DOP_ID))
    }

    val serializeds = for(lane <- 0 until Decode.LANES) yield new Area {
      val c = dpp.ctrl(host[DecoderPlugin].decodeAt).lane(lane)
      host[DecoderPlugin].logic.await()
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val decodeId = wrap(c(Decode.DOP_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
      val microOp = wrap(c(Decode.UOP))
    }

    val dispatches = for (eu <- host.list[ExecuteLaneService]) yield new Area {
      val c = eu.ctrl(0)
      val fire = wrap(c.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
    }


    val executes = for (eu <- host.list[ExecuteLaneService]) yield new Area {
      val c = eu.ctrl(eu.executeAt - 1)
      val fire = wrap(c.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
    }

    val rfWrites = new Area{
      val ports = host.list[RegFileWriterService].flatMap(_.getRegFileWriters()).map(wrap)
    }

    val completions = new Area{
      val ports = host.list[CompletionService].flatMap(cp => cp.getCompletions().map(wrap))
    }
  }
}
