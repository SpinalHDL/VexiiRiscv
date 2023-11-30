package vexiiriscv.test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.{Decode, DecodePipelinePlugin, DecoderPlugin}
import vexiiriscv.execute._
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.misc.PipelineBuilderPlugin
import vexiiriscv.regfile.RegFileWriterService
import vexiiriscv.riscv.{Const, Riscv}
import vexiiriscv.schedule.ReschedulePlugin

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


    val decodes =  for(laneId <- 0 until Decode.LANES) yield new Area{
      val c = dpp.ctrl(0).lane(laneId)
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val pc = wrap(c(Global.PC))
      val fetchId = wrap(c(Fetch.ID))
      val decodeId = wrap(c(Decode.DOP_ID))
    }

    val serializeds = for(laneId <- 0 until Decode.LANES) yield new Area {
      val c = dpp.ctrl(host[DecoderPlugin].decodeAt).lane(laneId)
      host[DecoderPlugin].logic.await()
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val decodeId = wrap(c(Decode.DOP_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
      val microOp = wrap(c(Decode.UOP))
    }

    val dispatches = for (eu <- host.list[ExecuteLaneService]) yield new Area {
      val c = eu.ctrl(0)
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
    }


    val executes = for (eu <- host.list[ExecuteLaneService]) yield new Area {
      val c = eu.ctrl(eu.executeAt - 1)
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
    }

    val csr = new Area{
      val p = host[CsrAccessPlugin].logic
      val port = Verilator.public(Flow(new Bundle {
        val hartId = Global.HART_ID()
        val uopId = Decode.UOP_ID()
        val address = UInt(12 bits)
        val write = Bits(Riscv.XLEN bits)
        val read = Bits(Riscv.XLEN bits)
        val writeDone = Bool()
        val readDone = Bool()
      }))
      port.valid := p.fsm.regs.fire
      port.uopId := p.fsm.regs.uopId
      port.hartId := p.fsm.regs.hartId
      port.address := U(p.fsm.regs.uop)(Const.csrRange)
      port.write := p.fsm.regs.onWriteBits
      port.read := p.fsm.regs.csrValue
      port.writeDone := p.fsm.regs.write
      port.readDone := p.fsm.regs.read
    }

    val rfWrites = new Area{
      val ports = host.list[RegFileWriterService].flatMap(_.getRegFileWriters()).map(wrap)
    }

    val completions = new Area{
      val ports = host.list[CompletionService].flatMap(cp => cp.getCompletions().map(wrap))
    }

    val reschedules = new Area{
      val rp = host[ReschedulePlugin]
      rp.elaborationLock.await()
      val flushes = rp.flushPorts.map(wrap)
    }

    val loadExecute = new Area{
      val fire = Bool()
      val hartId = Global.HART_ID()
      val uopId = Decode.UOP_ID()
      val size = UInt(2 bits)
      val address = Global.PHYSICAL_ADDRESS()
      val data = Bits(Riscv.LSLEN bits)

      SimPublic(fire, hartId, uopId, size, address, data)

      val lcp = host.get[LsuCachelessPlugin] map (p =>new Area{
        val c = p.logic.wbCtrl
        fire := c.down.isFiring && c(AguPlugin.SEL) && c(AguPlugin.LOAD) && !c(p.logic.onAddress.translationPort.keys.IO)
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
        size := c(AguPlugin.SIZE).resized
        address := c(SrcStageables.ADD_SUB).asUInt
        data := host.find[IntFormatPlugin](_.laneName == p.laneName).logic.stages.find(_.stage == c).get.wb.payload
      })
    }

    val storeCommit = new Area {
      val fire = Bool()
      val hartId = Global.HART_ID()
      val uopId = Decode.UOP_ID()
      val size = UInt(2 bits)
      val address = Global.PHYSICAL_ADDRESS()
      val data = Bits(Riscv.LSLEN bits)
      SimPublic(fire, hartId, uopId, size, address, data)

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        val c = p.logic.forkCtrl
        val bus = p.logic.bus
        fire := bus.cmd.fire && bus.cmd.write && !bus.cmd.io
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
        size := bus.cmd.size.resized
        address := bus.cmd.address
        data := bus.cmd.data
      })
    }

    val storeBroadcast = new Area {
      val fire = Bool()
      val hartId = Global.HART_ID()
      val uopId = Decode.UOP_ID()
      SimPublic(fire, hartId, uopId)

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        val c = p.logic.joinCtrl
        fire := c.down.isFiring && c(AguPlugin.SEL) && !c(AguPlugin.LOAD)
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
      })
    }
  }
}
