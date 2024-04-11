package vexiiriscv.test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.Global.{HART_COUNT, TRAP}
import vexiiriscv.decode.{Decode, DecodePipelinePlugin, DecoderPlugin}
import vexiiriscv.execute._
import vexiiriscv.execute.lsu._
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.misc.{PipelineBuilderPlugin, PrivilegedPlugin, TrapPlugin}
import vexiiriscv.prediction.{BtbPlugin, LearnCmd, LearnPlugin}
import vexiiriscv.regfile.{RegFileWrite, RegFileWriter, RegFileWriterService}
import vexiiriscv.riscv.{Const, Riscv}
import vexiiriscv.schedule.{DispatchPlugin, FlushCmd, ReschedulePlugin}

import scala.collection.mutable.ArrayBuffer

class WhiteboxerPlugin extends FiberPlugin{

  val logic = during setup new Logic()
  class Logic extends Area{
    val pbp = host[PipelineBuilderPlugin]
    val buildBefore = retains(pbp.elaborationLock)
    awaitBuild()

    def wrap[T <: Data](that: T): T = CombInit(that).simPublic

    val fpp = host[FetchPipelinePlugin]
    val dpp = host[DecodePipelinePlugin]
    val fetch = new Area {
      val c = fpp.fetch(0)
      val fire = wrap(c.down.isFiring)
      val hartId = wrap(c(Global.HART_ID))
      val fetchId = wrap(c(Fetch.ID))
    }


    val decodes = for (laneId <- 0 until Decode.LANES) yield new Area {
      val c = dpp.ctrl(0).lane(laneId)
      val fire = wrap(c.up.isFiring)
      val spawn = wrap(c.up.transactionSpawn)
      val hartId = wrap(c(Global.HART_ID))
      val pc = wrap(Global.expendPc(c(Global.PC), 64).asSInt)
      val fetchId = wrap(c(Fetch.ID))
      val decodeId = wrap(c(Decode.DOP_ID))
    }

    val serializeds = for (laneId <- 0 until Decode.LANES) yield new Area {
      val decodeAt = host[DecoderPlugin].decodeAt
      val c = dpp.ctrl(decodeAt).lane(laneId)

      host[DecoderPlugin].logic.await()
      val fire = wrap(c.up.transactionSpawn)
      val hartId = wrap(c(Global.HART_ID))
      val decodeId = wrap(c(Decode.DOP_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
      val microOp = wrap(c(Decode.UOP))
    }

    val dispatches = for (eu <- host.list[ExecuteLaneService]) yield new Area {
      val c = eu.ctrl(0)
      val fire = wrap(c.down.transactionSpawn)
      val hartId = wrap(c(Global.HART_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
    }


    val executes = for (eu <- host.list[ExecuteLaneService]) yield new Area {
      val c = eu.ctrl(eu.executeAt)
      val fire = wrap(c.down.transactionSpawn && c.down(Global.COMMIT))
      val hartId = wrap(c(Global.HART_ID))
      val microOpId = wrap(c(Decode.UOP_ID))
    }

    val withCsr = host.get[CsrAccessPlugin].nonEmpty
    val csr = withCsr.option(new Area {
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
    })

    val rfWrites = new Area {
      val ports = host.list[RegFileWriterService].flatMap(_.getRegFileWriters()).map(wrap)
    }

    val completions = new Area {
      val ports = host.list[CompletionService].flatMap(cp => cp.getCompletions().map(wrap))
    }

    val reschedules = new Area {
      val rp = host[ReschedulePlugin]
      rp.elaborationLock.await()
      val flushes = rp.flushPorts.map(wrap)
    }

    val prediction = new Area{
      val lp = host[LearnPlugin]
      val learns = lp.logic.ups.map(e => wrap(e.asFlow))
    }

//    val btb = host.get[BtbPlugin]
//    val btbHit = btb.foreach(btb => new Area {
//      val fire    = wrap(btb.logic.applyIt.down.isFiring)
//      val fetchId = wrap(btb.logic.applyIt(Fetch.ID))
//    })

    val loadExecute = new Area {
      val fire = Bool()
      val hartId = Global.HART_ID()
      val uopId = Decode.UOP_ID()
      val size = UInt(2 bits)
      val address = Global.PHYSICAL_ADDRESS()
      val data = Bits(Riscv.LSLEN bits)

      SimPublic(fire, hartId, uopId, size, address, data)

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        val c = p.logic.wbCtrl
        fire := c.down.isFiring && c(AguPlugin.SEL) && c(AguPlugin.LOAD) && !c(TRAP) && !c(p.logic.onPma.RSP).io
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
        size := c(AguPlugin.SIZE).resized
        address := c(p.logic.tpk.TRANSLATED)
        data := host.find[IntFormatPlugin](_.laneName == p.layer.laneName).logic.stages.find(_.ctrlLink == c.ctrlLink).get.wb.payload
      })


      val lp = host.get[LsuPlugin] map (p => new Area {
        val c = p.logic.onWb
        fire := c.down.isFiring && c(AguPlugin.SEL) && c(AguPlugin.LOAD) && !c(TRAP) && !c(p.logic.onCtrl.IO)
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
        size := c(AguPlugin.SIZE).resized
        address := c(LsuL1.PHYSICAL_ADDRESS)
        data := host.find[IntFormatPlugin](_.laneName == p.layer.laneName).logic.stages.find(_.ctrlLink == c.ctrlLink).get.wb.payload
      })
    }

    val storeCommit = new Area {
      val fire = Bool()
      val hartId = Global.HART_ID()
      val uopId = Decode.UOP_ID()
      val storeId = Decode.STORE_ID()
      val size = UInt(2 bits)
      val address = Global.PHYSICAL_ADDRESS()
      val data = Bits(Riscv.LSLEN bits)
      SimPublic(fire, hartId, uopId, storeId, size, address, data)

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        val c = p.logic.forkCtrl
        val bus = p.logic.bus
        fire := bus.cmd.fire && bus.cmd.write && !bus.cmd.io
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
        size := bus.cmd.size.resized
        address := bus.cmd.address
        data := bus.cmd.data
        storeId := c(Decode.UOP_ID).resized
      })

      val lp = host.get[LsuPlugin] map (p => new Area {
        val c = p.logic.onWb
        fire := c.storeFire
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
        size := c(AguPlugin.SIZE)
        address := c(p.logic.tpk.TRANSLATED)
        data := c(LsuL1.WRITE_DATA)
        storeId := c(Decode.STORE_ID)
      })
    }

    val storeConditional = new Area {
      val fire = Bool()
      val hartId = Global.HART_ID()
      val uopId = Decode.UOP_ID()
      val miss = Bool()

      SimPublic(fire, hartId, uopId, miss)

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        val c = p.logic.wbCtrl
        fire := c.down.isFiring && c(AguPlugin.SEL) && (c(AguPlugin.ATOMIC) && !c(AguPlugin.LOAD)) && !c(TRAP)
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
        miss := c(p.logic.onJoin.SC_MISS)
      })
      val lp = host.get[LsuPlugin] map (p => new Area {
        val c = p.logic.onWb
        fire := c.down.isFiring && c(AguPlugin.SEL) && (c(AguPlugin.ATOMIC) && !c(AguPlugin.LOAD)) && !c(TRAP)
        hartId := c(Global.HART_ID)
        uopId := c(Decode.UOP_ID)
        miss := c(p.logic.onCtrl.SC_MISS)
      })
    }

    val storeBroadcast = new Area {
      val fire = Bool()
      val hartId = Global.HART_ID()
      val storeId = Decode.STORE_ID()
      SimPublic(fire, hartId, storeId)

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        fire    := storeCommit.fire
        hartId  := storeCommit.hartId
        storeId := storeCommit.storeId
      })
      val lp = host.get[LsuPlugin] map (p => new Area {
        val c = p.logic.onWb
        fire := c.storeBroadcast
        hartId := c(Global.HART_ID)
        storeId := c(Decode.STORE_ID)
      })
    }

    val wfi = wrap(host[TrapPlugin].logic.harts.map(_.trap.fsm.wfi).asBits)

    val perf = new Area{
      val dispatch = host[DispatchPlugin]
      val executeFreezed = wrap(host[ExecutePipelinePlugin].isFreezed())
      val dispatchHazards = wrap(dispatch.logic.candidates.map(c => c.ctx.valid && !c.fire).orR)
      val candidatesCount = wrap(CountOne(dispatch.logic.candidates.map(_.ctx.valid)))
      val dispatchFeedCount = CountOne(dispatch.logic.feeds.map(_.isValid))

      val executeFreezedCounter = wrap(Counter(1l << 60l, executeFreezed).value)
      val dispatchHazardsCounter = wrap(Counter(1l << 60l, dispatchHazards).value)
      val candidatesCountCounters = (0 to dispatch.logic.candidates.size).map(id => wrap(Counter(1l << 60l, candidatesCount === id).value))
      val dispatchFeedCounters = (0 to dispatch.logic.feeds.size).map(id => wrap(Counter(1l << 60l, dispatchFeedCount === id).value))
    }

    val trap = new Area {
      val ports = for(hartId <- 0 until HART_COUNT) yield new Area{
        val priv = host[TrapPlugin].logic.harts(hartId).trap
        val valid = wrap(priv.whitebox.trap)
        val interrupt = wrap(priv.whitebox.interrupt)
        val cause = wrap(priv.whitebox.code)
      }
    }

    def self = this
    abstract class Proxies {
      val fetch = new FetchProxy()
      val decodes = self.decodes.indices.map(new DecodeProxy(_)).toArray
      val serializeds = self.serializeds.indices.map(new SerializedProxy(_)).toArray
      val dispatches = self.dispatches.indices.map(new DispatchProxy(_)).toArray
      val executes = self.executes.indices.map(new ExecuteProxy(_)).toArray
      val csr = self.csr.map(_ => new CsrProxy())
      val rfWrites = self.rfWrites.ports.map(new RfWriteProxy(_)).toArray
      val completions = self.completions.ports.map(new CompletionProxy(_)).toArray
      val flushes = self.reschedules.flushes.map(new FlushProxy(_)).toArray
      val loadExecute = new LoadExecuteProxy()
      val storeCommit = new StoreCommitProxy()
      val storeConditional = new StoreConditionalProxy()
      val storeBroadcast = new StoreBroadcastProxy()
      val learns = self.prediction.learns.map(learn => new LearnProxy(learn)).toArray
      val perf = new PerfProxy()
      val trap = self.trap.ports.indices.map(new TrapProxy(_)).toArray
      val interrupts = new InterruptsProxy()
      val wfi = self.wfi.simProxy()

      def interrupt(hartId : Int, intId : Int, value : Boolean)

      class InterruptChecker(hartId : Int, pin: Bool, id: Int) {
        val proxy = pin.simProxy()
        var last = proxy.toBoolean

        def sync(): Unit = {
          interrupt(hartId, id, last)
        }

        def check(): Unit = {
          val value = proxy.toBoolean
          if (value != last) {
            interrupt(hartId, id, value)
            last = value
          }
        }
      }

      class InterruptsProxy {
        val priv = host[PrivilegedPlugin]
        val checkers = ArrayBuffer[InterruptChecker]()
        for ((hart, hartId) <- priv.logic.harts.zipWithIndex) {
          checkers += new InterruptChecker(hartId, hart.int.m.timer,  7)
          checkers += new InterruptChecker(hartId, hart.int.m.software,  3)
          checkers += new InterruptChecker(hartId, hart.int.m.external, 11)
          if (priv.p.withSupervisor) {
            checkers += new InterruptChecker(hartId, hart.int.s.external, 9)
          }
        }
        def check(): Unit = {
          checkers.foreach(_.check())
        }

        def sync(): Unit = {
          checkers.foreach(_.sync())
        }
      }
    }


    class FetchProxy {
      val fire = fetch.fire.simProxy()
      val hartd = fetch.hartId.simProxy()
      val id = fetch.fetchId.simProxy()
    }

    class DecodeProxy(laneId: Int) {
      val self = decodes(laneId)
      val spawn = self.spawn.simProxy()
      val fire = self.fire.simProxy()
      val hartId = self.hartId.simProxy()
      val pc = self.pc.simProxy()
      val fetchId = self.fetchId.simProxy()
      val decodeId = self.decodeId.simProxy()
    }

    class SerializedProxy(laneId: Int) {
      val self = serializeds(laneId)
      val fire = self.fire.simProxy()
      val hartId = self.hartId.simProxy()
      val decodeId = self.decodeId.simProxy()
      val microOpId = self.microOpId.simProxy()
      val microOp = self.microOp.simProxy()
    }

    class DispatchProxy(laneId: Int) {
      val self = dispatches(laneId)
      val fire = self.fire.simProxy()
      val hartId = self.hartId.simProxy()
      val microOpId = self.microOpId.simProxy()
    }

    class ExecuteProxy(laneId: Int) {
      val self = executes(laneId)
      val fire = self.fire.simProxy()
      val hartId = self.hartId.simProxy()
      val microOpId = self.microOpId.simProxy()
    }


    class CsrProxy{
      val csr = self.csr.get
      val valid = csr.port.valid.simProxy()
      val hartId = csr.port.hartId.simProxy()
      val uopId = csr.port.uopId.simProxy()
      val address = csr.port.address.simProxy()
      val write = csr.port.write.simProxy()
      val read = csr.port.read.simProxy()
      val writeDone = csr.port.writeDone.simProxy()
      val readDone = csr.port.readDone.simProxy()
    }

    class RfWriteProxy(val port : Flow[RegFileWriter]) {
      val valid = port.valid.simProxy()
      val data = port.data.simProxy()
      val hartId = port.hartId.simProxy()
      val uopId = port.uopId.simProxy()
    }

    class CompletionProxy(port: Flow[CompletionPayload]) {
      val valid = port.valid.simProxy()
      val hartId = port.hartId.simProxy()
      val uopId = port.uopId.simProxy()
      val trap = port.trap.simProxy()
      val commit = port.commit.simProxy()
    }

    class FlushProxy(port: Flow[FlushCmd]) {
      val withUopId = port.withUopId
      val valid = port.valid.simProxy()
      val hartId = port.hartId.simProxy()
      val uopId = port.withUopId generate port.uopId.simProxy()
      val laneAge = port.laneAge.simProxy()
      val self = port.self.simProxy()
    }

    class LoadExecuteProxy {
      val fire = loadExecute.fire.simProxy()
      val hartId = loadExecute.hartId.simProxy()
      val uopId = loadExecute.uopId.simProxy()
      val size = loadExecute.size.simProxy()
      val address = loadExecute.address.simProxy()
      val data = loadExecute.data.simProxy()
    }

    class StoreCommitProxy {
      val fire = storeCommit.fire.simProxy()
      val hartId = storeCommit.hartId.simProxy()
      val uopId = storeCommit.uopId.simProxy()
      val storeId = storeCommit.storeId.simProxy()
      val size = storeCommit.size.simProxy()
      val address = storeCommit.address.simProxy()
      val data = storeCommit.data.simProxy()
    }

    class StoreConditionalProxy {
      val fire = storeConditional.fire.simProxy()
      val hartId = storeConditional.hartId.simProxy()
      val uopId = storeConditional.uopId.simProxy()
      val miss = storeConditional.miss.simProxy()
    }

    class StoreBroadcastProxy {
      val fire = storeBroadcast.fire.simProxy()
      val hartId = storeBroadcast.hartId.simProxy()
      val storeId = storeBroadcast.storeId.simProxy()
    }

    class LearnProxy(port: Flow[LearnCmd]) {
      val valid = port.valid.simProxy()
      val pcOnLastSlice = port.pcOnLastSlice.simProxy()
      val pcTarget = port.pcTarget.simProxy()
      val taken = port.taken.simProxy()
      val isBranch = port.isBranch.simProxy()
      val wasWrong = port.wasWrong.simProxy()
      val history = port.history.simProxy()
      val uopId = port.uopId.simProxy()
      val hartId = port.hartId.simProxy()
    }

    class PerfProxy() {
      val candidatesMax = perf.dispatch.logic.candidates.size
      val executeFreezed = perf.executeFreezed.simProxy()
      val dispatchHazards = perf.dispatchHazards.simProxy()
      val candidatesCount = perf.candidatesCount.simProxy()
    }

    class TrapProxy(val hartId : Int) {
      val self = trap.ports(hartId)
      val fire = self.valid.simProxy()
      val interrupt = self.interrupt.simProxy()
      val cause = self.cause.simProxy()
    }

    buildBefore.release()
  }
}
