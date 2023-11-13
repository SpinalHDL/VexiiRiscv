package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin._
import vexiiriscv.Global._
import Fetch._

import scala.collection.mutable.ArrayBuffer

class PcPlugin(var resetVector : BigInt = 0x80000000l) extends FiberPlugin with PcService{
  lazy val pp = host[PipelinePlugin]
  during setup {
    pp.retain()
  }

  case class JumpSpec(bus : Flow[JumpCmd], priority : Int, aggregationPriority : Int)
  val jumps = ArrayBuffer[JumpSpec]()
  override def createJumpInterface(priority: Int, aggregationPriority : Int = 0): Flow[JumpCmd] = {
    jumps.addRet(JumpSpec(Flow(JumpCmd()), priority, aggregationPriority)).bus
  }

  val logic = during build new Area{
    val injectStage = pp.ctrl(0).up

    // Used to wait until everybody is ready after reset
    val init = new Area {
      val requests = host.list[InitService]
      val booted = RegNext(True) init(False)
      booted clearWhen(requests.map(_.initHold()).orR)
    }

    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      // Self is a jump interface which store the hart PC
      val self = new Area {
        val flow = createJumpInterface(-1)
        val increment = RegInit(False)
        val state = Reg(PC) init (resetVector)
        val pc = state + (U(increment) << WORD_BYTES)
        flow.valid := True
        flow.pc := pc
        flow.hartId := hartId
      }

      // Takes all the pc sources and aggregate them into a single value, based on priority
      val aggregator = new Area {
        val sortedByStage = jumps.sortWith(_.priority > _.priority)
        val valids = sortedByStage.map(e => e.bus.valid && e.bus.hartId === hartId)
        val cmds = sortedByStage.map(_.bus.payload)
        val oh = OHMasking.firstV2(valids.asBits)

        val grouped = sortedByStage.groupByLinked(_.aggregationPriority).toList.sortBy(_._1).map(_._2)
        var target = PC()
        for (group <- grouped) {
          val indexes = group.map(e => sortedByStage.indexOf(e))
          val goh = indexes.map(i => oh(i))
          val mux = OhMux.or(goh, group.map(_.bus.pc))
          if (group == grouped.head) target := mux else when(goh.orR) {
            KeepAttribute(target)
            target \= mux
          }
        }
      }

      // Stream of PC for the given hart
      val output = Stream(PC)
      output.valid := True
      output.payload := aggregator.target
      output.payload(SLICE_RANGE_LOW - 1 downto 0) := 0

      // Update the hart PC state
      when(output.valid) {
        self.state := output.payload
        self.increment := False
        when(output.ready) {
          self.increment := True
          self.state(Fetch.SLICE_RANGE) := 0
        }
      }
    }

    assert(HART_COUNT.get() == 1)

    val inject = new injectStage.Area {
      valid := True
      harts(0).output.ready := ready
      Fetch.WORD_PC := harts(0).output.payload
      HART_ID := 0

      when(!init.booted){
        valid := False
        harts.foreach(_.output.ready := False)
      }
    }
    pp.release()
  }
}
