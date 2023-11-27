package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin._
import vexiiriscv.Global._
import Fetch._
import spinal.core.sim.{SimDataPimper, _}
import vexiiriscv.Global

import scala.collection.mutable.ArrayBuffer

class PcPlugin(var resetVector : BigInt = 0x80000000l) extends FiberPlugin with PcService{
  lazy val pp = host[FetchPipelinePlugin]
  setupRetain(pp.elaborationLock)

  case class JumpSpec(bus : Flow[JumpCmd], priority : Int, aggregationPriority : Int) extends Composite(bus) {
    val laneValid = Bool()
  }
  val jumps = ArrayBuffer[JumpSpec]()
  override def createJumpInterface(age: Int, laneAgeWidth : Int, aggregationPriority : Int): Flow[JumpCmd] = {
    jumps.addRet(JumpSpec(Flow(JumpCmd(laneAgeWidth)), age, aggregationPriority)).bus
  }


  override def simSetPc(value: Long): Unit = {
    logic.harts.foreach { h =>
      val pc = (value & h.self.state.maxValue)
      h.self.state #= pc
      h.output.payload #= pc
    }
  }


  override def forcedSpawn(): Bool = logic.forcedSpawn

  val logic = during build new Area{
    elaborationLock.await()
    val injectStage = pp.ctrl(0).up

    // Used to wait until everybody is ready after reset
    val init = new Area {
      val requests = host.list[InitService]
      val booted = RegNext(True) init(False)
      booted clearWhen(requests.map(_.initHold()).orR)
    }


    assert(Global.HART_COUNT.get == 1)
    val forcedSpawn = jumps.map(_.bus.valid).orR

    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      // Self is a jump interface which store the hart PC
      val self = new Area {
        val id = Reg(Fetch.ID) init(0)
        val flow = createJumpInterface(-1, laneAgeWidth = 0, aggregationPriority = 0)
        val increment = RegInit(False)
        val state = Reg(PC) init (resetVector) simPublic()
        val pc = state + U(WORD_BYTES).andMask(increment)
        flow.valid := True
        flow.pc := pc
        flow.hartId := hartId
      }

      // Takes all the pc sources and aggregate them into a single value, based on priority
      val aggregator = new Area {
        for(spec <- jumps){
          val others = jumps.filter(o => o != spec && o.priority == spec.priority)
          spec.laneValid := others.map(o => !o.bus.valid || o.bus.laneAge > spec.bus.laneAge).andR
        }

        val sortedByPriority = jumps.sortWith(_.priority > _.priority)
        val valids = sortedByPriority.map(e => e.bus.valid && e.bus.hartId === hartId && e.laneValid)
        val cmds = sortedByPriority.map(_.bus.payload)
        val oh = OHMasking.firstV2(valids.asBits)

        val grouped = sortedByPriority.groupByLinked(_.aggregationPriority).toList.sortBy(_._1).map(_._2)
        var target = PC()
        for (group <- grouped) {
          val indexes = group.map(e => sortedByPriority.indexOf(e))
          val goh = indexes.map(i => oh(i))
          val mux = OhMux.or(goh, group.map(_.bus.pc))
          if (group == grouped.head) target := mux else when(goh.orR) {
            KeepAttribute(target)
            target \= mux
          }
        }
      }

      // Stream of PC for the given hart
      val output = Stream(PC).simPublic()
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

    assert(HART_COUNT.get == 1)

    val inject = new injectStage.Area {
      valid := True
      harts(0).output.ready := ready
      Fetch.WORD_PC := harts(0).output.payload
      HART_ID := 0

      when(!init.booted){
        valid := False
        harts.foreach(_.output.ready := False)
      }

      Fetch.ID.assignDontCare()
      harts.onSel(HART_ID) { hart =>
        when(isFiring) {
          hart.self.id := hart.self.id + 1
        }
        Fetch.ID := hart.self.id
      }
    }
    pp.elaborationLock.release()
  }
}
