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

  case class JumpSpec(bus : Flow[JumpCmd], priority : Int, aggregationPriority : Int) extends Composite(bus) {
    val laneValid = Bool()
  }
  val jumps = ArrayBuffer[JumpSpec]()
  override def newJumpInterface(age: Int, laneAgeWidth : Int, aggregationPriority : Int): Flow[JumpCmd] = {
    jumps.addRet(JumpSpec(Flow(JumpCmd(laneAgeWidth)), age, aggregationPriority)).bus
  }


  override def simSetPc(value: Long): Unit = {
    logic.harts.foreach { h =>
      val pc = (value & h.self.state.maxValue)
      h.self.state #= pc
      h.self.fault #= false
      h.output.pc #= pc
      h.output.fault #= false
    }
  }


  override def forcedSpawn(): Bool = logic.forcedSpawn

  val logic = during setup new Area{
    val pp = host[FetchPipelinePlugin]
    val buildBefore = retains(pp.elaborationLock)
    awaitBuild()

    elaborationLock.await()
    val injectStage = pp.fetch(0).up

    assert(Global.HART_COUNT.get == 1)
    val forcedSpawn = jumps.map(_.bus.valid).orR

    val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
      // Self is a jump interface which store the hart PC
      val self = new Area {
        val id = Reg(Fetch.ID) init(0)
        val flow = newJumpInterface(-1, laneAgeWidth = 0, aggregationPriority = 0)
        val increment = RegInit(False)
        val fault = RegInit(False) simPublic()
        val state = Reg(PC) init (resetVector) simPublic()
        val pc = state + U(WORD_BYTES).andMask(increment)
        flow.valid := True
        flow.fault := fault
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
        var fault = Bool()
        for (group <- grouped) {
          val indexes = group.map(e => sortedByPriority.indexOf(e))
          val goh = indexes.map(i => oh(i))
          val muxPc = OhMux.or(goh, group.map(_.bus.pc))
          val muxFault = OhMux.or(goh, group.map(_.bus.fault))
          if (group == grouped.head) {target := muxPc; fault := muxFault} else when(goh.orR) {
            KeepAttribute(target)
            target \= muxPc
            fault \= muxFault
          }
        }
      }

      val holdComb = holdPorts.filter(_.hartId == hartId).map(_.valid).orR
      val holdReg = RegNext(holdComb) init(True)

      // Stream of PC for the given hart
      val output = Stream(new Bundle{
        val pc = PC()
        val fault = Bool()
      }).simPublic()

      output.valid := !holdReg
      output.fault := aggregator.fault
      output.pc := aggregator.target
      output.pc(SLICE_RANGE_LOW - 1 downto 0) := 0

      // Update the hart PC state
      self.state := output.pc
      self.fault := output.fault
      self.increment := False
      when(output.fire) {
        self.increment := True
        self.state(Fetch.SLICE_RANGE) := 0
      }
    }

    assert(HART_COUNT.get == 1)

    val inject = new injectStage.Area {
      valid := harts(0).output.valid
      harts(0).output.ready := ready
      Fetch.WORD_PC := harts(0).output.pc
      Fetch.PC_FAULT := harts(0).output.fault
      HART_ID := 0

      Fetch.ID.assignDontCare()
      harts.onSel(HART_ID) { hart =>
        when(isFiring) {
          hart.self.id := hart.self.id + 1
        }
        Fetch.ID := hart.self.id
      }
    }

    //harts.output (the pc stream) use holdPorts with one delay cycle (to improve timings), so here we need to prevent fetch(0) to fire down if the combinatorial check of holdPorts hits
    val holdHalter = new pp.Fetch(0) {
      val doIt = harts.reader(HART_ID)(_.holdComb)
      haltWhen(doIt)
    }

    buildBefore.release()
  }
}
