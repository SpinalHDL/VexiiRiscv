package vexiiriscv.schedule

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{CtrlApi, SignalKey}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.{Decode, DecodePipelinePlugin, DecoderService}
import vexiiriscv.execute.ExecuteUnitPlugin

/*
How to check if a instruction can schedule :
- If one of the pipeline which implement its micro op is free
- There is no inflight non-bypassed RF write to one of the source operand
- There is no scheduling fence
- For credit based execution, check there is enough credit

Schedule euristic :
- In priority order, go through the slots
- Check which pipeline could schedule it (free && compatible)
- Select the pipeline which the highest priority (to avoid using the one which can do load and store, for instance)
- If the slot can't be schedule, disable all following ones with same HART_ID
*/

class DispatchPlugin(dispatchAt : Int = 3) extends FiberPlugin{
  lazy val dpp = host[DecodePipelinePlugin]
  lazy val dp = host[DecoderService]
  addLockable(dpp)
  addRetain(dp)

  val logic = during build new Area{
    val dispatchCtrl = dpp.ctrl(dispatchAt)

    val eus = host.list[ExecuteUnitPlugin].sortBy(_.priority).reverse
    val EU_COMPATIBILITY = eus.map(eu => eu -> SignalKey(Bool())).toMapLinked()
    for(eu <- eus){
      val key = EU_COMPATIBILITY(eu)
      dp.addMicroOpDecodingDefault(key, False)
      for(op <- eu.getMicroOp()){
        dp.addMicroOpDecoding(op, key, True)
      }
    }
    dp.release()
    val slotsCount = 0

//    val slots = for(i <- 0 until slotsCount) yield new Area{
//      val valid = Bool()
//      val compatibility = eus.map(_ => True) //TODO
////      val
//    }

    case class MicroOpCtx() extends Bundle{
      val valid = Bool()
      val hazard = Bool()
      val compatibility = Bits(eus.size bits)
      val hartId = Global.HART_ID()
    }

    val candidates = List.fill(slotsCount + Decode.LANES)(MicroOpCtx())
    for(lane <- 0 until Decode.LANES) new dispatchCtrl.Area(lane){
      val c = candidates(slotsCount + lane)
      c.valid := dispatchCtrl.isValid && Dispatch.MASK
      c.compatibility := EU_COMPATIBILITY.values.map(this(_)).asBits()
      c.hartId := Global.HART_ID
    }

    for((c, cId) <- candidates.zipWithIndex){
      c.hazard := False
    }

    val scheduler = new Area {
      val eusFree, hartFree = Array.fill(candidates.size + 1)(Bits(eus.size bits))
      eusFree(0).setAll()
      hartFree(0).setAll()
      val layer = for ((c, id) <- candidates.zipWithIndex) yield new Area {
        val eusHits = c.compatibility & eusFree(id)
        val eusOh = OHMasking.firstV2(eusHits)
        val doIt = c.valid && eusHits.orR && hartFree(id)(c.hartId)
        eusFree(id + 1) := eusFree(id) & eusOh.orMask(!doIt)
        hartFree(id + 1) := hartFree(id) & (~UIntToOh(c.hartId)).orMask(doIt)
      }
    }

    val dispatcher = for ((eu, id) <- eus.zipWithIndex; ctrl = eu.ctrl(0)) yield new Area with CtrlApi {
      override def getCtrl = ctrl
      val oh = B(scheduler.layer.map(l => l.doIt && l.eusOh(id)))
      val mux = candidates.reader(oh, true)
      ctrl.up.valid := oh.orR
      Global.HART_ID := mux(_.hartId)
      val ressources = eu.getMicroOp().flatMap(_.resources).distinctLinked.toArray
      println(ressources.mkString(" "))
    }

    dispatchCtrl.down.ready := True //TODO remove
  }
}