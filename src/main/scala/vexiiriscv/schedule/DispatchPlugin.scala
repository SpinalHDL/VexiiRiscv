package vexiiriscv.schedule

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import spinal.lib.misc.pipeline.{CtrlApi, CtrlLaneApi, CtrlLink, NodeApi, Payload}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.Global.{HART_COUNT, TRAP}
import vexiiriscv.decode.{AccessKeys, Decode, DecodePipelinePlugin, DecoderService}
import vexiiriscv.execute.{Execute, ExecuteLanePlugin, ExecuteLaneService, ExecutePipelinePlugin, LaneLayer}
import vexiiriscv.misc.{CommitService, InflightService, PipelineBuilderPlugin, TrapService}
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{MicroOp, RD, RfRead, RfResource}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

class DispatchPlugin(var dispatchAt : Int,
                     var trapLayer : LaneLayer,
                     var withBuffer : Boolean) extends FiberPlugin{
  val elaborationLock = Retainer()

  val MAY_FLUSH = Payload(Bool())
  val DONT_FLUSH = Payload(Bool())
  val DONT_FLUSH_FROM_LANES = Payload(Bool())
  val FENCE_OLDER = Payload(Bool())


  val hmKeys = mutable.LinkedHashSet[Payload[_ <: Data]]()

//  val fenceYoungerOps = mutable.LinkedHashSet[MicroOp]()
  val fenceOlderOps = mutable.LinkedHashSet[MicroOp]()
//  def fenceYounger(op : MicroOp) = fenceYoungerOps += op
  def fenceOlder(op : MicroOp) = fenceOlderOps += op
  def haltDispatchWhen(cond : Bool) = api.haltDispatch.setWhen(cond)

  val api = during build new Area{
    val haltDispatch = False
  }

  val logic = during setup new Area{
    val dpp = host[DecodePipelinePlugin]
    val dp = host[DecoderService]
    val eupp = host[ExecutePipelinePlugin]
    val pbp = host[PipelineBuilderPlugin]
    val ts = host[TrapService]
    val buildBefore = retains(
      List(pbp.elaborationLock, dpp.elaborationLock, eupp.pipelineLock) ++ host.list[ExecuteLaneService].map(_.pipelineLock)
    )
    val dpRetains = retains(dp.elaborationLock)
    val tsRetains = retains(ts.trapLock)
    awaitBuild()

    Execute.LANE_AGE_WIDTH.set(log2Up(Decode.LANES))
    val trapPendings = ts.newTrapPending(); tsRetains.release()

    elaborationLock.await()
    val dispatchCtrl = dpp.ctrl(dispatchAt)

    val eus = host.list[ExecuteLaneService]
    val lanesLayers = host.list[ExecuteLanePlugin].flatMap(_.getLayers()).sortBy(_.priority).reverse
    val LANES_LAYER_HIT = lanesLayers.map(ll => ll -> Payload(Bool())).toMapLinked()
    for(ll <- lanesLayers){
      val key = LANES_LAYER_HIT(ll)
      dp.addMicroOpDecodingDefault(key, False)
      for(op <- ll.uops.values){
        dp.addMicroOpDecoding(op.uop, key, True)
      }
    }


    class ReservationSpec(what: Any) extends Area {
      val sels = mutable.LinkedHashMap[LaneLayer,mutable.LinkedHashMap[Int, Payload[Bool]]]()
    }
    val reservationSpecs = mutable.LinkedHashMap[Any, ReservationSpec]()


    hmKeys.add(FENCE_OLDER)
    hmKeys.add(MAY_FLUSH)
    hmKeys.add(DONT_FLUSH)
    hmKeys.add(DONT_FLUSH_FROM_LANES)
    hmKeys.add(Decode.INSTRUCTION_SLICE_COUNT)
    dp.addMicroOpDecodingDefault(MAY_FLUSH, False)
    dp.addMicroOpDecodingDefault(DONT_FLUSH, False)
    dp.addMicroOpDecodingDefault(DONT_FLUSH_FROM_LANES, False)
    dp.addMicroOpDecodingDefault(FENCE_OLDER, False)
    val mayFlushUpToMax = eus.flatMap(_.getUopLayerSpec().map(_.mayFlushUpTo.getOrElse(-1))).max
    val mayFlushUops, dontFlushUops, dontFlushFromLanesUops = mutable.LinkedHashSet[MicroOp]()
    for (eu <- eus; spec <- eu.getUopLayerSpec()) {
      spec.mayFlushUpTo foreach( x => mayFlushUops += spec.uop )
      spec.dontFlushFrom foreach { x =>
        if (x <= mayFlushUpToMax) dontFlushFromLanesUops += spec.uop
        if (x < mayFlushUpToMax) dontFlushUops += spec.uop
      }
      for((what, ats) <- spec.reservations; at <- ats){
        val resSpec = reservationSpecs.getOrElseUpdate(what, new ReservationSpec(what))
        val onLayer = resSpec.sels.getOrElseUpdate(spec.elImpl, new mutable.LinkedHashMap[Int, Payload[Bool]]())
        val sel = onLayer.getOrElseUpdate(at, {
          val p = Payload(Bool()).setCompositeName(what, s"RESERVED_ON_${spec.elImpl.name}_AT_$at")
          dp.addMicroOpDecodingDefault(p, False)
          hmKeys.add(p)
          p
        })
        dp.addMicroOpDecoding(spec.uop, sel, True)
      }
    }

    for (uop <- mayFlushUops) dp.addMicroOpDecoding(uop, MAY_FLUSH, True)
    for (uop <- dontFlushUops) dp.addMicroOpDecoding(uop, DONT_FLUSH, True)
    for (uop <- dontFlushFromLanesUops) dp.addMicroOpDecoding(uop, DONT_FLUSH_FROM_LANES, True)
    for (uop <- fenceOlderOps) dp.addMicroOpDecoding(uop, FENCE_OLDER, True)

    // Generate upstream up dontFlush precise decoding
    case class DontFlushSpec(at: Int, value: Payload[Bool])
    val dontFlushSpecs = mutable.LinkedHashMap[Int, DontFlushSpec]()
    def getDontFlush(at: Int): Payload[Bool] = {
      dontFlushSpecs.getOrElseUpdate(at, DontFlushSpec(at, Payload(Bool()).setName(s"DONT_FLUSH_PRECISE_$at"))).value
    }
    val uopsGrouped = eus.flatMap(_.getUopLayerSpec()).groupByLinked(_.uop)
    val uopsDontFlushFrom = uopsGrouped.map(e => e._1 -> e._2.map(_.dontFlushFrom.getOrElse(100)).min)
    val dontFlushFromMin = uopsDontFlushFrom.map(_._2).min
    val eusFlushHazardUpTo = mutable.LinkedHashMap[ExecuteLaneService, Int]()
    for (elp <- eus) yield new Area {
      val flushUpTo = elp.getUopLayerSpec().flatMap(e => e.mayFlushUpTo).fold(-100)(_ max _)
      val hazardRange = dontFlushFromMin+1 to flushUpTo //+1 because the flush hazard on the same warp are handled somewere else, here we just need to care about past cycles
      eusFlushHazardUpTo(elp) = flushUpTo
      for (i <- hazardRange) hmKeys += getDontFlush(i)
    }
    val dontFlushDecoding = for (spec <- dontFlushSpecs.values) yield new Area {
      for ((uop, dontFlushFrom) <- uopsDontFlushFrom) {
        dp.addMicroOpDecoding(uop, spec.value, Bool(dontFlushFrom < spec.at)) //Answer the question : For this uop, when it reach the dontFlush point in the pipeline, is it not ok to have another op which emit flushes in the given spec.at stage
      }
    }

    dpRetains.release()

    hmKeys.add(Global.PC)
    hmKeys.add(Global.TRAP)
    hmKeys.add(Decode.UOP_ID)
    for ((k, ac) <- Decode.rfaKeys) {
      hmKeys.add(ac.ENABLE)
      hmKeys.add(ac.RFID)
      hmKeys.add(ac.PHYS)
    }

    case class MicroOpCtx() extends Bundle{
      val valid = Bool()
      val laneLayerHits = Bits(lanesLayers.size bits)
      val hartId = Global.HART_ID()
      val uop = Decode.UOP()
      val hm = new HardMap()
      hmKeys.foreach(e => hm.add(e))
    }

    val rdKeys = Decode.rfaKeys.get(RD)
    assert(Global.HART_COUNT.get == 1, "need to implement write to write RD hazard for stuff which can be schedule in same cycle")
//    assert(rdKeys.rfMapping.size == 1, "Need to check RFID usage and missing usage kinda everywhere, also the BYPASS signal should be set high for all stages after the writeback for the given RF")

    val slotsCount = if(withBuffer) Decode.LANES-1 else 0
    val slots = for(slotId <- 0 until slotsCount) yield new Area {
      val ctx = Reg(MicroOpCtx())
      ctx.valid init(False)
    }
    for(hartId <- 0 until HART_COUNT.get){
      trapPendings(hartId) := slots.map(s => s.ctx.valid && s.ctx.hm(TRAP)).orR
    }

    val candidates = for(cId <- 0 until slotsCount + Decode.LANES) yield new Area{
      val ctx = MicroOpCtx()
      val fire = Bool()
      val cancel = Bool()

      val rsHazards = Bits(lanesLayers.size bits)
      val reservationHazards = Bits(lanesLayers.size bits)
      val flushHazards = Bool() //Pessimistic, could instead track the flush hazard per layer, but that's likely overkill, as instruction which may have flush hazard are likely single layer
      val fenceOlderHazards = Bool()
      val age = Execute.LANE_AGE()
      val moving = !ctx.valid || fire || cancel
    }

    for((c,i) <- candidates.zipWithIndex){
      c.age := CountOne(candidates.take(i).map(o => o.ctx.valid && o.ctx.hartId === c.ctx.hartId)).resize(Execute.LANE_AGE_WIDTH)
    }

    case class BypassedSpec(el: ExecuteLaneService, at: Int, value : Payload[Bool])
    val bypassedSpecs = mutable.LinkedHashMap[(ExecuteLaneService, Int), BypassedSpec]()
    def getBypassed(el: ExecuteLaneService, at: Int): Payload[Bool] = {
      bypassedSpecs.getOrElseUpdate(el -> at, BypassedSpec(el, at, Payload(Bool()).setName("BYPASSED_AT_" + at))).value
    }

    val rfaReads = Decode.rfaKeys.filter(_._1.isInstanceOf[RfRead])
    val rsHazardChecker = for(c <- candidates) yield new Area {
      val onLl = for((ll, llId) <- lanesLayers.zipWithIndex) yield new Area {
        // Identify which RS are used by the pipeline
        val resources = ll.uops.keySet.flatMap(_.resources).distinctLinked
        val readAccess = rfaReads.filter(e => resources.exists{
          case RfResource(_, e) => true
          case _ => false
        }).values
        val hazardUntilMax = eus.map(_.getRdBroadcastedFromMax()).max

        val onRs = for (rs <- readAccess) yield new Area {
          val hazards = ArrayBuffer[Bool]()
          val decodeSpec = ArrayBuffer[(Masked, Masked)]()
          for(uop <- ll.uops.values){
            uop.rs.get(rs.rfa.asInstanceOf[RfRead]).foreach{v =>
              decodeSpec += Masked(uop.uop.key) -> (v.from >= hazardUntilMax).mux(Masked.one, Masked.zero)
            }
          }
          val skip = Symplify(c.ctx.uop, decodeSpec, 1).as(Bool()) /// Overall, hazardUntilMax doesn't work when using the FPU as the pipeline get longer, need a proper implementation to enable late RS use

          for (spec <- bypassedSpecs.values) yield new Area {
            for (l <- spec.el.getLayers(); uop <- l.uops.values) {
              uop.rd.foreach { rd =>
                uop.addDecoding(spec.value -> Bool(rd.broadcastedFrom <= spec.at))
              }
            }
          }
          for(writeEu <- eus) {
            val hazardFrom = ll.lane.rfReadHazardFrom(ll.getRsUseAtMin()) // This is a pessimistic aproach
            val hazardUntil = writeEu.getRdBroadcastedFromMax()
            val hazardRange = hazardFrom until hazardUntil
            for(id <- hazardRange) {
              val node = writeEu.ctrl(id-hazardFrom+1)
              hazards += node.up(rdKeys.ENABLE) && node.up(rdKeys.PHYS) === c.ctx.hm(rs.PHYS) && node.up(rdKeys.RFID) === c.ctx.hm(rs.RFID) && !node(getBypassed(writeEu, id)) // node.isValid &&
            }
          }
          val hazard = c.ctx.hm(rs.ENABLE) && hazards.orR && !skip
        }
        c.rsHazards(llId) := onRs.map(_.hazard).orR
      }
    }


    val reservationChecker = for (c <- candidates) yield new Area {
      val onLl = for ((ll, llId) <- lanesLayers.zipWithIndex) yield new Area {
        val res = for (resSpec <- reservationSpecs.values; if resSpec.sels.contains(ll); (selfAt, selfSel) <- resSpec.sels(ll)) yield new Area {
          val checks = for ((otherLl, otherAts) <- resSpec.sels;
                            (otherAt, otherSel) <- otherAts;
                            delta = otherAt - selfAt; if delta > 0) yield new Area { //So currently we aren't trying to catch multi issue reservation being dispacthed at the same cycle (limitation)
            val otherCtrl = otherLl.lane.ctrl(delta)
            val hit = c.ctx.hm(selfSel) && otherCtrl.isValid && otherCtrl(otherSel)
          }
        }
        val hit = res.flatMap(_.checks).map(_.hit).orR
        c.reservationHazards(llId) := hit
      }
    }

    case class MayFlushSpec(el: ExecuteLaneService, at: Int, value: Payload[Bool])
    val mayFlushSpecs = mutable.LinkedHashMap[(ExecuteLaneService, Int), MayFlushSpec]()
    def getMayFlush(el: ExecuteLaneService, at: Int): Payload[Bool] = {
      mayFlushSpecs.getOrElseUpdate(el -> at, MayFlushSpec(el, at, Payload(Bool()).setName("MAY_FLUSH_PRECISE_" + at))).value
    }

    val flushChecker = for (cId <- 0 until slotsCount + Decode.LANES) yield new Area {
      val c = candidates(cId)
      val executeCheck = for (elp <- eus) yield new Area {
        val ctrlRange = dontFlushFromMin+1 to eusFlushHazardUpTo(elp)
        val hits = for(from <- ctrlRange) yield {
          val downstream = for(i <- ctrlRange.low to ctrlRange.high-(from-ctrlRange.low)) yield {
            val otherCtrl = elp.ctrl(from - dontFlushFromMin)
            c.ctx.hm(getDontFlush(i)) && otherCtrl.isValid && otherCtrl(Global.HART_ID) === c.ctx.hartId && otherCtrl(getMayFlush(elp, i))
          }
          downstream.orR
        }
      }
      val olders = candidates.take(cId)
      val oldersHazard = olders.map(o => o.ctx.valid && o.ctx.hm(MAY_FLUSH)).orR
      c.flushHazards := executeCheck.flatMap(_.hits).orR || c.ctx.hm(DONT_FLUSH_FROM_LANES) && oldersHazard
    }

    //TODO this is a very clean implementation, but would need to move it in part before the dispatch to improve timings
    val bypassDecoding = for (spec <- bypassedSpecs.values) yield new Area{
      for (l <- spec.el.getLayers(); uop <- l.uops.values) {
        uop.rd.foreach { rd =>
          uop.addDecoding(spec.value -> Bool(rd.broadcastedFrom <= spec.at))
        }
      }
    }

    //TODO this is a very clean implementation, but would need to move it in part before the dispatch to improve timings
    val mayFlushDecoding = for (spec <- mayFlushSpecs.values) yield new Area {
      val ctrl = spec.el.ctrl(spec.at)
      for (l <- spec.el.getLayers(); uop <- l.uops.values) {
        val v = uop.mayFlushUpTo.getOrElse(-100)
        uop.addDecoding(spec.value -> Bool(v >= spec.at))
      }
    }

    val fenceChecker = new Area{
      val olderInflights = B(for(hartId <- 0 until Global.HART_COUNT) yield host.list[InflightService].map(_.hasInflight(hartId)).orR)
      for (cId <- 0 until slotsCount + Decode.LANES) yield new Area {
        val c = candidates(cId)
        val olderCandidate = candidates.take(cId).map(older => older.ctx.valid && older.ctx.hartId === c.ctx.hartId).orR
        val olderExecute = olderInflights(c.ctx.hartId)
        c.fenceOlderHazards := c.ctx.hm(FENCE_OLDER) && (olderExecute || olderCandidate)
      }
    }



    dispatchCtrl.link.down.ready := True
    val feeds = for(lane <- 0 until Decode.LANES) yield new dispatchCtrl.LaneArea(lane){
      val c = candidates(slotsCount + lane)
      val sending = CombInit(c.fire)
      val sent = RegInit(False) setWhen(sending) clearWhen(ctrlLink.up.isMoving)
      c.cancel := dispatchCtrl.lane(lane).isCancel
      c.ctx.valid := dispatchCtrl.link.isValid && isValid && !sent
      c.ctx.laneLayerHits := LANES_LAYER_HIT.values.map(this(_)).asBits()
      c.ctx.hartId := Global.HART_ID
      c.ctx.uop := Decode.UOP
      for (k <- hmKeys) c.ctx.hm(k).assignFrom(this(k))
      dispatchCtrl.link.down.ready clearWhen(isValid && !sent && !c.fire)
      when(Global.TRAP){ //TODO  May it could be injected futher down the arbitration ?
        c.ctx.laneLayerHits := 1 << lanesLayers.indexOf(trapLayer)
      }
    }

    for (slotId <- 0 until slotsCount) {
      val c = candidates(slotId)
      val s =  slots(slotId)
      c.ctx := s.ctx
      s.ctx.valid clearWhen (c.fire || c.cancel)
      val rp = host[ReschedulePlugin]
      c.cancel := rp.isFlushedAt(dpp.getAge(dispatchAt + 1, false), s.ctx.hartId, 0).getOrElse(False)
    }

    val slotsFeeds = (slotsCount != 0) generate new Area {
      val upCand = candidates.drop(slotsCount)
      val slotCand = candidates.take(slotsCount)
      val free = slotCand.map(_.moving).andR
      val fit = CountOne(upCand.map(!_.moving)) <= slotsCount
      val doIt = free && fit
      when(doIt) {
        dispatchCtrl.link.down.ready := True
        var mask = CombInit(B(upCand.map(!_.moving)))
        val on = for((slot, slotId) <- slots.zipWithIndex) yield new Area{
          val oh = OHMasking.firstV2(mask)
          slot.ctx := OHMux.or(oh, upCand.drop(slotId).map(_.ctx), true)
          mask = (mask & ~oh).drop(1)
        }
      }
    }


    val scheduler = new Area {
      val eusFree = Array.fill(candidates.size + 1)(Bits(eus.size bits))
      val hartFree = Array.fill(candidates.size + 1)(Bits(Global.HART_COUNT bits))
      eusFree(0).setAll()
      hartFree(0).setAll()

      def eusToLayers(mask: Bits) = B(lanesLayers.map(ll => mask(eus.indexOf(ll.lane))))
      def layersToEus(mask: Bits) = B(eus.map(eu => lanesLayers.zipWithIndex.filter(_._1.lane == eu).map(e => mask(e._2)).orR))

      val arbiters = for ((c, id) <- candidates.zipWithIndex) yield new Area {
        val olders = candidates.take(id)
        val candHazards = for(o <- olders) yield new Area{
          val doWrite = o.ctx.valid && o.ctx.hartId === c.ctx.hartId && o.ctx.hm(rdKeys.ENABLE)
          val rfas = Decode.rfaKeys.get.map{case (rfa, k) => c.ctx.hm(k.ENABLE) && o.ctx.hm(rdKeys.PHYS) === c.ctx.hm(k.PHYS) && o.ctx.hm(rdKeys.RFID) === c.ctx.hm(k.RFID)}
          val hit = doWrite && rfas.orR
        }
        val candHazard = candHazards.map(_.hit).orR
        val layersHits = c.ctx.laneLayerHits & ~c.rsHazards & ~c.reservationHazards & eusToLayers(eusFree(id))
        val layerOh = OHMasking.firstV2(layersHits)
        val eusOh = layersToEus(layerOh)
        val doIt = c.ctx.valid && !c.flushHazards && !c.fenceOlderHazards && layerOh.orR && hartFree(id)(c.ctx.hartId) && !candHazard
        eusFree(id + 1) := eusFree(id) & (~eusOh).orMask(!doIt)
        hartFree(id + 1) := hartFree(id) & (~UIntToOh(c.ctx.hartId)).orMask(!c.ctx.valid || doIt)
        c.fire := doIt && !eupp.isFreezed() && !api.haltDispatch
      }
    }

    val inserter = for ((eu, id) <- eus.zipWithIndex; insertNode = eu.ctrl(0).up) yield new Area {
      import insertNode._
      val oh = B(scheduler.arbiters.map(l => l.doIt && l.eusOh(id)))
      val mux = candidates.reader(oh, true)
      val trap = mux(_.ctx.hm(TRAP))
      insertNode(CtrlLaneApi.LANE_SEL) := oh.orR && !mux(_.cancel) && !api.haltDispatch
      Global.HART_ID := mux(_.ctx.hartId)
      Decode.UOP := mux(_.ctx.uop)
      for(k <- hmKeys) insertNode(k).assignFrom(mux(_.ctx.hm(k)))
      when(!CtrlLaneApi.LANE_SEL || trap){
        //Allow to avoid having to check the valid down the pipeline
        rdKeys.ENABLE := False
        MAY_FLUSH := False
      }
      Execute.LANE_AGE := mux(_.age)
      Global.COMPLETED := trap

      val layerOhUnfiltred = scheduler.arbiters.reader(oh)(_.layerOh) // include the bits of the other eu
      val layersOfInterest = lanesLayers.zipWithIndex.filter(_._1.lane == eu) // Only the bits for our eu  (LaneLayer -> Int
      val layer = layersOfInterest.map(e => B(eu.getLayerId(e._1), log2Up(eu.getLayers().size) bits) -> layerOhUnfiltred(e._2))
      eu.LAYER_SEL := OHMux.or(layer.map(_._2).asBits(), layer.map(_._1), true)
    }


//    for(c <- candidates){
//      c.rsHazards.removeAssignments().clearAll()
//      c.flushHazards.removeAssignments().clearAll()
//      c.fenceOlderHazards.removeAssignments().clearAll()
//    }

    buildBefore.release()
  }
}

/*
How to detect RD->RSx hazards for a given candidate:
0)  for each ctrl (pipeline deepness)
      for each execute lane
        decode if RD is readable or not
1)  for each ctrl (pipeline deepness)
      for each execute lane,
        generate a hazard signal := ctrl.rd == RSx
      Aggregate the lanes hazards
2)  for each implementation slot
      process if scheduled to that slot would produce hazards
3)  schedule to the implementation slot with the best priority

 */