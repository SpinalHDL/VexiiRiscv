package vexiiriscv.schedule

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.pipeline.{CtrlApi, CtrlLaneApi, CtrlLink, NodeApi, Payload}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.{AccessKeys, Decode, DecodePipelinePlugin, DecoderService}
import vexiiriscv.execute.{Execute, ExecuteLanePlugin, ExecuteLaneService, ExecutePipelinePlugin}
import vexiiriscv.misc.PipelineBuilderPlugin
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

class DispatchPlugin(var dispatchAt : Int) extends FiberPlugin{
  lazy val dpp = host[DecodePipelinePlugin]
  lazy val dp = host[DecoderService]
  lazy val eupp = host[ExecutePipelinePlugin]

  buildBefore(host[PipelineBuilderPlugin].elaborationLock)
  buildBefore(dpp.elaborationLock)
  buildBefore(eupp.pipelineLock)
  setupRetain(dp.elaborationLock)

  during setup{
    host.list[ExecuteLaneService].foreach(_.pipelineLock.retain())
  }

  val elaborationLock = Lock()

  val MAY_FLUSH = Payload(Bool())
  val DONT_FLUSH = Payload(Bool())
  val DONT_FLUSH_FROM_LANES = Payload(Bool())


  val hmKeys = mutable.LinkedHashSet[Payload[_ <: Data]]()

//  val fenceYoungerOps = mutable.LinkedHashSet[MicroOp]()
//  val fenceOlderOps = mutable.LinkedHashSet[MicroOp]()
//  def fenceYounger(op : MicroOp) = fenceYoungerOps += op
//  def fenceOlder(op : MicroOp) = fenceOlderOps += op

  val logic = during build new Area{
    Execute.LANE_AGE_WIDTH.set(log2Up(Decode.LANES))

    elaborationLock.await()
    val dispatchCtrl = dpp.ctrl(dispatchAt)

    val eus = host.list[ExecuteLaneService].sortBy(_.dispatchPriority).reverse
    val EU_COMPATIBILITY = eus.map(eu => eu -> Payload(Bool())).toMapLinked()
    for(eu <- eus){
      val key = EU_COMPATIBILITY(eu)
      dp.addMicroOpDecodingDefault(key, False)
      for(op <- eu.getMicroOp()){
        dp.addMicroOpDecoding(op, key, True)
      }
    }

    hmKeys.add(MAY_FLUSH)
    hmKeys.add(DONT_FLUSH)
    hmKeys.add(DONT_FLUSH_FROM_LANES)
    dp.addMicroOpDecodingDefault(MAY_FLUSH, False)
    dp.addMicroOpDecodingDefault(DONT_FLUSH, False)
    dp.addMicroOpDecodingDefault(DONT_FLUSH_FROM_LANES, False)
    val flushesUpTo = eus.flatMap(_.getMicroOpSpecs().map(_.mayFlushUpTo.getOrElse(-1))).max
    for (eu <- eus; spec <- eu.getMicroOpSpecs()) {
      spec.mayFlushUpTo match {
        case Some(x) => dp.addMicroOpDecoding(spec.op, MAY_FLUSH, True)
        case None =>
      }
      spec.dontFlushFrom match {
        case Some(x) =>
          dp.addMicroOpDecoding(spec.op, DONT_FLUSH_FROM_LANES, True)
          if (x < flushesUpTo) {
            dp.addMicroOpDecoding(spec.op, DONT_FLUSH, True)
          }
        case None =>
      }
    }

    dp.elaborationLock.release()
    val slotsCount = 0

    hmKeys.add(Global.PC)
    hmKeys.add(Decode.UOP_ID)
    for ((k, ac) <- Decode.rfaKeys) {
      hmKeys.add(ac.ENABLE)
      hmKeys.add(ac.RFID)
      hmKeys.add(ac.PHYS)
    }

    case class MicroOpCtx() extends Bundle{
      val valid = Bool()
      val compatibility = Bits(eus.size bits)
      val hartId = Global.HART_ID()
      val microOp = Decode.UOP()
      val hm = new HardMap()
      hmKeys.foreach(e => hm.add(e))
    }

    val rdKeys = Decode.rfaKeys.get(RD)
    val euToCheckRange = eus.map{eu =>
      val opSpecs = eu.getMicroOp().map(eu.getSpec)
      val opWithRd = opSpecs.filter(spec => spec.op.resources.exists {
        case RfResource(_, RD) => true
        case _ => false
      })
      val readableAt = opWithRd.flatMap(_.rd.map(_.rfReadableAt)).max
      val range = 1 to readableAt
      eu -> range
    }.toMapLinked()

    // Will generate the Execute.BYPASSED decoder in each EU for their respective euToCheckRange
    for(eu <- eus){
      val all = ArrayBuffer[Masked]()
      for(op <- eu.getMicroOp()){
        all += Masked(op.key)
      }
      val checkRange = euToCheckRange(eu)
      val decs = checkRange.map(_ -> new DecodingSpec(Bool()).setDefault(Masked.zero)).toMapLinked()
      val node = eu.ctrl(1) //TODO May be done earlier to improve FMax
      for (spec <- eu.getMicroOpSpecs()) {
        val key = Masked(spec.op.key)
        spec.rd match {
          case Some(rd) => rd.bypassesAt.foreach { id =>
            decs(id).addNeeds(key, Masked.one)
          }
          case None =>
        }
      }

      for (id <- checkRange) {
        node(Execute.BYPASSED, id) := decs(id).build(node(Decode.UOP), all)
      }
    }

    assert(Global.HART_COUNT.get == 1, "need to implement write to write RD hazard for stuff which can be schedule in same cycle")
    assert(rdKeys.rfMapping.size == 1, "Need to check RFID usage and missing usage kinda everywhere, also the BYPASS signal should be set high for all stages after the writeback for the given RF")

    val dontFlushFrom = eus.flatMap(_.getMicroOpSpecs().map(_.dontFlushFrom.getOrElse(100))).min
    val candidates = for(cId <- 0 until slotsCount + Decode.LANES) yield new Area{
      val ctx = MicroOpCtx()
      val fire = Bool()
      val flushHazard = Bool()
      val cancel = Bool()

      val eusReady = Bits(eus.size bits)
      //TODO merge duplicated logic using signal cache
      val euLogic = for((readEu, euId) <- eus.zipWithIndex) yield new Area{
        val readAt = readEu.rfReadAt

        // Identify which RS are used by the pipeline
        val readEuRessources = readEu.getMicroOp().flatMap(_.resources).distinctLinked
        val rfaReads = Decode.rfaKeys.filter(_._1.isInstanceOf[RfRead])
        val rfaReadsFiltered = rfaReads.filter(e => readEuRessources.exists{
          case RfResource(_, e) => true
          case _ => false
        }).values

        // Check hazard for each rs in the whole cpu
        val rsLogic = for (rs <- rfaReadsFiltered) yield new Area {
          val hazards = ArrayBuffer[Bool]()
          for(writeEu <- eus) {
            val range = (1 until writeEu.getRdReadableAtMax() + readEu.rfReadLatencyMax - readAt)
            for(id <- range) {
              val node = writeEu.ctrl(id)
              hazards += node(rdKeys.ENABLE) && node(rdKeys.PHYS) === ctx.hm(rs.PHYS) && !node(Execute.BYPASSED, id)
            }
          }
          val hazard = ctx.hm(rs.ENABLE) && hazards.orR
        }
        eusReady(euId) :=  !rsLogic.map(_.hazard).orR
      }
    }

    //TODO flushChecker is pessimistic, would need to improve by checking hazard depending from were the current op can't be flushed any more from (LSU)
    val flushChecker = for (cId <- 0 until slotsCount + Decode.LANES) yield new Area {
      val c = candidates(cId)
      val executeCheck = for (elp <- eus) yield new Area {
        val flushUpTo = elp.getMicroOpSpecs().flatMap(e => e.mayFlushUpTo).fold(-100)(_ max _)
        val ctrlRange = 1 to flushUpTo - dontFlushFrom
        val hits = ctrlRange.map(i => elp.ctrl(i)(MAY_FLUSH) && elp.ctrl(i)(Global.HART_ID) === c.ctx.hartId)
      }
      val olders = candidates.take(cId)
      val oldersHazard = olders.map(o => o.ctx.valid && o.ctx.hm(MAY_FLUSH)).orR
      c.flushHazard := c.ctx.hm(DONT_FLUSH) && executeCheck.flatMap(_.hits).orR || c.ctx.hm(DONT_FLUSH_FROM_LANES) && oldersHazard
    }

    dispatchCtrl.link.down.ready := True
    val feeds = for(lane <- 0 until Decode.LANES) yield new dispatchCtrl.LaneArea(lane){
      val c = candidates(slotsCount + lane)
      val sending = CombInit(c.fire)
      val sent = RegInit(False) setWhen(sending) clearWhen(ctrlLink.up.isMoving)
      c.cancel := dispatchCtrl.lane(lane).cancel
      c.ctx.valid := dispatchCtrl.link.isValid && LANE_SEL && !sent
      c.ctx.compatibility := EU_COMPATIBILITY.values.map(this(_)).asBits()
      c.ctx.hartId := Global.HART_ID
      c.ctx.microOp := Decode.UOP
      for (k <- hmKeys) c.ctx.hm(k).assignFrom(this(k))
      dispatchCtrl.link.down.ready clearWhen(LANE_SEL && !sent && !c.fire)
    }

    val scheduler = new Area {
      val eusFree = Array.fill(candidates.size + 1)(Bits(eus.size bits))
      val hartFree = Array.fill(candidates.size + 1)(Bits(Global.HART_COUNT bits))
      eusFree(0).setAll()
      hartFree(0).setAll()
      val layer = for ((c, id) <- candidates.zipWithIndex) yield new Area {
        val olders = candidates.take(id)
        val olderHazards = for(o <- olders) yield new Area{
          val doWrite = o.ctx.valid && o.ctx.hartId === c.ctx.hartId && o.ctx.hm(rdKeys.ENABLE)
          val rfas = Decode.rfaKeys.get.map{case (rfa, k) => c.ctx.hm(k.ENABLE) && o.ctx.hm(rdKeys.PHYS) === c.ctx.hm(k.PHYS) && o.ctx.hm(rdKeys.RFID) === c.ctx.hm(k.RFID)}
          val hit = doWrite && rfas.orR
        }
        val oldersHazard = olderHazards.map(_.hit).orR
        val eusHits = c.ctx.compatibility & eusFree(id) & c.eusReady
        val eusOh = OHMasking.firstV2(eusHits)
        val doIt = c.ctx.valid && !c.flushHazard && eusHits.orR && hartFree(id)(c.ctx.hartId) && !oldersHazard
        eusFree(id + 1) := eusFree(id) & (~eusOh).orMask(!doIt)
        hartFree(id + 1) := hartFree(id) & (~UIntToOh(c.ctx.hartId)).orMask(!c.ctx.valid || doIt)
        c.fire := doIt && !eupp.isFreezed()
      }
    }

    val inserter = for ((eu, id) <- eus.zipWithIndex; insertNode = eu.ctrl(0).up) yield new Area {
      import insertNode._
      val oh = B(scheduler.layer.map(l => l.doIt && l.eusOh(id)))
      val mux = candidates.reader(oh, true)
      insertNode(CtrlLaneApi.LANE_SEL) := oh.orR && !mux(_.cancel)
      Global.HART_ID := mux(_.ctx.hartId)
      Decode.UOP := mux(_.ctx.microOp)
      for(k <- hmKeys) insertNode(k).assignFrom(mux(_.ctx.hm(k)))
      when(!CtrlLaneApi.LANE_SEL){
        //Allow to avoid having to check the valid down the pipeline
        rdKeys.ENABLE := False
        MAY_FLUSH := False
      }
      Execute.LANE_AGE := OHToUInt(oh)
    }

    eupp.ctrl(0).up.setAlwaysValid()

    eus.foreach(_.pipelineLock.release())
  }
}

/*
//TODO
- RFID hazard
 */