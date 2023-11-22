package vexiiriscv.schedule

import spinal.core._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.pipeline.{CtrlApi, CtrlLink, NodeApi, Payload}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.{AccessKeys, Decode, DecodePipelinePlugin, DecoderService}
import vexiiriscv.execute.{Execute, ExecuteUnitService}
import vexiiriscv.misc.PipelineBuilderPlugin
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{RD, RfRead, RfResource}

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

class DispatchPlugin(dispatchAt : Int = 3) extends FiberPlugin{
  lazy val dpp = host[DecodePipelinePlugin]
  lazy val dp = host[DecoderService]
  addLockable(host[PipelineBuilderPlugin])
  addLockable(dpp)
  addRetain(dp)

  during setup{
    host.list[ExecuteUnitService].foreach(_.linkLock.retain())
  }

  val logic = during build new Area{
    val dispatchCtrl = dpp.ctrl(dispatchAt)

    val eus = host.list[ExecuteUnitService].sortBy(_.dispatchPriority).reverse
    val EU_COMPATIBILITY = eus.map(eu => eu -> Payload(Bool())).toMapLinked()
    for(eu <- eus){
      val key = EU_COMPATIBILITY(eu)
      dp.addMicroOpDecodingDefault(key, False)
      for(op <- eu.getMicroOp()){
        dp.addMicroOpDecoding(op, key, True)
      }
    }
    dp.release()
    val slotsCount = 0

    val hmKeys = mutable.LinkedHashSet[Payload[_ <: Data]]()
    hmKeys.add(Global.PC)
    hmKeys.add(Decode.MICRO_OP_ID)
    for ((k, ac) <- Decode.rfaKeys) {
      hmKeys.add(ac.ENABLE)
      hmKeys.add(ac.RFID)
      hmKeys.add(ac.PHYS)
    }

    case class MicroOpCtx() extends Bundle{
      val valid = Bool()
      val compatibility = Bits(eus.size bits)
      val hartId = Global.HART_ID()
      val microOp = Decode.MICRO_OP()
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
      val readAt = eu.rfReadAt
      val readableAt = opWithRd.map(_.rd.get.rfReadableAt).max
      val checkCount = readableAt - 1
      val range = 1 until 1 + checkCount
      eu -> range
    }.toMapLinked()

    for(eu <- eus){
      val all = ArrayBuffer[Masked]()
      for(op <- eu.getMicroOp()){
        all += Masked(op.key)
      }
      val checkRange = euToCheckRange(eu)
      val decs = checkRange.map(_ -> new DecodingSpec(Bool()).setDefault(Masked.zero)).toMapLinked()
      val node = eu.nodeAt(1)
      for (spec <- eu.getMicroOpSpecs()) {
        val key = Masked(spec.op.key)
        spec.rd match {
          case Some(rd) => rd.bypassesAt.foreach { id =>
            decs(id-checkRange.low).addNeeds(key, Masked.one)
          }
          case None =>
        }
      }

      for (id <- checkRange) {
        node(Execute.BYPASSED, id) := decs(id).build(node(Decode.MICRO_OP), all)
      }
    }

    assert(Global.HART_COUNT.get == 1, "need to implement write to write RD hazard for stuff which can be schedule in same cycle")
    assert(rdKeys.rfMapping.size == 1, "Need to check RFID usage and missing usage kinda everywhere, also the BYPASS signal should be set high for all stages after the writeback for the given RF")

    val candidates = for(cId <- 0 until slotsCount + Decode.LANES) yield new Area{
      val ctx = MicroOpCtx()
      val fire = Bool()
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
            val range = euToCheckRange(writeEu).dropRight(readAt)
            for(id <- range) {
              val node = writeEu.nodeAt(id)
              hazards += node(rdKeys.ENABLE) && node(rdKeys.PHYS) === ctx.hm(rs.PHYS) && !node(Execute.BYPASSED, id)
            }
          }
          val hazard = ctx.hm(rs.ENABLE) && hazards.orR
        }
        eusReady(euId) :=  !rsLogic.map(_.hazard).orR //TODO handle bypasses
      }
    }

    for(lane <- 0 until Decode.LANES) new dispatchCtrl.Area(lane){
      val c = candidates(slotsCount + lane)
      c.ctx.valid := dispatchCtrl.isValid && Dispatch.MASK
      c.ctx.compatibility := EU_COMPATIBILITY.values.map(this(_)).asBits()
      c.ctx.hartId := Global.HART_ID
      c.ctx.microOp := Decode.MICRO_OP
      for (k <- hmKeys) c.ctx.hm(k).assignFrom(this(k))
      haltWhen(!c.fire)
    }

    val scheduler = new Area {
      val eusFree, hartFree = Array.fill(candidates.size + 1)(Bits(eus.size bits))
      eusFree(0).setAll()
      hartFree(0).setAll()
      val layer = for ((c, id) <- candidates.zipWithIndex) yield new Area {
        val eusHits = c.ctx.compatibility & eusFree(id) & c.eusReady
        val eusOh = OHMasking.firstV2(eusHits)
        val doIt = c.ctx.valid && eusHits.orR && hartFree(id)(c.ctx.hartId)
        eusFree(id + 1) := eusFree(id) & eusOh.orMask(!doIt)
        hartFree(id + 1) := hartFree(id) & (~UIntToOh(c.ctx.hartId)).orMask(doIt)
        c.fire := doIt
      }
    }

    val inserter = for ((eu, id) <- eus.zipWithIndex; insertNode = eu.insertNode) yield new Area with NodeApi {
      override def getNode = insertNode
      val oh = B(scheduler.layer.map(l => l.doIt && l.eusOh(id)))
      val mux = candidates.reader(oh, true)
      insertNode.valid := oh.orR
      Global.HART_ID := mux(_.ctx.hartId)
      Decode.MICRO_OP := mux(_.ctx.microOp)
      for(k <- hmKeys) k.assignFrom(mux(_.ctx.hm(k)))
      Decode.rfaKeys.get(RD).ENABLE clearWhen(!insertNode.valid) //Allow to avoid having to check the valid down the pipeline
    }

    dispatchCtrl.down.ready := True //TODO remove

   eus.foreach(_.linkLock.release())
  }
}

/*
//TODO
- RFID hazard
- no hazard on RD x0
- Check RD ordering for dual issue (2 instruction scheduled the same cycle)
 */