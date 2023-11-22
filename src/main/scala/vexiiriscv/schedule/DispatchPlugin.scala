package vexiiriscv.schedule

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{CtrlApi, CtrlLink, NodeApi, Payload}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.{AccessKeys, Decode, DecodePipelinePlugin, DecoderService}
import vexiiriscv.execute.ExecuteUnitService
import vexiiriscv.misc.PipelineBuilderPlugin
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{RD, RfRead, RfResource}

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

    case class MicroOpCtx() extends Bundle{
      val valid = Bool()
      val compatibility = Bits(eus.size bits)
      val hartId = Global.HART_ID()
      val microOp = Decode.MICRO_OP()
      val rfa = new HardMap()
      for((k, ac) <- Decode.rfaKeys){
        rfa.add(ac.ENABLE)
        rfa.add(ac.RFID)
        rfa.add(ac.PHYS)
      }
    }



    val rdKeys = Decode.rfaKeys.get(RD)

    assert(Global.HART_COUNT.get == 1, "need to implement write to write RD hazard for stuff which can be schedule in same cycle")
    assert(rdKeys.rfMapping.size == 1, "Need to check RFID usage and missing usage kinda everywhere")

    val candidates = for(cId <- 0 until slotsCount + Decode.LANES) yield new Area{
      val ctx = MicroOpCtx()
      val fire = Bool()
      val eusReady = Bits(eus.size bits)
      val euLogic = for((readEu, euId) <- eus.zipWithIndex) yield new Area{
        val insertAt = readEu.insertAt
        val readAt = readEu.rfReadAt
        val readTime = readAt - insertAt

        //Identify which RS are used by the pipeline
        val readEuRessources = readEu.getMicroOp().flatMap(_.resources).distinctLinked
        val rfaReads = Decode.rfaKeys.filter(_._1.isInstanceOf[RfRead])
        val rfaReadsFiltered = rfaReads.filter(e => readEuRessources.exists{
          case RfResource(_, e) => true
          case _ => false
        }).values

        val rsLogic = for (rs <- rfaReadsFiltered) yield new Area {
          val hazards = ArrayBuffer[Bool]()
          for(writeEu <- eus) {

            val opSpecs = writeEu.getMicroOp().map(writeEu.getSpec)
            val opWithRd = opSpecs.filter(spec => spec.op.resources.exists{
              case RfResource(_, RD) => true
              case _ => false
            })
            val readableAt = opWithRd.map(_.rd.get.rfReadableAt).max
            val checkCount = readableAt - 1 - readAt
            def checkFrom = insertAt + 1
            val range = checkFrom until checkFrom + checkCount
            for(id <- range) {
              val node = writeEu.nodeAt(id)
              hazards += node(rdKeys.ENABLE) && node(rdKeys.PHYS) === ctx.rfa(rs.PHYS)
            }
          }
          val hazard = ctx.rfa(rs.ENABLE) && hazards.orR
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
      for ((k, ac) <- Decode.rfaKeys) {
        c.ctx.rfa(ac.ENABLE) := ac.ENABLE
        c.ctx.rfa(ac.RFID) := ac.RFID
        c.ctx.rfa(ac.PHYS) := ac.PHYS
      }
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
      for ((k, ac) <- Decode.rfaKeys) {
        ac.ENABLE := mux(_.ctx.rfa(ac.ENABLE))
        ac.RFID := mux(_.ctx.rfa(ac.RFID))
        ac.PHYS := mux(_.ctx.rfa(ac.PHYS))
      }
      Decode.rfaKeys.get(RD).ENABLE clearWhen(!insertNode.valid) //Allow to avoid having to check the valid down the pipeline
//      RD_HAZARD := False
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