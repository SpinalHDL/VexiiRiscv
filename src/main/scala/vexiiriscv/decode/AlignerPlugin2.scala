package vexiiriscv.decode

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.{Global, decode}
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.misc.PipelineService
import vexiiriscv.prediction.{FetchWordPrediction, Prediction}
import vexiiriscv.riscv.{INSTRUCTION_SIZE, Riscv}
import vexiiriscv.schedule.{Ages, ReschedulePlugin}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * - Warning : bad btb hash prediction may cut a word, need to project against that
 *
 * - Build buffer long enough
 * - Scan buffer for instructions [fusion]
 * - Decode -> Serialize uop
 * - dispatch on lanes
 */

//Warning, if it start to hold stats => you need to notify TrapService when flush is pending
class AlignerPlugin2(fetchAt : Int,
                     lanes : Int = 1) extends FiberPlugin with PipelineService{
  override def getLinks(): Seq[Link] = Nil //logic.connectors

  val lastSliceData, firstSliceData = mutable.LinkedHashSet[NamedType[_ <: Data]]()

  val elaborationLock = Retainer()
  val logic = during setup new Area{
    val fpp = host[FetchPipelinePlugin]
    val dpp = host[DecodePipelinePlugin]
    val buildBefore = retains(fpp.elaborationLock, dpp.elaborationLock)
    awaitBuild()

    Decode.LANES.set(lanes)
    Decode.INSTRUCTION_WIDTH.get

    assert(Decode.INSTRUCTION_WIDTH.get * Decode.LANES == Fetch.WORD_WIDTH.get)
    assert(!Riscv.RVC)
    assert(isPow2(Decode.LANES.get))

    elaborationLock.await()

    val scannerSlices = Fetch.SLICE_COUNT * 2

    val MASK_BACK, MASK_FRONT = Payload(Bits(Fetch.SLICE_COUNT bits))


    val maskGen = new fpp.Fetch(fetchAt - 1) {
      val frontMasks = (0 until Fetch.SLICE_COUNT).map(i => B((1 << Fetch.SLICE_COUNT) - (1 << i), Fetch.SLICE_COUNT bits))
      MASK_FRONT := frontMasks.read(Fetch.WORD_PC(Fetch.SLICE_RANGE.get))
      //TODO MASK_BACK
    }

    val up = fpp.fetch(fetchAt).down
    val downCtrl = dpp.ctrl(0)
    val downNode = downCtrl.link.up


    firstSliceData += Fetch.ID
    lastSliceData += Prediction.BRANCH_HISTORY
    val hmElements = (firstSliceData ++ lastSliceData).toSeq
    case class SliceCtx() extends Bundle {
      val pc = Global.PC()
      val instruction = Decode.INSTRUCTION()
      val hm = HardMap(hmElements)
    }

    // Provide the fetched data in a buffer agnostic way
    val slices = new Area {
      val data = Vec.fill(scannerSlices)(Bits(Fetch.SLICE_WIDTH bits))
      val mask = Bits(scannerSlices bits)

      case class ReadCtxFirst(sid : Int, oh : Bool)
      case class ReadCtxSpec(first : Seq[ReadCtxFirst], usage : Bits, ctx : SliceCtx)
      val readCtxs = ArrayBuffer[ReadCtxSpec]()
      def readCtx(slices : Seq[Int], oh : Bits, usage : Bits) : SliceCtx = readCtxs.addRet(ReadCtxSpec(
        (slices, oh.asBools).zipped.toSeq.map(e => ReadCtxFirst(e._1, e._2)),
        usage,
        SliceCtx()
      )).ctx
    }

    val slicesInstructions = (0 until scannerSlices).map(sid =>
      slices.data.drop(sid).take(Decode.INSTRUCTION_SLICE_COUNT_MAX).asBits.resize(Decode.INSTRUCTION_WIDTH bits)
    )

    //TODO prediction disruption
    //Find out which slices would have enough data to be the start of an instruction
    val scanners = for (i <- 0 until scannerSlices) yield new Area {
      val usageMask = B(0, scannerSlices bits)
      val checker = for(lid <- 0 until Decode.INSTRUCTION_SLICE_COUNT_MAX) yield new Area{
        val sid = i+lid
        val required = lid match {
          case 0 => True
          case 1 => slices.data(i)(1 downto 0) === 3 // => Not RVC
        }
        if(sid < scannerSlices) usageMask(sid) := required
        val present = (sid < scannerSlices).mux(slices.mask(sid), False)
        val valid = !required || present
      }
      val valid = checker.map(_.valid).andR
    }

    val usedMask = Vec.fill(Decode.LANES.get+1)(Bits(scannerSlices bits))
    usedMask(0) := 0
    val extractors = for (eid <- 0 until Decode.LANES) yield new Area {
      val usableSliceRange = eid until scannerSlices //Can be tweek to generate smaller designs
      val usableMask = usableSliceRange.map(sid => scanners(sid).valid && !usedMask(eid)(sid)).asBits
      val slicesOh = OHMasking.firstV2(usableMask)
      val usageMask = MuxOH.or(slicesOh, usableSliceRange.map(sid => scanners(sid).usageMask), true)
      usedMask(eid+1) := usedMask(eid) | usageMask

      val valid = slicesOh.orR
      val ctx = slices.readCtx(usableSliceRange, slicesOh, usageMask)
    }

    val feeder = new Area{
      val harts = for (hartId <- Global.hartsIds) yield new Area {
        val dopId = Reg(Decode.DOP_ID) init (0)
        when(downNode.isFiring && downNode(Global.HART_ID) === hartId) {
          dopId := downCtrl.lane(Decode.LANES-1)(Decode.DOP_ID) + 1
        }
      }

      val lanes = for(laneId <- Decode.laneIds) yield new Area{
        val lane = downCtrl.lane(laneId)
        val pcLaneLow = log2Up(Decode.INSTRUCTION_WIDTH/8)
        val pcLaneRange = pcLaneLow + log2Up(Decode.LANES) -1 downto pcLaneLow
        val extractor = extractors(laneId)

        val valid = CombInit(extractor.valid)
        lane.up(lane.LANE_SEL)       := valid
        lane.up(Decode.INSTRUCTION)     := extractor.ctx.instruction
        lane.up(Decode.INSTRUCTION_RAW) := extractor.ctx.instruction
        lane.up(Global.PC)              := extractor.ctx.pc
        lane.up(Decode.DOP_ID)          := (laneId match {
          case 0 => harts.map(_.dopId).read(downNode(Global.HART_ID))
          case _ => downCtrl.lane(laneId-1)(Decode.DOP_ID) + downCtrl.lane(laneId-1).isValid.asUInt
        })
        for (e <- firstSliceData ++ lastSliceData) lane.up(e).assignFrom(extractor.ctx.hm.apply(e))
        lane.up(Global.TRAP) := False //TODO
//        lane(Global.TRAP) := up(Global.TRAP)
//        val onBtb = withBtb generate new Area{
//          assert(!Riscv.RVC)
//          val afterPrediction = lane(Global.PC)(Fetch.SLICE_RANGE) > up(Prediction.WORD_JUMP_SLICE)
//          val didPrediction = !afterPrediction && lane(Global.PC)(Fetch.SLICE_RANGE) + lane(Decode.INSTRUCTION_SLICE_COUNT) >= up(Prediction.WORD_JUMP_SLICE) //TODO take care of : what's about the prediction landed on a slice which map nobody ?
//          lane(Prediction.ALIGNED_JUMPED) := up(Prediction.WORD_JUMPED) && didPrediction
//          lane(Prediction.ALIGNED_JUMPED_PC) := up(Prediction.WORD_JUMP_PC)
//          lane.up(lane.LANE_SEL) clearWhen(up(Prediction.WORD_JUMPED) && afterPrediction)
//          lane(Prediction.ALIGNED_SLICES_BRANCH) := up(Prediction.WORD_SLICES_BRANCH)
//          lane(Prediction.ALIGNED_SLICES_TAKEN) := up(Prediction.WORD_SLICES_TAKEN)
//        }
      }

      downNode.valid := lanes.map(_.valid).orR
    }



    val buffer = new Area {
      val bufferedSlices = Fetch.WORD_WIDTH/Fetch.SLICE_WIDTH
      val data = Reg(Fetch.WORD)
      val mask = Reg(MASK_FRONT) init(0)
      val pc = Reg(Fetch.WORD_PC())
      val trap = Reg(Bool()) init (False)
      val hm = Reg(HardMap(hmElements))

      slices.data.assignFromBits(up(Fetch.WORD) ## data)
      slices.mask := (up.valid ? up(MASK_FRONT) | B(0)) ## mask

      val downFire = downNode.isReady || downNode.isCancel

      val haltUp = (mask & ~ usedMask.last.dropHigh(Fetch.SLICE_COUNT).andMask(downFire)).orR
      up.ready := !haltUp

      when(downFire){
        mask := 0
      }
      when(up.isValid && up.isReady){
        data := up(Fetch.WORD)
        mask := up(MASK_FRONT) & ~ usedMask.last.takeHigh(Fetch.SLICE_COUNT).andMask(downFire)
        trap := up(Global.TRAP)
        pc   := up(Fetch.WORD_PC)
        for(e <- hmElements) hm(e).assignFrom(up(e))
      }

      val age = Ages.DECODE - Ages.STAGE
      val flushIt = host[ReschedulePlugin].isFlushedAt(age, U(0), U(0)).getOrElse(False)
      when(flushIt) {
        mask := 0
      }

      val readers = for(spec <- slices.readCtxs) yield new Area{
        val idsBuffer = spec.first.filter(_.sid < bufferedSlices)
        val idsUp = spec.first.filter(_.sid >= bufferedSlices)
        val firstFromBuffer = idsBuffer.map(e => e.oh).orR
        val lastFromBuffer = idsUp.map(e => spec.usage(e.sid)).norR
        spec.ctx.instruction := OhMux.or(spec.first.map(_.oh).asBits, spec.first.map(e => slicesInstructions(e.sid)), true)
        spec.ctx.pc := firstFromBuffer.mux(pc, up(Fetch.WORD_PC))
        spec.ctx.pc(Fetch.SLICE_RANGE) := OHToUInt((0 until Fetch.SLICE_COUNT).map(off => spec.first.filter(_.sid % Fetch.SLICE_COUNT == off).map(_.oh).orR))
        for (e <- firstSliceData) spec.ctx.hm(e).assignFrom(firstFromBuffer.mux(hm(e), up(e)))
        for (e <- lastSliceData) spec.ctx.hm(e).assignFrom(lastFromBuffer.mux(hm(e), up(e)))
      }
    }

    buildBefore.release()
  }
}
