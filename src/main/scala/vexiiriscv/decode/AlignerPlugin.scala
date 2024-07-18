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
import vexiiriscv.riscv.{INSTRUCTION_SIZE, Riscv, RvcDecompressor}
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
/*
withBuffer => A Fetch.WORD sized buffer will be added to allow unaligned instruction to be read
!withBuffer => Fetch lanes directly feed Decode lanes without muxes
 */
class AlignerPlugin(fetchAt : Int,
                    lanes : Int,
                    withBuffer : Boolean) extends FiberPlugin with AlignerService with InjectorService{


  val api = during build new Area{
    assert(Global.HART_COUNT.get == 1)
    val singleFetch = False
    val downMoving = Bool()
    val haltIt = False
  }

  val logic = during setup new Area{
    val fpp = host[FetchPipelinePlugin]
    val dpp = host[DecodePipelinePlugin]
    val buildBefore = retains(fpp.elaborationLock, dpp.elaborationLock)
    awaitBuild()

    Decode.LANES.set(lanes)
    Decode.INSTRUCTION_WIDTH.get

    if(!withBuffer) assert(Decode.INSTRUCTION_WIDTH.get * Decode.LANES == Fetch.WORD_WIDTH.get)
    assert(isPow2(Decode.LANES.get))

    elaborationLock.await()

    val withBtb = host.get[FetchWordPrediction].nonEmpty
    val scannerSlices = Fetch.SLICE_COUNT * (1+withBuffer.toInt)
    val FETCH_MASK, FETCH_LAST = Payload(Bits(Fetch.SLICE_COUNT bits)) //You can assume that if a given bit of FETCH_LAST is set, you can assume it is valid data


    val maskGen = new fpp.Fetch(fetchAt) { //Could be move up for better timings, partialy
      val frontMasks = (0 until Fetch.SLICE_COUNT).map(i => B((1 << Fetch.SLICE_COUNT) - (1 << i), Fetch.SLICE_COUNT bits))
      val backMasks = (0 until Fetch.SLICE_COUNT).map(i => B((2 << i) - 1, Fetch.SLICE_COUNT bits))
      withBtb match {
        case false => {
          FETCH_MASK := frontMasks.read(Fetch.WORD_PC(Fetch.SLICE_RANGE.get))
          FETCH_LAST.clearAll()
        }
        case true => {
          FETCH_MASK := frontMasks.read(Fetch.WORD_PC(Fetch.SLICE_RANGE.get)) & backMasks.read(Prediction.WORD_JUMP_SLICE).orMask(!Prediction.WORD_JUMPED)
          FETCH_LAST := withBtb.mux(UIntToOh(Prediction.WORD_JUMP_SLICE).andMask(isValid && Prediction.WORD_JUMPED), B(0))
        }
      }
    }

    val up = fpp.fetch(fetchAt).down
    val downCtrl = dpp.ctrl(0)
    val downNode = downCtrl.link.up


    firstSliceData += Fetch.ID
    lastSliceData += Prediction.BRANCH_HISTORY
    if(withBtb) lastSliceData ++= List(
      Prediction.WORD_SLICES_BRANCH,
      Prediction.WORD_SLICES_TAKEN,
      Prediction.WORD_JUMP_PC,
      Prediction.WORD_JUMPED,
      Prediction.WORD_JUMP_SLICE
    )

    val hmElements = (firstSliceData ++ lastSliceData).toSeq
    case class SliceCtx() extends Bundle {
      val pc = Global.PC()
      val instruction = Decode.INSTRUCTION()
      val hm = HardMap(hmElements)
      val trap = Bool()
    }

    // Provide the fetched data in a buffer agnostic way
    val slices = new Area {
      val data = Vec.fill(scannerSlices)(Bits(Fetch.SLICE_WIDTH bits))
      val mask = Bits(scannerSlices bits)
      val last = Bits(scannerSlices bits) //when the given bit is set, it mean that the given slice was predicted as being the last one of an instruction

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
      val usageMask = B(0, scannerSlices bits).allowOverride()
      val checker = for(lid <- 0 until Decode.INSTRUCTION_SLICE_COUNT_MAX) yield new Area{
        val sid = i+lid
        val required = lid match {
          case 0 => True
          case 1 => slices.data(i)(1 downto 0) === 3 // => Not RVC
        }
        val last = lid match {
          case 0 => slices.data(i)(1 downto 0) =/= 3
          case 1 => slices.data(i)(1 downto 0) === 3
        }
        if(sid < scannerSlices) usageMask(sid) := required
        val redo = (sid < scannerSlices && withBtb && withBuffer && Riscv.RVC).mux(required && slices.last(sid) && !last, False)
        val present = (sid < scannerSlices).mux(slices.mask(sid), False)
        val valid = lid match {
          case 0 => CombInit(present)
          case _ => present || !required
        }
      }
      val redo = checker.map(_.redo).orR
      val valid = checker.head.valid && (checker.tail.map(_.valid).andR || checker.map(_.redo).orR)
    }

    val usedMask = Vec.fill(Decode.LANES.get+1)(Bits(scannerSlices bits))
    usedMask(0) := 0
    val extractors = for (eid <- 0 until Decode.LANES) yield new Area {
      val usableSliceRange = if(withBuffer) eid until scannerSlices else eid to eid //Can be tweek to generate smaller designs
      val first = if(withBuffer) Bool(eid == 0) else slices.mask.takeLow(usableSliceRange.low) === 0
      val usableMask = usableSliceRange.map(sid => scanners(sid).valid && !usedMask(eid)(sid)).asBits
      val slicesOh = OHMasking.firstV2(usableMask)
      val redo = MuxOH.or(slicesOh, usableSliceRange.map(sid => scanners(sid).redo), true)
      val localMask = MuxOH.or(slicesOh, usableSliceRange.map(sid => scanners(sid).checker.map(_.required).asBits()), true)
      val usageMask = MuxOH.or(slicesOh, usableSliceRange.map(sid => scanners(sid).usageMask), true)
      usedMask(eid+1) := usedMask(eid) | usageMask

      val valid = slicesOh.orR
      val ctx = slices.readCtx(usableSliceRange, slicesOh, usageMask)

      when(api.haltIt || api.singleFetch && !first) {
        valid := False
        redo := False
        if(!withBuffer) usageMask := 0
      }
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
        lane.up(lane.LANE_SEL) := valid

        lane.up(Decode.INSTRUCTION) := extractor.ctx.instruction
        lane.up(Decode.DECOMPRESSION_FAULT) := False
        lane.up(Decode.INSTRUCTION_RAW) := extractor.ctx.instruction
        val isRvc = extractor.ctx.instruction(1 downto 0) =/= 3
        val withRvc = Riscv.RVC.get generate new Area {
          val dec = RvcDecompressor(extractor.ctx.instruction, rvf = Riscv.RVF, rvd = Riscv.RVD, Riscv.XLEN)
          when(isRvc) {
            lane.up(Decode.INSTRUCTION) := dec.inst
            lane.up(Decode.DECOMPRESSION_FAULT) := dec.illegal
          }
        }
        when(isRvc) {
          lane.up(Decode.INSTRUCTION_RAW)(Decode.INSTRUCTION_WIDTH - 1 downto 16) := 0 //To match spike
        }

        lane.up(Decode.INSTRUCTION_SLICE_COUNT) := OHToUInt(OHMasking.lastV2(extractor.localMask))
        lane.up(Global.PC)              := extractor.ctx.pc
        lane.up(Decode.DOP_ID)          := (laneId match {
          case 0 => harts.map(_.dopId).read(downNode(Global.HART_ID))
          case _ => downCtrl.lane(laneId-1)(Decode.DOP_ID) + downCtrl.lane(laneId-1).isValid.asUInt
        })
        for (e <- firstSliceData ++ lastSliceData) lane.up(e).assignFrom(extractor.ctx.hm.apply(e))
        lane.up(Global.TRAP) := extractor.ctx.trap

        val onBtb = withBtb generate new Area{
          val pcLastSlice = lane.up(Global.PC)(Fetch.SLICE_RANGE) + lane.up(Decode.INSTRUCTION_SLICE_COUNT)
//          val afterPrediction = pcLastSlice > lane.up(Prediction.WORD_JUMP_SLICE)
          //TODO !!! take care of : what's about the prediction landed on a slice which map nobody ?
          val didPrediction = /*!afterPrediction && */ pcLastSlice >= lane.up(Prediction.WORD_JUMP_SLICE)
          lane.up(Prediction.ALIGNED_JUMPED) := lane.up(Prediction.WORD_JUMPED) && didPrediction
          lane.up(Prediction.ALIGNED_JUMPED_PC) := lane.up(Prediction.WORD_JUMP_PC)
//          lane.up(lane.LANE_SEL) clearWhen(lane.up(Prediction.WORD_JUMPED) && afterPrediction)
          lane.up(Prediction.ALIGNED_SLICES_BRANCH) := lane.up(Prediction.WORD_SLICES_BRANCH)
          lane.up(Prediction.ALIGNED_SLICES_TAKEN) := lane.up(Prediction.WORD_SLICES_TAKEN)
          lane.up(Prediction.ALIGN_REDO) := extractor.redo
        }
      }

      downNode.valid := lanes.map(_.valid).orR
    }



    val buffer = withBuffer generate new Area {
      val bufferedSlices = Fetch.WORD_WIDTH/Fetch.SLICE_WIDTH
      val data = Reg(Fetch.WORD)
      val mask = Reg(FETCH_MASK) init (0)
      val last = Reg(FETCH_MASK) init (0)
      val pc = Reg(Fetch.WORD_PC())
      val trap = Reg(Bool()) init (False)
      val hm = Reg(HardMap(hmElements))

      slices.data.assignFromBits(up(Fetch.WORD) ## data)
      slices.mask := up(FETCH_MASK) ## mask
      slices.last := up(FETCH_LAST) ## last

      when(!up.valid){
        for(e <- extractors) when(e.usageMask(scannerSlices-1 downto Fetch.SLICE_COUNT) =/= 0){
          e.valid := False
        }
      }

      val downFire = downNode.isReady || downNode.isCancel
      val usedMask = extractors.map(e => e.usageMask.andMask(e.valid)).reduce(_ | _)

      val haltUp = (mask & ~ usedMask.dropHigh(Fetch.SLICE_COUNT).andMask(downFire)).orR || api.haltIt
      up.ready := !up.valid || !haltUp

      when(downFire){
        mask := mask & ~usedMask.takeLow(Fetch.SLICE_COUNT)
        last := last & ~usedMask.takeLow(Fetch.SLICE_COUNT)
      }
      when(up.isValid && up.isReady && !up.isCancel){
        data := up(Fetch.WORD)
        mask := up(FETCH_MASK) & ~usedMask.takeHigh(Fetch.SLICE_COUNT).andMask(downFire)
        trap := up(Global.TRAP)
        pc   := up(Fetch.WORD_PC)
        last := up(FETCH_LAST)
        for(e <- hmElements) hm(e).assignFrom(up(e))
      }

      //TODO improve the flush condition ?
      val age = Ages.DECODE - Ages.STAGE
      val flushIt = host[ReschedulePlugin].isFlushedAt(age, U(0), U(0)).getOrElse(False)
      when(flushIt/* && !(downNode.isValid && !downNode.isReady)*/) {
        mask := 0
        last := 0
      }

      val readers = for(spec <- slices.readCtxs) yield new Area{
        val idsBuffer = spec.first.filter(_.sid < bufferedSlices)
        val idsUp = spec.first.filter(_.sid >= bufferedSlices)
        val firstFromBuffer = idsBuffer.map(e => e.oh).orR
        val lastFromBuffer = idsUp.map(e => spec.usage(e.sid)).norR
        spec.ctx.instruction := OhMux.or(spec.first.map(_.oh).asBits, spec.first.map(e => slicesInstructions(e.sid)), true)
        spec.ctx.pc := firstFromBuffer.mux(pc, up(Fetch.WORD_PC))
        spec.ctx.pc(Fetch.SLICE_RANGE) := OHToUInt((0 until Fetch.SLICE_COUNT).map(off => spec.first.filter(_.sid % Fetch.SLICE_COUNT == off).map(_.oh).orR))
        spec.ctx.trap := firstFromBuffer && trap || !lastFromBuffer && up(Global.TRAP)
        for (e <- firstSliceData) spec.ctx.hm(e).assignFrom(firstFromBuffer.mux(hm(e), up(e)))
        for (e <- lastSliceData) spec.ctx.hm(e).assignFrom(lastFromBuffer.mux(hm(e), up(e)))
      }
    }

    val nobuffer = !withBuffer generate new Area {
      assert(!Riscv.RVC)
      assert(Decode.INSTRUCTION_WIDTH.get*Decode.LANES == Fetch.WORD_WIDTH.get)
      val mask = Reg(FETCH_MASK) init ((1 << Decode.LANES)-1)

      val remaningMask = mask & ~usedMask.last
      when(downNode.isValid && downNode.isReady) {
        mask := remaningMask
      }

      val age = Ages.DECODE - Ages.STAGE
      val flushIt = host[ReschedulePlugin].isFlushedAt(age, U(0), U(0)).getOrElse(False)
      when(flushIt || !up.isValid || up.isReady) {
        mask := (1 << Decode.LANES)-1
      }

      slices.data.assignFromBits(up(Fetch.WORD))
      slices.mask := up(FETCH_MASK).andMask(up.valid) & (Decode.LANES.get > 1).mux(mask, mask.getAllTrue)
      slices.last := 0

      up.ready := !up.valid || downNode.isReady && !api.haltIt && remaningMask === 0

      val readers = for ((spec, sid) <- slices.readCtxs.zipWithIndex) yield new Area {
        assert(spec.first.size == 1 && spec.first.head.sid == sid)
        spec.ctx.instruction := slicesInstructions(sid)
        spec.ctx.pc := up(Fetch.WORD_PC)
        spec.ctx.pc(Fetch.SLICE_RANGE) := sid
        spec.ctx.trap := up(Global.TRAP)
        for (e <- firstSliceData) spec.ctx.hm(e).assignFrom(up(e))
        for (e <- lastSliceData) spec.ctx.hm(e).assignFrom(up(e))
      }
    }

    injectRetainer.await()
    val injectLogic = for (port <- injectPorts) yield new Area {
      val ext = extractors.last
      val rvc = port.payload(1 downto 0) =/= 3
      when(port.valid) {
        ext.valid := True
        ext.redo := False
        ext.ctx.trap := False
        ext.ctx.instruction := port.payload
        if (withBtb) {
          ext.ctx.hm(Prediction.WORD_JUMPED) := False
        }
      }
    }

    api.downMoving := downNode.isMoving
    buildBefore.release()
  }
}
