package vexiiriscv.decode

import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.misc.pipeline.{CtrlLink, Link}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.misc.PipelineService
import vexiiriscv.prediction.{FetchWordPrediction, Prediction}
import vexiiriscv.riscv.{INSTRUCTION_SIZE, Riscv}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class AlignerPlugin(fetchAt : Int,
                    lanes : Int = 1) extends FiberPlugin with PipelineService{
  lazy val fpp = host[FetchPipelinePlugin]
  lazy val dpp = host[DecodePipelinePlugin]

  buildBefore(fpp.elaborationLock)
  buildBefore(dpp.elaborationLock)

  override def getLinks(): Seq[Link] = logic.connectors

  val lastSliceData = mutable.LinkedHashSet[NamedType[_ <: Data]]()

  val elaborationLock = Lock()
  val logic = during build new Area{
    val connectors = ArrayBuffer[Link]()
    Decode.LANES.set(lanes)
    Decode.INSTRUCTION_WIDTH.get


    assert(Decode.INSTRUCTION_WIDTH.get*Decode.LANES == Fetch.WORD_WIDTH.get)
    assert(!Riscv.RVC)
    assert(isPow2(Decode.LANES.get))

    elaborationLock.await()

    val withBtb = host.get[FetchWordPrediction].nonEmpty

    val up = fpp.fetch(fetchAt).down
    val downCtrl = dpp.ctrl(0)
    val downNode = downCtrl.link.up
    val connector = CtrlLink(up, downNode)
    connectors += connector



    val feeder = new Area{
      val harts = for (hartId <- Global.hartsIds) yield new Area {
        val dopId = Reg(Decode.DOP_ID) init (0)
        when(downNode.isFiring && up(Global.HART_ID) === hartId) {
          dopId := downCtrl.lane(Decode.LANES-1)(Decode.DOP_ID) + 1
        }
      }

      val instructionSlices = up(Fetch.WORD).subdivideIn(Decode.LANES.get slices)
      val lane = for(laneId <- Decode.laneIds) new Area{
        val lane = downCtrl.lane(laneId)
        val pcLaneLow = log2Up(Decode.INSTRUCTION_WIDTH/8)
        val pcLaneRange = pcLaneLow + log2Up(Decode.LANES) -1 downto pcLaneLow

        lane.up(lane.LANE_SEL)       := up.valid && up(Fetch.WORD_PC)(pcLaneRange) <= laneId
        lane(Decode.INSTRUCTION)     := instructionSlices(laneId)
        lane(Global.PC)              := up(Fetch.WORD_PC)
        lane(Global.PC)(pcLaneRange) := laneId
        lane(Fetch.ID)               := up(Fetch.ID)
        lane(Decode.DOP_ID)          := (laneId match {
          case 0 => harts.map(_.dopId).read(up(Global.HART_ID))
          case _ => downCtrl.lane(laneId-1)(Decode.DOP_ID) + downCtrl.lane(laneId-1).isValid.asUInt
        })
        lane(Prediction.BRANCH_HISTORY) := up(Prediction.BRANCH_HISTORY)
        for(e <- lastSliceData) lane(e).assignFrom(up.apply(e))

        val onBtb = withBtb generate new Area{
          assert(!Riscv.RVC)
          val afterPrediction = lane(Global.PC)(Fetch.SLICE_RANGE) > up(Prediction.WORD_JUMP_SLICE)
          val didPrediction = !afterPrediction && lane(Global.PC)(Fetch.SLICE_RANGE) + lane(Decode.INSTRUCTION_SLICE_COUNT) >= up(Prediction.WORD_JUMP_SLICE) //TODO take care of : what's about the prediction landed on a slice which map nobody ?
          lane(Prediction.ALIGNED_JUMPED) := up(Prediction.WORD_JUMPED) && didPrediction
          lane(Prediction.ALIGNED_JUMPED_PC) := up(Prediction.WORD_JUMP_PC)
          lane.up(lane.LANE_SEL) clearWhen(up(Prediction.WORD_JUMPED) && afterPrediction)
          lane(Prediction.ALIGNED_SLICES_BRANCH) := up(Prediction.WORD_SLICES_BRANCH)
          lane(Prediction.ALIGNED_SLICES_TAKEN) := up(Prediction.WORD_SLICES_TAKEN)
        }
      }
    }
  }
}
