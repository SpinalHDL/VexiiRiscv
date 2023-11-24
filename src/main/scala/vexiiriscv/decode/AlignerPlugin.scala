package vexiiriscv.decode

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{CtrlLink, Link}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.misc.PipelineService
import vexiiriscv.riscv.{INSTRUCTION_SIZE, Riscv}

import scala.collection.mutable.ArrayBuffer

class AlignerPlugin(fetchAt : Int = 3,
                    lanes : Int = 1) extends FiberPlugin with PipelineService{
  lazy val fpp = host[FetchPipelinePlugin]
  lazy val dpp = host[DecodePipelinePlugin]

  buildBefore(fpp.elaborationLock)
  buildBefore(dpp.elaborationLock)

  override def getLinks(): Seq[Link] = logic.connectors

  val logic = during build new Area{
    val connectors = ArrayBuffer[Link]()
    Decode.LANES.set(lanes)
    Decode.INSTRUCTION_WIDTH.get


    assert(Decode.INSTRUCTION_WIDTH.get*Decode.LANES == Fetch.WORD_WIDTH.get)
    assert(!Riscv.RVC)
    assert(isPow2(Decode.LANES.get))



    val up = fpp.ctrl(fetchAt).down
    val down = dpp.up
    val connector = CtrlLink(up, down)
    connectors += connector



    val feeder = new down.Area{
      val harts = for (hartId <- Global.hartsIds) yield new Area {
        val dopId = Reg(Decode.DOP_ID) init (0)
        when(isFiring && up(Global.HART_ID) === hartId) {
          dopId := down(Decode.DOP_ID, Decode.LANES-1) + 1
        }
      }

      val instructionSlices = Fetch.WORD.subdivideIn(Decode.LANES.get slices)
      val lane = for(laneId <- Decode.laneIds) new Area{
        val lane = new down.Area(laneId)
        val pcLaneLow = log2Up(Decode.INSTRUCTION_WIDTH/8)
        val pcLaneRange = pcLaneLow + log2Up(Decode.LANES) -1 downto pcLaneLow
        lane(Decode.ALIGNED_MASK)    := Fetch.WORD_PC(pcLaneRange) >= laneId
        lane(Decode.INSTRUCTION)     := instructionSlices(laneId)
        lane(Global.PC)              := Fetch.WORD_PC
        lane(Global.PC)(pcLaneRange) := laneId
        lane(Fetch.ID)               := Fetch.ID
        lane(Decode.DOP_ID)          := (laneId match {
          case 0 => harts.map(_.dopId).read(up(Global.HART_ID))
          case _ => down(Decode.DOP_ID, laneId-1) + down(Decode.ALIGNED_MASK, laneId-1).asUInt
        })
      }

    }
  }
}
