package vexiiriscv.schedule

import spinal.core._
import spinal.core.fiber.{Retainer, Lockable}
import spinal.lib.Flow
import spinal.lib.misc.pipeline.CtrlLink
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute.Execute
import vexiiriscv.fetch.JumpCmd

/**
 * This contains the integer constant which allows to compute identifier for a given point in the pipeline.
 * This id can then be used with the ReschedulePlugin to querry/register flushes
 *
 * So for instance, if you want to get the age id for the execute stage 3, then you do EXECUTE + 3*STAGE + NOT_PREDICTION
 */
object Ages {
  val STAGE = 10
  val NOT_PREDICTION = 1
  val FETCH = 0
  val DECODE = 1000
  val EXECUTE = 2000
  val TRAP = 3000
}

case class FlushCmd(age : Int, laneAgeWidth : Int, withUopId : Boolean) extends Bundle{
  val hartId = Global.HART_ID()
  val uopId = withUopId generate Decode.UOP_ID() //Used for debugging/tracking
  val laneAge = UInt(laneAgeWidth bits) //Used to know the order between lanes of the same pipeline (decode/execute lanes)
  val self = Bool() //True if the flush source is killing itself
}

/**
 * Provide an API to create new interfaces to flush the CPU pipelines.
 * The "age" can be generated from the Ages object.
 */
trait ScheduleService {
  def newFlushPort(age: Int, laneAgeWidth: Int, withUopId: Boolean): Flow[FlushCmd]
  def isFlushedAt(age: Int, hartId : UInt, laneAge : UInt): Option[Bool] // Querry if the given place in the CPU is being flushed
  val elaborationLock = Retainer()
}


