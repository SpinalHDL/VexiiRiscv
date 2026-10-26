package vexiiriscv.test

import spinal.core.{ClockDomain, UInt}
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.aia.{ImsicFileInfo, ImsicMapping, ImsicTrigger}
import spinal.lib.sim.StreamDriver

import scala.collection.mutable

case class ImsicSimFile(base: BigInt, info: ImsicFileInfo, trigger: Stream[UInt], cd: ClockDomain) {
  val (driver, queue) = StreamDriver.queue(trigger, cd)
  driver.transactionDelay = () => 0
  driver.delay = 0

  def enqueue(bytes: Array[Byte]): Unit = {
    val bytesCopy = bytes.clone()
    queue.enqueue((payload: UInt) => payload #= bytesCopy)
  }
}

class ImsicPeripheralEmulator(val localMapping: ImsicMapping, val files: Seq[ImsicSimFile]) extends EmulatedDevice {
  import ImsicPeripheralEmulator._

  def mappedSize: BigInt = localMapping.interruptFileHartSize
  def addressMapping: SizeMapping = SizeMapping(files.head.base, mappedSize)

  val registerSize = 4
  val IP_LE = 0x0L
  val IP_BE = 0x4L

  override def access(write: Boolean, address: BigInt, data: Array[Byte]): Boolean = {
    if (address % registerSize != 0) return true

    if (!write) {
      java.util.Arrays.fill(data, 0.toByte)
      return false
    }

    if (data.length != registerSize) return true

    val pageId = (address / ImsicTrigger.interruptFileSize).toInt
    val pageOffset = (address % ImsicTrigger.interruptFileSize).toLong

    files.lift(pageId).foreach { file =>
      pageOffset match {
        case IP_LE => file.enqueue(data)
        case IP_BE => file.enqueue(data.reverse)
        case _ =>
      }
    }

    false
  }
}

object ImsicPeripheralEmulator {
  val machineLayout = new {
    val base = 0x24000000L
    val mapping = ImsicMapping(
      interruptFileHartSize = ImsicTrigger.interruptFileSize,
      interruptFileHartOffset = 0,
      interruptFileGroupSize = 0,
    )
  }
  val supervisorLayout = new {
    val base = 0x28000000L
    val mapping = ImsicMapping(
      interruptFileHartSize = 64 * ImsicTrigger.interruptFileSize,
      interruptFileHartOffset = 0,
      interruptFileGroupSize = 0,
    )
  }

  def build(base: BigInt, mapping: ImsicMapping, bindings: Seq[(ImsicFileInfo, Stream[UInt])], cd: ClockDomain): Seq[ImsicPeripheralEmulator] = {
    val infos = bindings.map(_._1)
    val calibratedPhysicalMapping = ImsicTrigger.mappingCalibrate(mapping, infos)
    val harts = bindings.groupBy(_._1.hartId).values.map(_.sortBy(_._1.guestId)).toSeq
      .sortBy(hart => ImsicTrigger.imsicGroupHartOffset(calibratedPhysicalMapping, hart.head._1))

    harts.map { hart =>
      val hartMapping = ImsicTrigger.mappingCalibrate(ImsicMapping(), hart.last._1.guestId, maxGroupHartId = 0, maxGroupId = 0)
      val files = hart.map { case (info, trigger) =>
        val offset = base + ImsicTrigger.imsicOffset(calibratedPhysicalMapping, info)
        ImsicSimFile(offset, info, trigger, cd)
      }
      new ImsicPeripheralEmulator(hartMapping, files)
    }
  }
}
