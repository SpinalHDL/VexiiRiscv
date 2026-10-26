package vexiiriscv.test

import spinal.lib.bus.misc.{AddressMapping, SizeMapping}

import scala.collection.mutable

trait EmulatedDevice {
  def access(write: Boolean, address: BigInt, data: Array[Byte]): Boolean
}

case class IoDeviceManager() {
  case class DeviceEntry (mapping: AddressMapping, device: EmulatedDevice)

  val devices = mutable.ArrayBuffer[DeviceEntry]()

  def registerDevice(mapping: AddressMapping, device: EmulatedDevice) =
    devices += DeviceEntry(mapping, device)

  private def findEntry(address: Long): Option[DeviceEntry] = devices.find(_.mapping.hit(address))

  def find(address: Long): Option[EmulatedDevice] = findEntry(address).map(_.device)

  def hit(address: Long): Boolean = find(address).nonEmpty

  def access(write: Boolean, address: Long, data: Array[Byte]): Boolean =
    findEntry(address).map(e => e.device.access(write, address - e.mapping.lowerBound, data)).getOrElse(true)
}
