package spinal.lib.misc.pipeline

import spinal.core._

trait CtrlLaneApi{
  def ctrlLink: CtrlLink
  def laneName: String
  def LANE_SEL: Payload[Bool]

  private val _c = ctrlLink

  def isValid: Bool = up(LANE_SEL)
  def isReady : Bool = _c.isReady
  def isFiring: Bool = isValid && _c.up.isReady && !hasCancelRequest
  def isMoving: Bool = isValid && (_c.up.isReady || hasCancelRequest)
  def isCanceling: Bool = isValid && hasCancelRequest
  def hasCancelRequest : Bool

  def apply[T <: Data](that: Payload[T]): T = _c.apply(that, laneName)
  def apply[T <: Data](that: Payload[T], subKey : Any): T = _c.apply(that, laneName + "_" + subKey.toString)
  def insert[T <: Data](that: T): Payload[T] = {
    val p = Payload(that)
    apply(p) := that
    p
  }
  def bypass[T <: Data](that: Payload[T]): T =  _c.bypass(that, laneName)

  def up = {
    val up = _c.up
    new up.Area(laneName)
  }
  def down = {
    val down = _c.down
    new down.Area(laneName)
  }

  implicit def stageablePiped2[T <: Data](stageable: Payload[T]): T = this (stageable)
  implicit def bundlePimper[T <: Bundle](stageable: Payload[T]) = new BundlePimper[T](this (stageable))
  class BundlePimper[T <: Bundle](pimped: T) {
    def :=(that: T): Unit = pimped := that
  }

  class Area(from : CtrlLaneApi = this) extends CtrlLaneMirror(from)
}

class CtrlLaneMirror(from : CtrlLaneApi) extends spinal.core.Area with CtrlLaneApi {
  override def ctrlLink: CtrlLink = from.ctrlLink
  override def laneName: String = from.laneName
  override def LANE_SEL: Payload[Bool] = from.LANE_SEL
  override def hasCancelRequest: Bool = from.hasCancelRequest
}