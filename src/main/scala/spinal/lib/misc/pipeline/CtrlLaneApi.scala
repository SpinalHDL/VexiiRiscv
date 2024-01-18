package spinal.lib.misc.pipeline

import spinal.core._

object CtrlLaneApi extends AreaRoot{
  val LANE_SEL = Payload(Bool())
}

trait CtrlLaneApi{
  def ctrlLink: CtrlLink
  def laneName: String
  def LANE_SEL: Payload[Bool] = CtrlLaneApi.LANE_SEL

  private val _c = ctrlLink

  def isValid: Bool = up(LANE_SEL)
  def isReady : Bool = _c.isReady
  def isCancel : Bool = upIsCancel

  def upIsCancel : Bool
  def downIsCancel : Bool

  def apply[T <: Data](that: Payload[T]): T = _c.apply(that, laneName)
  def apply[T <: Data](that: Payload[T], subKey : Any): T = _c.apply(that, laneName + "_" + subKey.toString)
  def insert[T <: Data](that: T): Payload[T] = {
    val p = Payload(that)
    apply(p) := that
    p
  }
  def bypass[T <: Data](that: Payload[T]): T =  _c.bypass(that, laneName)


  abstract class NodeMirror(node : Node) extends NodeBaseApi {
    override def valid = node(LANE_SEL, laneName)
    override def ready = node.ready
    override def cancel = ??? //node.cancel //TODO not that great ?-
    override def isValid: Bool = node(LANE_SEL, laneName)
    override def isReady: Bool = node.isReady
    override def isFiring = valid && isReady && !isCancel
    override def isMoving = valid && (isReady || isCancel)
//    override def isCancel: Bool = node.isCancel
    override def isCanceling = valid && isCancel
    override def apply(key: NamedTypeKey) = ???
    override def apply[T <: Data](key: Payload[T]) = node(key, laneName)
    override def apply(subKey: Seq[Any]) = ???
    def transactionSpawn = valid && !RegNext(valid, False).clearWhen(isReady || isCancel)
  }

  def up = new NodeMirror(_c.up){
    override def isCancel: Bool = upIsCancel
  }
  def down = new NodeMirror(_c.down){
    override def isCancel: Bool = downIsCancel
  }

  implicit def stageablePiped2[T <: Data](stageable: Payload[T]): T = this (stageable)
  implicit def bundlePimper[T <: Bundle](stageable: Payload[T]): BundlePimper[T] = new BundlePimper[T](this (stageable))
  class BundlePimper[T <: Bundle](pimped: T) {
    def :=(that: T): Unit = pimped := that
  }

  class Area(from : CtrlLaneApi = this) extends CtrlLaneMirror(from)
}

class CtrlLaneMirror(from : CtrlLaneApi) extends spinal.core.Area with CtrlLaneApi {
  override def ctrlLink: CtrlLink = from.ctrlLink
  override def laneName: String = from.laneName
  override def LANE_SEL: Payload[Bool] = from.LANE_SEL
  override def upIsCancel: Bool = from.upIsCancel
  override def downIsCancel: Bool = from.downIsCancel
}