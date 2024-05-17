package spinal.lib.misc.pipeline

import spinal.core._

trait NodeLaneApi{
  def nodeLink: Node
  def laneName: String
  def LANE_SEL: Payload[Bool] = CtrlLaneApi.LANE_SEL

  private val _c = nodeLink

  def isValid: Bool = _c.isValid
  def isReady : Bool = _c.isReady
  def isCancel : Bool = _c.isCancel

  def apply[T <: Data](that: Payload[T]): T = _c.apply(that, laneName)
  def apply[T <: Data](that: Payload[T], subKey : Any): T = _c.apply(that, laneName + "_" + subKey.toString)
  def insert[T <: Data](that: T): Payload[T] = {
    val p = Payload(that)
    apply(p) := that
    p
  }


  abstract class NodeMirror(node : Node) extends NodeBaseApi {
    override def valid = node(LANE_SEL, laneName)
    override def ready = node.ready
    override def cancel = node.cancel
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


  implicit def stageablePiped2[T <: Data](stageable: Payload[T]): T = this (stageable)
  implicit def bundlePimper[T <: Bundle](stageable: Payload[T]): BundlePimper[T] = new BundlePimper[T](this (stageable))
  class BundlePimper[T <: Bundle](pimped: T) {
    def :=(that: T): Unit = pimped := that
  }

  class Area(from : NodeLaneApi = this) extends NodeLaneMirror(from)
}

class NodeLaneMirror(from : NodeLaneApi) extends spinal.core.Area with NodeLaneApi {
  override def nodeLink: Node = from.nodeLink
  override def laneName: String = from.laneName
  override def LANE_SEL: Payload[Bool] = from.LANE_SEL
}