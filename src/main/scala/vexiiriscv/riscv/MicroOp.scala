package vexiiriscv.riscv

import spinal.core.{AreaObject, MaskedLiteral, Nameable}
import spinal.lib.logic.Masked

class Resource
case class RfResource(rf : RegfileSpec, access : RfAccess) extends Resource

class RfAccess extends Nameable
class RfRead extends RfAccess
class RfWrite extends RfAccess

object RS1 extends RfRead with AreaObject
object RS2 extends RfRead with AreaObject
object RS3 extends RfRead with AreaObject
object RD  extends RfWrite with AreaObject
object PC_READ  extends Resource with AreaObject
object INSTRUCTION_SIZE  extends Resource with AreaObject
object LQ  extends Resource with AreaObject
object SQ  extends Resource with AreaObject
object FPU extends Resource with AreaObject
object RM  extends Resource with AreaObject

abstract class MicroOp(val resources : Seq[Resource]){
  def keys : Seq[MaskedLiteral]
  def keysMasked = keys.map(Masked.apply)
}
object SingleDecoding{
  def apply(key : MaskedLiteral, resources : Seq[Resource]): SingleDecoding = {
    SingleDecoding(List(key), resources)
  }
}

case class SingleDecoding(keys : Seq[MaskedLiteral], override val resources : Seq[Resource]) extends MicroOp(resources) with Nameable {
  override def toString = s"SingleDecoding ${getName("")} $keys"
}
case class MultiDecoding(key : MaskedLiteral, uop : Seq[MicroOp])

trait RegfileSpec extends Nameable{
  def sizeArch : Int
  def width : Int
  def x0AlwaysZero : Boolean
  def getName() : String
  def initialValue : BigInt

  def ->(access : RfAccess) = RfResource(this, access)
}

