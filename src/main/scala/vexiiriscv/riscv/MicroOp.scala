package vexiiriscv.riscv

import spinal.core.{AreaObject, MaskedLiteral, Nameable}
import spinal.lib.logic.Masked

// Will be used to build a data model of what a micro op needs / does.
abstract class MicroOp(val resources : scala.collection.Seq[Resource]){
  def keys : Seq[MaskedLiteral]
  def keysMasked = keys.map(Masked.apply)
}

// Define one to one instruction to micro op mapping.
// In RISC-V "all" instruction can be considered as one to one with micro op.
// This symplifies things.
// The keys argument specifie what is the RISC-V instruction leading to that micro op.
case class SingleDecoding(keys : Seq[MaskedLiteral], override val resources : scala.collection.Seq[Resource]) extends MicroOp(resources) with Nameable {
  override def toString = s"${getName("")} $keys"
}
object SingleDecoding{
  def apply(key : MaskedLiteral, resources : scala.collection.Seq[Resource]): SingleDecoding = {
    SingleDecoding(List(key), resources)
  }
}

// Here we will define the classes and object which can be used to specify what a micro op needs and produces
// Those will be used to build a data model which can later be processed by some hardware generator.
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


trait RegfileSpec extends Nameable{
  def sizeArch : Int // How many words it has from a RISC-V perspective (not physicaly)
  def width : Int // How many bits per word
  def x0AlwaysZero : Boolean // used by the RISC-V integer register file to hardwire x0
  def getName() : String
  def initialValue : BigInt // For the FPU, it allows to encode NaN as an initial value

  def ->(access : RfAccess) = RfResource(this, access)
}

