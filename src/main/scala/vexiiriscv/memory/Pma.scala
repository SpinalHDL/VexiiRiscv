package vexiiriscv.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping
import spinal.lib.bus.tilelink.M2sTransfers
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.system.tag.{MappedTransfers, PmaRegion}
import vexiiriscv.Global

import scala.collection.mutable.ArrayBuffer


trait PmaOp
object PmaLoad extends PmaOp
object PmaStore extends PmaOp

class PmaCmd(addressWidth : Int, sizes : Seq[Int], ops : Seq[PmaOp]) extends Bundle{
  val address = UInt(addressWidth bits)
  val size = Bits(log2Up(sizes.size) bits)
  val op = Bits(log2Up(ops.size) bits)
}

class PmaRsp() extends Bundle{
  val fault = Bool()
  val io = Bool()
}

case class PmaPort(addressWidth : Int, sizes : Seq[Int], ops : Seq[PmaOp]) extends Bundle{
  val cmd = new PmaCmd(addressWidth, sizes, ops)
  val rsp = new PmaRsp()
}


class PmaLogic(port : PmaPort, regions : Seq[PmaRegion]) extends Area{
  import port._
  val hitsTerms = ArrayBuffer[Masked]()
  val mainSpec = new DecodingSpec(Bool()).setDefault(Masked.zero)
  val executableSpec = new DecodingSpec(Bool()).setDefault(Masked.zero)

  val addressBits = cmd.address.asBits
  val argsBits = cmd.size ## cmd.op
  val argsWidth = widthOf(argsBits)
  val argsMask = BigInt((1 << argsWidth)-1)
  def opMask(opId: Int, sizeId: Int) = Masked(opId | (sizeId << widthOf(cmd.op)), argsMask)


  val onRegion = for (region <- regions) yield new Area {
    val regionTerms = AddressMapping.terms(region.mapping, addressWidth)
    hitsTerms ++= regionTerms
    if (region.isMain) mainSpec.addNeeds(regionTerms, Masked.one)
    if(region.isExecutable) executableSpec.addNeeds(regionTerms, Masked.one)
  }

  val byTransfers = regions.groupBy(_.transfers)
  val onTransfers = for ((transfer, regions) <- byTransfers) yield new Area{
    val terms = ArrayBuffer[Masked]()
    val addressSpec = new DecodingSpec(Bool()).setDefault(Masked.zero)
    for (region <- regions) terms ++= AddressMapping.terms(region.mapping, addressWidth)
    addressSpec.addNeeds(terms, Masked.one)
    val addressHit = addressSpec.build(addressBits, hitsTerms)

    val argsOk, argsKo = ArrayBuffer[Masked]()
    for((size, sizeId) <- sizes.zipWithIndex){
      for((op, opId) <- ops.zipWithIndex){
        val mask = opMask(opId, sizeId)
        val ok = op match {
          case PmaLoad => transfer match {
            case t: M2sTransfers => t.get.contains(size) || t.acquireB.contains(size)
          }
          case PmaStore => transfer match {
            case t: M2sTransfers => t.putFull.contains(size) || t.acquireT.contains(size)
          }
        }
        if(ok) argsOk += mask else argsKo += mask
      }
    }
    val argsHit = Symplify(argsBits, argsOk, argsKo)

    val hit = argsHit && addressHit
  }


  port.rsp.fault := !(Symplify(addressBits, hitsTerms) && onTransfers.map(_.hit).orR)
  port.rsp.io := !mainSpec.build(addressBits, hitsTerms)
}
