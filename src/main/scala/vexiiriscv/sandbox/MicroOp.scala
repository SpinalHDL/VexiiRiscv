//package vexiiriscv.sandbox
//
//import spinal.core._
//
//
//class Resource
//class RegFileKey
//case class RfResource(rf : RegFileKey, access : RfAccess) extends Resource
//
//class RfAccess extends Nameable
//class RfRead extends RfAccess
//class RfWrite extends RfAccess
//
//object RS1 extends RfRead with AreaObject
//object RS2 extends RfRead with AreaObject
//object RS3 extends RfRead with AreaObject
//object RD extends RfWrite with AreaObject
//object IMM extends Resource
//object FUNC extends Resource
//object PC_READ extends Resource
//
//object IntRegFile extends RegFileKey{
//  val RS1 = RfResource(this, vexiiriscv.sandbox.RS1)
//  val RS2 = RfResource(this, vexiiriscv.sandbox.RS2)
//  val RS3 = RfResource(this, vexiiriscv.sandbox.RS3)
//  val RD  = RfResource(this, vexiiriscv.sandbox.RD)
//}
//
//class MicroOp(val resources : Seq[Resource]){
//  def this(head : Resource, tail : Resource *) = {
//    this(head +: tail)
//  }
//  def copyAdd(head : Resource, tail : Resource *) = new MicroOp(
//    (resources :+ head) ++ tail
//  )
//}
//
//case class SimpleDecoding(key : MaskedLiteral, microOps : Seq[Resource]) {
//  override def toString = s"SimpleDecoding $key"
//}
//
//class MappingElement
//case class BitsMapping(from : Range, to : Range) extends MappingElement
//case class ResourceMapper(resource : Resource, mapping : Seq[MappingElement]){
//  def this(resource : Resource, head: MappingElement, tail: MappingElement*) = {
//    this(resource, head +: tail)
//  }
//}
//case class MicroOpMapping(microOp: MicroOp, mappings : Seq[ResourceMapper]){
//  def this(microOp: MicroOp, head: ResourceMapper, tail: ResourceMapper*) = {
//    this(microOp, head +: tail)
//  }
//
//  def this(microOp: MicroOp) = {
//    this(microOp, Nil)
//  }
//}
//
//case class RiscvEncoding(key : MaskedLiteral, mapping : Seq[MicroOpMapping]){
//  def this(key : MaskedLiteral, head: MicroOpMapping, tail: MicroOpMapping*) = {
//    this(key, head +: tail)
//  }
//}
//
//
//object MicroOps {
//  def TypeRRR(resource : Resource*)  = new MicroOp(List(IntRegFile.RD, IntRegFile.RS1, IntRegFile.RS2)      ++ resource)
//  def TypeRRI(resource : Resource*)  = new MicroOp(List(IntRegFile.RD, IntRegFile.RS1,                 IMM) ++ resource)
//  def TypeXRRI(resource : Resource*) = new MicroOp(List(             , IntRegFile.RS1, IntRegFile.RS2, IMM) ++ resource)
//
//  val ALU_R = TypeRRR(FUNC)
//  val ALU_I = TypeRRI(FUNC)
//  val LOAD  = TypeRRI(FUNC)
//  val STORE = TypeXRRI(FUNC)
//
//  val LUI = new MicroOp(
//    IntRegFile.RD,
//    IMM
//  )
//  val AUIPC = new MicroOp(
//    IntRegFile.RD,
//    PC_READ,
//    IMM
//  )
//}
//
//object RiscvEncoding{
//  val IMM_I = new ResourceMapper(IMM, BitsMapping(31 downto 20, 11 downto 0))
//  val IMM_S = new ResourceMapper(IMM, BitsMapping(31 downto 25, 11 downto 5), BitsMapping(11 downto 7, 4 downto 0))
//  val FUNC3 = new ResourceMapper(vexiiriscv.sandbox.FUNC, BitsMapping(14 downto 12, 2 downto 0))
//  val FUNC_ALU = new ResourceMapper(vexiiriscv.sandbox.FUNC, BitsMapping(30 downto 30, 4 downto 4), BitsMapping(14 downto 12, 2 downto 0))
//  def TypeAluR(key: MaskedLiteral) = new RiscvEncoding(key, new MicroOpMapping(MicroOps.ALU_R, FUNC_ALU))
//  def TypeAluI(key: MaskedLiteral) = new RiscvEncoding(key, new MicroOpMapping(MicroOps.ALU_I, IMM_I, FUNC_ALU))
//  def TypeIF(key: MaskedLiteral, microOp: MicroOp) = new RiscvEncoding(key, new MicroOpMapping(microOp, IMM_I, FUNC3))
//  def TypeSF(key: MaskedLiteral, microOp: MicroOp) = new RiscvEncoding(key, new MicroOpMapping(microOp, IMM_S, FUNC3))
//
//  val ADD  = TypeAluR(M"0000000----------000-----0110011")
//  val ADDI = TypeAluI(M"-----------------000-----0010011")
//  val LW   = TypeIF(M"-----------------010-----0000011", MicroOps.LOAD)
//  val SW   = TypeIF(M"-----------------010-----0000011", MicroOps.STORE)
//}
//
//
//object Miaou{
////  add(MicroOps.ALU_R, List(FUNC -> M"0000") , List(Op.ADD, SRC1.RF, SRC2.RF), DecodeList(ALU_CTRL -> ace.ADD_SUB ))
//}
//
