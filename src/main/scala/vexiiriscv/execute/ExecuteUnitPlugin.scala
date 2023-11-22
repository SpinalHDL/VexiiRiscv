package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.decode.Decode
import vexiiriscv.misc.{CtrlPipelinePlugin, PipelineService}
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{MicroOp, RD, RegfileSpec, RfAccess, RfRead, RfResource}
import vexiiriscv.schedule.DispatchPlugin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ExecuteUnitPlugin(val euId : String,
                        val priority : Int,
                        override val rfReadAt : Int,
                        val decodeAt : Int,
                        override val executeAt : Int) extends FiberPlugin with PipelineService with ExecuteUnitService {
  withPrefix(euId)

  during setup {
    host.list[RegfileService].foreach(_.retain())
  }

  override def euName(): String = euId
  override def insertNode: Node = ctrl(logic.idMin).up
  override def nodeAt(id : Int): Node = ctrl(id).down
  override def dispatchPriority: Int = priority
  override def getMicroOp(): Seq[MicroOp] = {
    lock.await()
    microOps.keys.toSeq
  }
  override def getMicroOpSpecs(): Iterable[MicroOpSpec] = microOps.values

  val microOps = mutable.LinkedHashMap[MicroOp, MicroOpSpec]()
  def addMicroOp(op : MicroOp): Unit = {
    microOps.getOrElseUpdate(op, new MicroOpSpec(op))
  }

  def setRdSpec(op : MicroOp, data : Payload[Bits], rfReadableAt : Int, bypassesAt : Seq[Int]): Unit = {
    assert(microOps(op).rd.isEmpty)
    microOps(op).rd = Some(RdSpec(data, rfReadableAt + executeAt, bypassesAt.map(_ + executeAt)))
  }

  override def getSpec(op: MicroOp): MicroOpSpec = microOps(op)

  def setDecodingDefault(key: Payload[_ <: BaseType], value: BaseType): Unit = {
    getDecodingSpec(key).setDefault(Masked(value))
  }

  def addDecoding(microOp: MicroOp, values: Seq[(Payload[_ <: BaseType], Any)]): Unit = {
    val op = Masked(microOp.key)
    for ((key, value) <- values) {
      getDecodingSpec(key).addNeeds(op, Masked(value))
    }
  }

  def addDecoding(microOp: MicroOp, head : (Payload[_ <: BaseType], Any), tail : (Payload[_ <: BaseType], Any)*): Unit = {
    addDecoding(microOp, head :: tail.toList)
  }

  def getDecodingSpec(key: Payload[_ <: BaseType]) = decodingSpecs.getOrElseUpdate(key, new DecodingSpec(key))
  val decodingSpecs = mutable.LinkedHashMap[Payload[_ <: BaseType], DecodingSpec[_ <: BaseType]]()

  val rfStageables = mutable.LinkedHashMap[RfResource, Payload[Bits]]()

  def apply(rf: RegfileSpec, access: RfAccess) = getStageable(rf -> access)
  def apply(r: RfResource) = getStageable(r)
  def getStageable(r: RfResource): Payload[Bits] = {
    rfStageables.getOrElseUpdate(r, Payload(Bits(r.rf.width bits)).setName(s"${r.rf.getName()}_${r.access.getName()}"))
  }


  val pipelineLock = new Lockable(){}
  override def getConnectors(): Seq[Link] = pipeline.connectors
  val idToCtrl = mutable.LinkedHashMap[Int, CtrlLink]()
  def ctrl(id : Int)  : CtrlLink = {
    idToCtrl.getOrElseUpdate(id, CtrlLink().setCompositeName(this, if(id >= executeAt) "exe" + (id - executeAt) else "dis" + id))
  }
  def execute(id: Int) : CtrlLink = {
    assert(id >= 0)
    ctrl(id + executeAt)
  }
  val logic = during build new Area{
    pipelineLock.await()

    val specs = microOps.values
    val resources = specs.flatMap(_.op.resources).distinctLinked

    val rf = new Area{
      val rfSpecs = rfStageables.keys.map(_.rf).distinctLinked
      val rfPlugins = rfSpecs.map(spec => host.find[RegfileService](_.rfSpec == spec))
      val readLatencyMax = rfPlugins.map(_.readLatency).max
      val readCtrl = ctrl(rfReadAt)

      val reads = for((spec, payload) <- rfStageables) yield new Area{
        val rfa = Decode.rfaKeys.get(spec.access)
        val rfPlugin = host.find[RegfileService](_.rfSpec == spec.rf)
        val port = rfPlugin.newRead(false)
        port.valid := readCtrl.isValid && readCtrl(rfa.ENABLE) && rfa.is(spec.rf, readCtrl(rfa.RFID))
        port.address := readCtrl(rfa.PHYS)

        case class BypassSpec(eu : ExecuteUnitService, nodeId : Int, payload : Payload[Bits])
        val bypassSpecs = mutable.LinkedHashSet[BypassSpec]()

        // Fill the bypassSpecs from all the eu's microOp rd spec
        val eus = host.list[ExecuteUnitService]
        for(eu <- eus; ops = eu.getMicroOp();
            op <- ops; opSpec = eu.getSpec(op)){
          eu.lock.await() // Ensure that the eu specification is done
          val sameRf = opSpec.op.resources.exists{
            case RfResource(spec.rf, RD) => true
            case _ => false
          }
          if(sameRf){
            val rd = opSpec.rd.get
            for(nodeId <- rd.bypassesAt){
              val bypassSpec = BypassSpec(eu, nodeId, rd.DATA)
              bypassSpecs += bypassSpec
            }
          }
        }

        val dataCtrl = ctrl(rfReadAt + rfPlugin.readLatency)
        val rfaRd = Decode.rfaKeys.get(RD)
        val bypassSorted = bypassSpecs.toSeq.sortBy(_.nodeId)
        val bypassEnables = Bits(bypassSorted.size + 1 bits)
        for((b, id) <- bypassSorted.zipWithIndex){
          val node = b.eu.nodeAt(b.nodeId)
          bypassEnables(id) := node(rfaRd.ENABLE) && node(rfaRd.PHYS) === dataCtrl(rfa.PHYS) && node(rfaRd.RFID) === dataCtrl(rfa.RFID)
        }
        bypassEnables.msb := True
        val sel = OHMasking.firstV2(bypassEnables)
        dataCtrl(payload) := OHMux.or(sel, bypassSorted.map(b =>  b.eu.nodeAt(b.nodeId)(b.payload)) :+ port.data, true)
      }
    }

    val decodeCtrl = ctrl(decodeAt)
    val decoding = new decodeCtrl.Area {
      val coverAll = getMicroOp().map(e => Masked(e.key))
      for ((key, spec) <- decodingSpecs) {
        key.assignFromBits(spec.build(Decode.MICRO_OP, coverAll).asBits)
      }
    }

    val idMin = (0 +: idToCtrl.keys.toList).min
    host.list[RegfileService].foreach(_.release())
  }

  val pipeline = during build new Area {
    linkLock.await()
    val idMax = (0 +: idToCtrl.keys.toList).max
    for (i <- 0 to idMax) ctrl(i)  //To ensure the creation to all intermediate nodes
    val ctrls = idToCtrl.toList.sortBy(_._1).map(_._2)
    ctrls.last.down.setAlwaysReady()
    val sc = for ((from, to) <- (ctrls, ctrls.tail).zipped) yield new StageLink(from.down, to.up).withoutCollapse()
    val connectors = (sc ++ ctrls).toSeq
  }
}
