package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Lock, Lockable}
import spinal.idslplugin.Location
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.misc.{CtrlPipelinePlugin, PipelineService}
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{MicroOp, RD, RegfileSpec, RfAccess, RfRead, RfResource}
import vexiiriscv.schedule.{Ages, DispatchPlugin, ReschedulePlugin}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class ExecuteLanePlugin(override val laneName : String,
                        val priority : Int,
                        override val rfReadAt : Int,
                        val decodeAt : Int,
                        override val executeAt : Int) extends FiberPlugin with ExecuteLaneService with CompletionService{
  lazy val eupp = host[ExecutePipelinePlugin]
  setupRetain(eupp.pipelineLock)
  setName("execute_" + laneName)

  during setup {
    host.list[RegfileService].foreach(_.elaborationLock.retain())
  }

  override def dispatchPriority: Int = priority
  override def getMicroOp(): Seq[MicroOp] = {
    uopLock.await()
    microOps.keys.toSeq
  }
  override def getMicroOpSpecs(): Iterable[MicroOpSpec] = {
    uopLock.await()
    microOps.values
  }

  val microOps = mutable.LinkedHashMap[MicroOp, MicroOpSpec]()
  def addMicroOp(op : MicroOp): Unit = {
    microOps.getOrElseUpdate(op, new MicroOpSpec(op))
  }

  def setRdSpec(op : MicroOp, data : Payload[Bits], rfReadableAt : Int, bypassesAt : Seq[Int]): Unit = {
    assert(microOps(op).rd.isEmpty)
    microOps(op).rd = Some(RdSpec(data, rfReadableAt + executeAt, bypassesAt.map(_ + executeAt)))
  }

  def setRdOutOfPip(op: MicroOp): Unit = {
    assert(microOps(op).rd.isEmpty)
    microOps(op).rd = None
  }

  def setCompletion(executeCtrlId: Int, head : MicroOp, tail : MicroOp*): Unit = setCompletion(executeCtrlId, head +: tail)
  def setCompletion(executeCtrlId: Int, uops: Seq[MicroOp]): Unit = {
    uops.foreach(microOps(_).completion = Some(executeCtrlId + executeAt))
  }

  def mayFlushUpTo(executeCtrlId: Int, head: MicroOp, tail: MicroOp*): Unit = mayFlushUpTo(executeCtrlId, head +: tail)
  def mayFlushUpTo(executeCtrlId: Int, uops: Seq[MicroOp]): Unit = {
    uops.foreach(microOps(_).mayFlushUpTo = Some(executeCtrlId + executeAt))
  }

  def dontFlushFrom(executeCtrlId: Int, head: MicroOp, tail: MicroOp*): Unit = dontFlushFrom(executeCtrlId, head +: tail)
  def dontFlushFrom(executeCtrlId: Int, uops: Seq[MicroOp]): Unit = {
    uops.foreach(microOps(_).dontFlushFrom = Some(executeCtrlId + executeAt))
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

  val idToCtrl = mutable.LinkedHashMap[Int, CtrlLaneApiImpl]()

  class CtrlLaneApiImpl(ctrlId : Int) extends Area with CtrlLaneApi{
    val cancel = Bool()
    override def ctrlLink: CtrlLink = eupp.ctrl(ctrlId)
    override def laneName: String = ExecuteLanePlugin.this.laneName
    override def hasCancelRequest = cancel
  }
  def ctrl(id : Int) : CtrlLaneApi = {
    idToCtrl.getOrElseUpdate(id, new CtrlLaneApiImpl(id).setCompositeName(this, "ctrls_" + id.toString))
  }
  def execute(id: Int) : CtrlLaneApi = {
    assert(id >= 0)
    ctrl(id + executeAt)
  }

  def getExecuteAge(at : Int) = getAge(at + executeAt, false)
  def getAge(at: Int, prediction: Boolean): Int = Ages.EU + at * Ages.STAGE + prediction.toInt * Ages.PREDICTION
  override def getCompletions(): Seq[Flow[CompletionPayload]] = logic.completions.onCtrl.map(_.port).toSeq

  val logic = during build new Area {
    uopLock.await()
    pipelineLock.await()

    // Generate the register files read + bypass
    val rf = new Area {
      val rfSpecs = rfStageables.keys.map(_.rf).distinctLinked
      val rfPlugins = rfSpecs.map(spec => host.find[RegfileService](_.rfSpec == spec))

      val readCtrl = ctrl(rfReadAt)
      val reads = for ((spec, payload) <- rfStageables) yield new Area {
        // Implement the register file read
        val rfa = Decode.rfaKeys.get(spec.access)
        val rfPlugin = host.find[RegfileService](_.rfSpec == spec.rf)
        val port = rfPlugin.newRead(false)
        port.valid := readCtrl.up.isFiring && readCtrl(rfa.ENABLE) && rfa.is(spec.rf, readCtrl(rfa.RFID))
        port.address := readCtrl(rfa.PHYS)

        // Generate a bypass specification for the regfile readed data
        case class BypassSpec(eu: ExecuteLaneService, nodeId: Int, payload: Payload[Bits])
        val bypassSpecs = mutable.LinkedHashSet[BypassSpec]()
        val eus = host.list[ExecuteLaneService]
        for (eu <- eus; ops = eu.getMicroOp();
             op <- ops; opSpec = eu.getSpec(op)) {
          eu.pipelineLock.await() // Ensure that the eu specification is done
          val sameRf = opSpec.op.resources.exists {
            case RfResource(spec.rf, RD) => true
            case _ => false
          }
          if (sameRf) opSpec.rd match {
            case Some(rd) =>
              for (nodeId <- rd.bypassesAt) {
                val bypassSpec = BypassSpec(eu, nodeId, rd.DATA)
                bypassSpecs += bypassSpec
              }
            case None =>
          }
        }

        // Implement the bypass hardware
        val dataCtrl = ctrl(rfReadAt + rfPlugin.readLatency)
        val rfaRd = Decode.rfaKeys.get(RD)
        val bypassSorted = bypassSpecs.toSeq.sortBy(_.nodeId)
        val bypassEnables = Bits(bypassSorted.size + 1 bits)
        for ((b, id) <- bypassSorted.zipWithIndex) {
          val node = b.eu.ctrl(b.nodeId)
          bypassEnables(id) := node(rfaRd.ENABLE) && node(rfaRd.PHYS) === dataCtrl(rfa.PHYS) && node(rfaRd.RFID) === dataCtrl(rfa.RFID)
        }
        bypassEnables.msb := True
        val sel = OHMasking.firstV2(bypassEnables)
        dataCtrl(payload) := OHMux.or(sel, bypassSorted.map(b => b.eu.ctrl(b.nodeId)(b.payload)) :+ port.data, true)
      }
    }

    // Implement completion logic
    val completions = new Area{
      val groups = getMicroOpSpecs().groupBy(_.completion)

      val onCtrl = for((at, uops) <- groups if at.exists(_ != -1)) yield new Area {
        val c = ctrl(at.get)
        val ENABLE = Payload(Bool())
        setDecodingDefault(ENABLE, False)
        for(uop <- uops) addDecoding(uop.op, ENABLE -> True)
        val port = Flow(CompletionPayload())
        port.valid := c.isFiring && c(ENABLE)
        port.hartId := c(Global.HART_ID)
        port.uopId := c(Decode.UOP_ID)
      }
    }

    // Implement some UOP decoding for the execute's plugin usages
    val decodeCtrl = ctrl(decodeAt)
    val decoding = new decodeCtrl.Area {
      val coverAll = getMicroOp().map(e => Masked(e.key))
      for ((key, spec) <- decodingSpecs) {
        key.assignFromBits(spec.build(Decode.UOP, coverAll).asBits)
      }
    }

    // Handle SEL initialisation and flushes
    val rp = host[ReschedulePlugin]
    for(ctrlId <- 0 until idToCtrl.keys.max){
      val c = idToCtrl(ctrlId)
      if(ctrlId != 0) c.up(c.LANE_SEL).setAsReg().init(False)

      val age = getAge(ctrlId, false)
      val doIt = rp.isFlushedAt(age, c(Global.HART_ID), c(Execute.LANE_AGE))
      doIt match {
        case Some(cond) =>
          c.cancel := cond
          when(cond) {
            c.bypass(c.LANE_SEL) := False
          }
        case None => c.cancel := False
      }
    }

    host.list[RegfileService].foreach(_.elaborationLock.release())

    eupp.pipelineLock.release()
  }

  def freezeWhen(cond: Bool)(implicit loc: Location) = eupp.freezeWhen(cond)
  def isFreezed(): Bool = eupp.isFreezed()
}
