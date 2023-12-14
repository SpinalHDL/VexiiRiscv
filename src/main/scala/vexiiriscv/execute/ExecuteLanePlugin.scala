package vexiiriscv.execute

import spinal.core._
import spinal.core.fiber.{Handle, Lock, Lockable}
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
import vexiiriscv.schedule.{Ages, DispatchPlugin, FlushCmd, ReschedulePlugin}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class UopImplKey(uop : MicroOp, name : LaneLayer)

//class DecodingFromUopImpls(val target: Payload[_ <: BaseType]) {
//  var default = Option.empty[Masked]
//  val needs = mutable.LinkedHashMap[UopImplKey, Masked]()
//
//  def setDefault(value: Masked) = {
//    default match {
//      case Some(x) => ???
//      case None => default = Some(value)
//    }
//    this
//  }
//
//  def addNeeds(key: UopImplKey, value: Masked): this.type = {
//    needs.get(key) match {
//      case Some(x) => assert(needs(key) == value)
//      case None => needs(key) = value
//    }
//    this
//  }
//
//  def addNeeds(keys: Seq[UopImplKey], value: Masked): this.type = {
//    keys.foreach(addNeeds(_, value))
//    this
//  }
//}


class ExecuteLanePlugin(override val laneName : String,
                        override val rfReadAt : Int,
                        val decodeAt : Int,
                        override val executeAt : Int,
                        override val withBypasses : Boolean) extends FiberPlugin with ExecuteLaneService with CompletionService{
  lazy val eupp = host[ExecutePipelinePlugin]
  setupRetain(eupp.pipelineLock)
  setName("execute_" + laneName)

  during setup {
    host.list[RegfileService].foreach(_.elaborationLock.retain())
  }

  val readLatencyMax = Handle[Int]
  override def rfReadLatencyMax: Int = readLatencyMax.get

  val layers = ArrayBuffer[LaneLayer]()
  override def add(layer: LaneLayer): Unit = layers += layer

  override def getUopLayerSpec(): Iterable[UopLayerSpec] = {
    uopLock.await()
    layers.flatMap(_.uops.values)
  }

  override def getUops(): Iterable[MicroOp] = {
    uopLock.await()
    layers.flatMap(_.uops.values.map(_.uop)).distinctLinked
  }

  override def getLayers(): Iterable[LaneLayer] = {
    uopLock.await()
    layers
  }

  def setDecodingDefault(key: Payload[_ <: BaseType], value: BaseType): Unit = {
    val masked = Masked(value)
    decodingDefaults.get(key) match {
      case None => decodingDefaults(key) = masked
      case Some(x) => assert(x == masked)
    }
  }

  val decodingDefaults = mutable.LinkedHashMap[Payload[_ <: BaseType], Masked]()

  val rfStageables = mutable.LinkedHashMap[RfResource, Payload[Bits]]()

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
    ctrl(id + executeAt)
  }

//  val flushPorts = mutable.LinkedHashMap[Int, ArrayBuffer[Flow[FlushCmd]]]()
//  override def newFlushPort(executeId: Int): Flow[FlushCmd] = {
//    flushPorts.getOrElseUpdate(executeId, ArrayBuffer[Flow[FlushCmd]]()).addRet()
//  }

  override def rfReadHazardFrom(usedAt : Int) : Int = if(withBypasses) usedAt else rfReadAt+1

  override def getCompletions(): Seq[Flow[CompletionPayload]] = logic.completions.onCtrl.map(_.port).toSeq

  val logic = during build new Area {
    uopLock.await()

    getLayers().foreach(_.doChecks())

    var readLatencyMax = 0
    val rfSpecs = rfStageables.keys.map(_.rf).distinctLinked
    val reads = for ((spec, payload) <- rfStageables) yield new Area { //IMPROVE Bit pessimistic as it also trigger on rd
      readLatencyMax = readLatencyMax max readLatencyMax
    }
    ExecuteLanePlugin.this.readLatencyMax.load(readLatencyMax)

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
        for (eu <- eus; opSpec <- eu.getUopLayerSpec()) {
          eu.pipelineLock.await() // Ensure that the eu specification is done
          val sameRf = opSpec.uop.resources.exists {
            case RfResource(spec.rf, RD) => true
            case _ => false
          }
          if (sameRf) opSpec.rd match {
            case Some(rd) =>
              for (nodeId <- rd.broadcastedFrom until rd.rfReadableFrom + rfPlugin.readLatency) {
                val bypassSpec = BypassSpec(eu, nodeId, rd.DATA)
                bypassSpecs += bypassSpec
              }
            case None =>
          }
        }


        assert(rfReadAt + rfPlugin.readLatency + 1 == executeAt, "as for now the bypass isn't implemented to udpate the data on the read latency + 1 until execute at")

        // Implement the bypass hardware
        val mainBypassAt = rfReadAt + rfPlugin.readLatency
        val dataCtrl = ctrl(mainBypassAt)
        val rfaRd = Decode.rfaKeys.get(RD)
        val bypassSorted = bypassSpecs.toSeq.sortBy(_.nodeId)
        val bypassEnables = Bits(bypassSorted.size + 1 bits)
        for ((b, id) <- bypassSorted.zipWithIndex) {
          val node = b.eu.ctrl(b.nodeId)
          bypassEnables(id) := node.isValid && node(rfaRd.ENABLE) && node(rfaRd.PHYS) === dataCtrl(rfa.PHYS) && node(rfaRd.RFID) === dataCtrl(rfa.RFID)
        }
        bypassEnables.msb := True
        val sel = OHMasking.firstV2(bypassEnables)
        dataCtrl(payload) := OHMux.or(sel, bypassSorted.map(b => b.eu.ctrl(b.nodeId)(b.payload)) :+ port.data, true)


        //Update the RSx values along the pipeline
        val along = new Area {
          var useRsUntil = -100
          for (opSpec <- getUopLayerSpec()) {
            opSpec.uop.resources.foreach {
              case RfResource(spec.rf, spec.access) => useRsUntil = useRsUntil max opSpec.rs(spec.access.asInstanceOf[RfRead]).from
              case _ =>
            }
          }

          val updateSpecs = mutable.LinkedHashSet[BypassSpec]()
          for (eu <- eus; opSpec <- eu.getUopLayerSpec()) {
            val sameRf = opSpec.uop.resources.exists {
              case RfResource(spec.rf, RD) => true
              case _ => false
            }
            if (sameRf) opSpec.rd match {
              case Some(rd) =>
                val bypassSpec = BypassSpec(eu, rd.broadcastedFrom, rd.DATA)
                updateSpecs += bypassSpec
              case None =>
            }
          }


          val ctrlRange = mainBypassAt + 1 until useRsUntil
          val bypasses = for (ctrlId <- ctrlRange) yield new Area{
            val on = ctrl(ctrlId)
            val filtred = updateSpecs.filter(_.nodeId > ctrlId).toSeq
            val hits = filtred.map{ f =>
              val node = f.eu.ctrl(f.nodeId)
              node.isValid && node(rfaRd.ENABLE) && node(rfaRd.PHYS) === on(rfa.PHYS) && node(rfaRd.RFID) === on(rfa.RFID)
            }.asBits

            on.bypass(apply(spec)) := OHMux.or(Cat(hits, !hits.orR), on.up(apply(spec)) +: filtred.map(f => f.eu.ctrl(f.nodeId)(f.payload)), true)
          }
        }
      }
    }

    // Implement completion logic
    val completions = new Area{
      val groups = getUopLayerSpec().groupBy(_.completion)

      val onCtrl = for((at, impls) <- groups if at.exists(_ != -1)) yield new Area {
        val c = ctrl(at.get)
        val ENABLE = Payload(Bool())
        setDecodingDefault(ENABLE, False)
        for(impl <- impls) impl.addDecoding(ENABLE -> True)
        val port = Flow(CompletionPayload())
        port.valid := c.down.isFiring && c(ENABLE)
        port.hartId := c(Global.HART_ID)
        port.uopId := c(Decode.UOP_ID)
        port.trap := c(Global.TRAP)
      }
    }

    // Implement some UOP decoding for the execute's plugin usages
    val decodeCtrl = ctrl(decodeAt)
    val implSelMask = ((BigInt(1) << log2Up(layers.size))-1) << Decode.UOP_WIDTH
    val decoding = new decodeCtrl.Area {
      def implToMasked(impl : UopLayerSpec) = {
        val uop = Masked(impl.uop.key)
        val sel = Masked(BigInt(getLayerId(impl.elImpl)) << Decode.UOP_WIDTH, implSelMask)
        uop fuse sel
      }
      val coverAll = getUopLayerSpec().map(implToMasked)
      val decodingSpecs = mutable.LinkedHashMap[Payload[_ <: BaseType], DecodingSpec[_ <: BaseType]]()
      def ds(key : Payload[_ <: BaseType]) = decodingSpecs.getOrElseUpdate(key, new DecodingSpec(key))
      for((key, default) <- decodingDefaults) ds(key).setDefault(default)
      for(impl <- getUopLayerSpec()){
        for((key, value) <- impl.decodings) ds(key).addNeeds(implToMasked(impl), value)
      }
      val decodingBits = apply(LAYER_SEL) ## apply(Decode.UOP)
      for ((key, spec) <- decodingSpecs) {
        key.assignFromBits(spec.build(decodingBits, coverAll).asBits)
      }
    }

    // Handle SEL initialisation and flushes
    val rp = host[ReschedulePlugin]
    for(ctrlId <- 0 to idToCtrl.keys.max){
      ctrl(ctrlId) //Ensure creation
      val c = idToCtrl(ctrlId)
      if(ctrlId != 0) c.up(c.LANE_SEL).setAsReg().init(False)

      val age = getAge(ctrlId)
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
  override def atRiskOfFlush(executeId: Int): Bool = {
    val ctrlId = executeId + executeAt
    val elps = host.list[ExecuteLaneService]
    val hazards = ArrayBuffer[Bool]()
    for(elp <- elps){
      val upTo = elp.getUopLayerSpec().map(_.mayFlushUpTo.getOrElse(-100)).max
      for(i <- ctrlId + (elp == this).toInt until upTo){
        hazards += elp.ctrl(i).isValid && elp.ctrl(i)(Global.HART_ID) === ctrl(ctrlId)(Global.HART_ID) && (if(i == ctrlId) elp.ctrl(i)(Execute.LANE_AGE) === ctrl(ctrlId)(Execute.LANE_AGE)  else True) //Quite pessimistic
      }
    }
    hazards.orR
  }
}
