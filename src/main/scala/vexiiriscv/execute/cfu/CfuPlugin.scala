package vexiiriscv.execute.cfu


import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.WeakConnector
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute.{CsrService, LaneLayer, WriteBackPlugin}
import vexiiriscv.riscv.{IMM, IntRegFile, RD, RS1, RS2, RegfileSpec, Resource, SingleDecoding}

import scala.collection.mutable.ArrayBuffer

case class CfuPluginParameter(
                               CFU_VERSION : Int,
                               CFU_INTERFACE_ID_W : Int,
                               CFU_FUNCTION_ID_W : Int,
                               CFU_REORDER_ID_W : Int,
                               CFU_REQ_RESP_ID_W : Int,
                               CFU_INPUTS : Int,
                               CFU_INPUT_DATA_W : Int,
                               CFU_OUTPUTS : Int,
                               CFU_OUTPUT_DATA_W : Int,
                               CFU_FLOW_REQ_READY_ALWAYS : Boolean,
                               CFU_FLOW_RESP_READY_ALWAYS : Boolean)

case class CfuBusParameter(CFU_VERSION : Int = 0,
                           CFU_INTERFACE_ID_W : Int = 0,
                           CFU_FUNCTION_ID_W : Int,
                           CFU_CFU_ID_W : Int = 0,
                           CFU_REORDER_ID_W : Int = 0,
                           CFU_REQ_RESP_ID_W : Int = 0,
                           CFU_STATE_INDEX_NUM : Int = 0,
                           CFU_INPUTS : Int,
                           CFU_INPUT_DATA_W : Int,
                           CFU_OUTPUTS : Int,
                           CFU_OUTPUT_DATA_W : Int,
                           CFU_FLOW_REQ_READY_ALWAYS : Boolean,
                           CFU_FLOW_RESP_READY_ALWAYS : Boolean,
                           CFU_WITH_STATUS : Boolean = false,
                           CFU_RAW_INSN_W : Int = 0)

/**
 * CFU (Custom Functional Unit) allows people to implement custom instruction hardware outside the CPU through a stream based interface.
 * This plugin implements a old version of the CFU spec, the same as VexRiscv.
 */
case class CfuCmd( p : CfuBusParameter ) extends Bundle{
  val function_id = UInt(p.CFU_FUNCTION_ID_W bits)
  val reorder_id = UInt(p.CFU_REORDER_ID_W bits)
  val request_id = UInt(p.CFU_REQ_RESP_ID_W bits)
  val inputs = Vec(Bits(p.CFU_INPUT_DATA_W bits), p.CFU_INPUTS)
  val state_index = UInt(log2Up(p.CFU_STATE_INDEX_NUM) bits)
  val cfu_index = UInt(p.CFU_CFU_ID_W bits)
  val raw_insn = Bits(p.CFU_RAW_INSN_W bits)
  def weakAssignFrom(m : CfuCmd): Unit ={
    def s = this
    WeakConnector(m, s, m.function_id, s.function_id, defaultValue = null, allowUpSize = false, allowDownSize = true , allowDrop = true)
    WeakConnector(m, s, m.reorder_id,  s.reorder_id,  defaultValue = null, allowUpSize = false , allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.request_id,  s.request_id,  defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
    s.inputs := m.inputs
  }
}

case class CfuRsp(p : CfuBusParameter) extends Bundle{
  val response_id = UInt(p.CFU_REQ_RESP_ID_W bits)
  val outputs = Vec(Bits(p.CFU_OUTPUT_DATA_W bits), p.CFU_OUTPUTS)
  val status = p.CFU_WITH_STATUS generate Bits(3 bits)

  def weakAssignFrom(m : CfuRsp): Unit ={
    def s = this
    s.response_id := m.response_id
    s.outputs := m.outputs
  }
}

case class CfuBus(p : CfuBusParameter) extends Bundle with IMasterSlave{
  val cmd = Stream(CfuCmd(p))
  val rsp = Stream(CfuRsp(p))

  def <<(m : CfuBus) : Unit = {
    val s = this
    s.cmd.arbitrationFrom(m.cmd)
    m.rsp.arbitrationFrom(s.rsp)

    s.cmd.weakAssignFrom(m.cmd)
    m.rsp.weakAssignFrom(s.rsp)
  }

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

object CfuPlugin{
  object Input2Kind extends SpinalEnum{
    val RS, IMM_I = newElement()
  }
}

case class CfuPluginEncoding(instruction : MaskedLiteral,
                             functionId : List[Range],
                             input2Kind : CfuPlugin.Input2Kind.E){
  val functionIdWidth = functionId.map(_.size).sum
}

class CfuPlugin(val layer : LaneLayer,
                val forkAt : Int,
                val joinAt : Int,
                val allowZeroLatency : Boolean,
                val busParameter : CfuBusParameter,
                val encodings : List[CfuPluginEncoding] = null,
                val stateAndIndexCsrOffset : Int = 0xBC0,
                val statusCsrOffset : Int = 0x801,
                val withEnable : Boolean = true,
                val enableInit : Boolean = false) extends FiberPlugin{
  def p = busParameter
  import CfuPlugin._

  assert(p.CFU_INPUTS <= 2)
  assert(p.CFU_OUTPUTS == 1)


  val logic = during setup new Area{
    val wbp = host.find[WriteBackPlugin](p => p.rf == IntRegFile && p.lane == layer.lane)
    val cp = host[CsrService]
    val ds = host[DecoderService]
    val earlyLock = retains(layer.lane.uopLock, wbp.elaborationLock, ds.decodingLock)
    val lateLock = retains(List(layer.lane.pipelineLock, cp.csrLock))
    awaitBuild()

    val bus = master(CfuBus(p))

    val CFU_ENABLE = Payload(Bool())
    val CFU_IN_FLIGHT = Payload(Bool())
    val CFU_ENCODING = Payload(UInt(log2Up(encodings.size) bits))
    val CFU_INPUT_2_KIND = Payload(CfuPlugin.Input2Kind())

    val wb = wbp.createPort(at = joinAt)

    layer.lane.setDecodingDefault(CFU_ENABLE, False)
    if(withEnable) ds.addMicroOpDecodingDefault(CFU_ENABLE, False)

    val en = withEnable generate (Reg(Bool()) init(enableInit))
    val mappings = for((encoding, id) <- encodings.zipWithIndex) yield new Area{
      val ressources = ArrayBuffer[Resource]()
      ressources += IntRegFile -> RD
      ressources += IntRegFile -> RS1
      encoding.input2Kind match {
        case CfuPlugin.Input2Kind.RS => ressources += IntRegFile -> RS2
        case _ =>
      }
      val uopType = SingleDecoding(
        key = encoding.instruction,
        resources = ressources
      )

      val uop = layer.add(uopType)
      uop.addRsSpec(RS1, executeAt = 0)
      encoding.input2Kind match {
        case CfuPlugin.Input2Kind.RS => uop.addRsSpec(RS2, executeAt = 0)
        case _ =>
      }
      uop.setCompletion(joinAt)
      uop.addDecoding(CFU_ENABLE -> True)
      uop.addDecoding(CFU_ENCODING -> U(id))
      uop.addDecoding(CFU_INPUT_2_KIND -> encoding.input2Kind(native))
      uop.dontFlushFrom(forkAt)

      wbp.addMicroOp(wb, uop)
      if(withEnable) ds.addMicroOpDecoding(uopType, CFU_ENABLE, True)
    }


    if(withEnable) ds.addDecodingLogic{ctx =>
      ctx.legal clearWhen(ctx.node(CFU_ENABLE) && !en)
    }

    earlyLock.release()

    val csr = new Area{
      cp.flushOnWrite(stateAndIndexCsrOffset)
      if(withEnable) cp.readWrite(stateAndIndexCsrOffset, 31 -> en)

      val stateId = Reg(UInt(log2Up(p.CFU_STATE_INDEX_NUM) bits)) init(0)
      if(p.CFU_STATE_INDEX_NUM > 1) {
        assert(stateAndIndexCsrOffset != -1, "CfuPlugin stateCsrIndex need to be set in the parameters")
        cp.readWrite(stateAndIndexCsrOffset, 16 -> stateId)
      }

      val cfuIndex = Reg(UInt(p.CFU_CFU_ID_W bits)) init(0)
      if(p.CFU_CFU_ID_W != 0){
        cp.readWrite(stateAndIndexCsrOffset, 0 -> cfuIndex)
      }
      val status = p.CFU_WITH_STATUS generate new Area{
        val CU, OP, FI, OF, SI, CI = RegInit(False)
        val flags = List(CU, OP, FI, OF, SI, CI).reverse
        cp.readWrite(statusCsrOffset,  flags.zipWithIndex.map(_.swap) :_*)
        cp.flushOnWrite(statusCsrOffset)
      }
    }


    val onFork = new layer.Execute(forkAt) {
      val schedule = isValid && CFU_ENABLE

      val hold = False
      val fired = RegInit(False) setWhen(bus.cmd.fire) clearWhen(!layer.lane.isFreezed())
      CFU_IN_FLIGHT := schedule || hold || fired

      bus.cmd.valid := (schedule || hold) && !fired

      val freezeIt = bus.cmd.valid && !bus.cmd.ready
      layer.lane.freezeWhen(freezeIt)

      val functionIdFromInstructinoWidth = encodings.map(_.functionIdWidth).max
      val functionsIds = encodings.map(e => U(Cat(e.functionId.map(r => Decode.UOP(r))), functionIdFromInstructinoWidth bits))
      bus.cmd.cfu_index := csr.cfuIndex
      bus.cmd.state_index := csr.stateId
      bus.cmd.function_id := functionsIds.read(CFU_ENCODING)
      bus.cmd.reorder_id := 0
      bus.cmd.request_id := 0
      bus.cmd.raw_insn   := Decode.UOP.resized
      if(p.CFU_INPUTS >= 1) bus.cmd.inputs(0) := up(layer.lane(IntRegFile, RS1))
      if(p.CFU_INPUTS >= 2)  bus.cmd.inputs(1) := CFU_INPUT_2_KIND.mux[Bits](
        CfuPlugin.Input2Kind.RS -> up(layer.lane(IntRegFile, RS2)),
        CfuPlugin.Input2Kind.IMM_I -> IMM(Decode.UOP).h_sext.asBits
      )
    }

    val onJoin = new layer.Execute(joinAt) {
      val busRspStream = bus.rsp.toFlow.toStream
      val rsp = busRspStream.queueLowLatency(
        size = joinAt-forkAt+1,
        latency = 0
      )

      val freezeIt = CFU_IN_FLIGHT && !rsp.valid
      layer.lane.freezeWhen(freezeIt)
      rsp.ready := CFU_IN_FLIGHT && !layer.lane.isFreezed()

      wb.valid := isValid && CFU_ENABLE
      wb.payload := rsp.outputs(0)

      when(isValid && isReady && !isCancel && CFU_ENABLE){
        if(p.CFU_WITH_STATUS)
        switch(rsp.status) {
          for (i <- 1 to 6) is(i) {
            csr.status.flags(i-1) := True
          }
        }
      }
    }

    for(eid <- forkAt + 1 to joinAt) {
      layer.lane.execute(eid).up(CFU_IN_FLIGHT).setAsReg().init(False)
    }

    lateLock.release()
  }
}


object CfuTest{
  def getCfuParameter() = CfuBusParameter(
    CFU_VERSION = 0,
    CFU_INTERFACE_ID_W = 0,
    CFU_FUNCTION_ID_W = 3,
    CFU_REORDER_ID_W = 0,
    CFU_REQ_RESP_ID_W = 0,
    CFU_INPUTS = 2,
    CFU_INPUT_DATA_W = 32,
    CFU_OUTPUTS = 1,
    CFU_OUTPUT_DATA_W = 32,
    CFU_FLOW_REQ_READY_ALWAYS = false,
    CFU_FLOW_RESP_READY_ALWAYS = false
  )
}
case class CfuTest() extends Component{
  val io = new Bundle {
    val bus = slave(CfuBus(CfuTest.getCfuParameter()))
  }
  io.bus.rsp.arbitrationFrom(io.bus.cmd)
  io.bus.rsp.response_id := io.bus.cmd.request_id
  io.bus.rsp.outputs(0) := ~(io.bus.cmd.inputs(0) & io.bus.cmd.inputs(1))
}

