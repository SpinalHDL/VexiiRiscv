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