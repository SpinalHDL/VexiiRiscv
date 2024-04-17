package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute.{CompletionPayload, CompletionService, CsrAccessPlugin, CsrService, LaneLayer}
import vexiiriscv.regfile.{RegFileWriter, RegFileWriterService, RegfileService}
import vexiiriscv.riscv.{CSR, Const, FloatRegFile, IntRegFile, MicroOp, RD, RS1, RS2, RS3, RfRead, RfResource, RfWrite, Riscv, Rvfd}

import scala.collection.mutable.ArrayBuffer


class FpuExecute(val layer : LaneLayer,
                 val forkAt: Int,
                 val wbAt: Int) extends FiberPlugin with CompletionService with RegFileWriterService{


  override def getCompletions(): Seq[Flow[CompletionPayload]] = List(api.floatCompletion)
  override def getRegFileWriters(): Seq[Flow[RegFileWriter]] = List(logic.onFloatWb.fpWriter)

  val api = during build new Area{
    val rm = Reg(Bits(3 bits)) init (0)
    val floatCompletion = Flow(CompletionPayload())
  }

  val logic = during setup new Area{
    val cp = host[CsrService]
    val rfp = host.find[RegfileService](_.rfSpec == FloatRegFile)
    val buildBefore = retains(cp.csrLock, rfp.elaborationLock, layer.el.pipelineLock)
    val uopLock = retains(layer.el.uopLock)
    awaitBuild()

    val fpWb = rfp.newWrite(false) //TODO port sharing

    val slotsCount = wbAt - forkAt
    val robIdWidth = log2Up(slotsCount)
    val floatCmd = master(Stream(FpuFloatCmd(Riscv.RVD, robIdWidth)))
    val intCmd = master(Stream(FpuIntCmd(Riscv.XLEN.get == 64, robIdWidth)))
    val floatWriteback = slave(Flow(FpuFloatWriteback(robIdWidth, 32 + Riscv.RVD.get.toInt * 32)))
    val integerWriteback = slave(Stream(FpuIntWriteback(robIdWidth, Riscv.XLEN)))

    val SEL = Payload(Bool())
    val OPCODE = Payload(FpuOpcode())
    val FORMAT = Payload(FpuFormat())
    val FLOAT  = Payload(Bool())
    val ARG = Payload(Bits(2 bits))

    def arg(v: Int) = ARG -> B(v, 2 bits)
    def op(v: FpuOpcode.E) = OPCODE -> v

    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    import FpuOpcode._

    layer.el.setDecodingDefault(SEL, False)
    def add(uop: MicroOp, decodings : (Payload[_ <: BaseType], Any)*) = {
      val spec = layer.add(uop)
      spec.addDecoding(SEL -> True, FLOAT -> True)
      spec.addDecoding(decodings)
      spec.dontFlushFrom(forkAt) //This avoid having a reorder buffer on FPU responses
      uop.resources.foreach{
        case RfResource(_, rs: RfRead ) => spec.addRsSpec(rs, forkAt)
        case RfResource(_, rd: RfWrite) => spec.rdOutOfPip = true//spec.setRdSpec(forkAt+1, forkAt+1) //TODO
        case _ =>
      }
    }

//    add(Rvfd.FMV_X_W, DecodeList(op(FMV_X_W), f32))
//
    add(Rvfd.FADD_S, op(ADD), f32, arg(0))
//    add(Rvfd.FSUB_S, DecodeList(op(ADD), f32, arg(1)))
//    add(Rvfd.FMUL_S, DecodeList(op(MUL), f32))
//    add(Rvfd.FDIV_S, DecodeList(op(DIV), f32))
//    add(Rvfd.FSQRT_S, DecodeList(op(SQRT), f32))
//
    add(Rvfd.FMADD_S, op(FMA), f32, arg(0))
//    add(Rvfd.FMSUB_S, DecodeList(op(FMA), f32, arg(1)))
//    add(Rvfd.FNMADD_S, DecodeList(op(FMA), f32, arg(3)))
//    add(Rvfd.FNMSUB_S, DecodeList(op(FMA), f32, arg(2)))
//
//    add(Rvfd.FSGNJ_S, DecodeList(op(SGNJ), f32, arg(0)))
//    add(Rvfd.FSGNJN_S, DecodeList(op(SGNJ), f32, arg(1)))
//    add(Rvfd.FSGNJX_S, DecodeList(op(SGNJ), f32, arg(2)))
//
//    add(Rvfd.FMIN_S, DecodeList(op(MIN_MAX), f32, arg(0)))
//    add(Rvfd.FMAX_S, DecodeList(op(MIN_MAX), f32, arg(1)))
//
//    add(Rvfd.FLE_S, DecodeList(op(CMP), f32, arg(0)))
//    add(Rvfd.FEQ_S, DecodeList(op(CMP), f32, arg(2)))
//    add(Rvfd.FLT_S, DecodeList(op(CMP), f32, arg(1)))
//
//    add(Rvfd.FCLASS_S, DecodeList(op(FCLASS), f32))
//
//    add(Rvfd.FCVT_WU_S, DecodeList(op(F2I), f32, arg(0)))
//    add(Rvfd.FCVT_W_S, DecodeList(op(F2I), f32, arg(1)))
//
//    if (XLEN.get == 64) {
//      add(Rvfd.FCVT_LU_S, DecodeList(op(F2I), f32, arg(2)))
//      add(Rvfd.FCVT_L_S, DecodeList(op(F2I), f32, arg(3)))
//    }
//
//    if (RVD) {
//      add(Rvfd.FADD_D, DecodeList(op(ADD), f64, arg(0)))
//      add(Rvfd.FSUB_D, DecodeList(op(ADD), f64, arg(1)))
//      add(Rvfd.FMUL_D, DecodeList(op(MUL), f64))
//      add(Rvfd.FDIV_D, DecodeList(op(DIV), f64))
//      add(Rvfd.FSQRT_D, DecodeList(op(SQRT), f64))
//
//      add(Rvfd.FMADD_D, DecodeList(op(FMA), f64, arg(0)))
//      add(Rvfd.FMSUB_D, DecodeList(op(FMA), f64, arg(1)))
//      add(Rvfd.FNMADD_D, DecodeList(op(FMA), f64, arg(3)))
//      add(Rvfd.FNMSUB_D, DecodeList(op(FMA), f64, arg(2)))
//
//      add(Rvfd.FSGNJ_D, DecodeList(op(SGNJ), f64, arg(0)))
//      add(Rvfd.FSGNJN_D, DecodeList(op(SGNJ), f64, arg(1)))
//      add(Rvfd.FSGNJX_D, DecodeList(op(SGNJ), f64, arg(2)))
//
//      add(Rvfd.FMIN_D, DecodeList(op(MIN_MAX), f64, arg(0)))
//      add(Rvfd.FMAX_D, DecodeList(op(MIN_MAX), f64, arg(1)))
//
//      add(Rvfd.FLE_D, DecodeList(op(CMP), f64, arg(0)))
//      add(Rvfd.FEQ_D, DecodeList(op(CMP), f64, arg(2)))
//      add(Rvfd.FLT_D, DecodeList(op(CMP), f64, arg(1)))
//
//      add(Rvfd.FCLASS_D, DecodeList(op(FCLASS), f64))
//
//      add(Rvfd.FCVT_WU_D, DecodeList(op(F2I), f64, arg(0)))
//      add(Rvfd.FCVT_W_D, DecodeList(op(F2I), f64, arg(1)))
//      add(Rvfd.FCVT_D_S, DecodeList(op(FCVT_X_X), f32))
//      add(Rvfd.FCVT_S_D, DecodeList(op(FCVT_X_X), f64))
//
//      if (XLEN.get == 64) {
//        add(Rvfd.FMV_X_D, DecodeList(op(FMV_X_W), f64))
//
//        add(Rvfd.FCVT_LU_D, DecodeList(op(F2I), f64, arg(2)))
//        add(Rvfd.FCVT_L_D, DecodeList(op(F2I), f64, arg(3)))
//      }
//    }

//    val floatList, intList, lsuList = ArrayBuffer[MicroOp]()
//    floatList ++= List(
//      Rvfd.FMV_W_X, Rvfd.FADD_S, Rvfd.FSUB_S, Rvfd.FMUL_S, Rvfd.FDIV_S, Rvfd.FSQRT_S, Rvfd.FMADD_S,
//      Rvfd.FMSUB_S, Rvfd.FNMADD_S, Rvfd.FNMSUB_S, Rvfd.FSGNJ_S, Rvfd.FSGNJN_S, Rvfd.FSGNJX_S, Rvfd.FMIN_S,
//      Rvfd.FMAX_S, Rvfd.FCVT_S_WU, Rvfd.FCVT_S_W
//    )
//    intList ++= List(
//      Rvfd.FMV_X_W, Rvfd.FCVT_WU_S, Rvfd.FCVT_W_S, Rvfd.FCLASS_S, Rvfd.FLE_S, Rvfd.FEQ_S, Rvfd.FLT_S
//    )
//    lsuList ++= List(Rvfd.FLW, Rvfd.FSW)
//
//    if (Riscv.XLEN.get == 64) {
//      floatList ++= List(
//        Rvfd.FCVT_S_LU, Rvfd.FCVT_S_L
//      )
//      intList ++= List(
//        Rvfd.FCVT_LU_S, Rvfd.FCVT_L_S
//      )
//      lsuList ++= List(Rvfd.FLD, Rvfd.FSD)
//    }
//
//    if (Riscv.RVD) {
//      floatList ++= List(
//        Rvfd.FADD_D, Rvfd.FSUB_D, Rvfd.FMUL_D, Rvfd.FDIV_D, Rvfd.FSQRT_D, Rvfd.FMADD_D, Rvfd.FMSUB_D,
//        Rvfd.FNMADD_D, Rvfd.FNMSUB_D, Rvfd.FSGNJ_D, Rvfd.FSGNJN_D, Rvfd.FSGNJX_D, Rvfd.FMIN_D, Rvfd.FMAX_D,
//        Rvfd.FCVT_D_WU, Rvfd.FCVT_D_W, Rvfd.FCVT_D_S, Rvfd.FCVT_S_D
//      )
//      intList ++= List(
//        Rvfd.FCVT_WU_D, Rvfd.FCVT_W_D, Rvfd.FCLASS_D, Rvfd.FLE_D, Rvfd.FEQ_D, Rvfd.FLT_D
//      )
//      if (Riscv.XLEN.get == 64) {
//        floatList ++= List(
//          Rvfd.FMV_D_X, Rvfd.FCVT_D_LU, Rvfd.FCVT_D_L
//        )
//        intList ++= List(
//          Rvfd.FMV_X_D, Rvfd.FCVT_LU_D, Rvfd.FCVT_L_D
//        )
//      }
//    }



    uopLock.release()

    val state = new Area {
      assert(Global.HART_COUNT.get == 1)
      val flags = Reg(FpuFlags())
      flags.NV init (False)
      flags.DZ init (False)
      flags.OF init (False)
      flags.UF init (False)
      flags.NX init (False)

      cp.readWrite(CSR.FCSR, 5 -> api.rm)
      cp.readWrite(CSR.FCSR, 0 -> flags)
      cp.readWrite(CSR.FRM, 0 -> api.rm)
      cp.readWrite(CSR.FFLAGS, 0 -> flags)
      assert(host[CsrAccessPlugin].layer == layer, "Csr and FPU need to be on the same layer, unless CSR -> RM -> FPU fencing is implemented")
    }

    val rfa = Decode.rfaKeys.get(RD)

    val forkPtr, joinPtr = Counter(slotsCount)
    val slots = List.fill(slotsCount)(new Area{
      val forked, joined = Reg(Bool()) init(False)
      val data = Reg(Bits(Math.max(Riscv.XLEN, Riscv.FLEN) bits))
    })

    val onFork = new layer.el.Execute(forkAt){
      val instrRounding = Decode.UOP(Const.funct3Range)
      val roundMode = (instrRounding === B"111") ? api.rm | instrRounding
      val format = (if (Riscv.RVD) this(FORMAT) else FpuFormat.FLOAT())

      val forked = RegInit(False) setWhen (floatCmd.fire || intCmd.fire) clearWhen (!layer.el.isFreezed())
      val freezeIt = !forked && isValid && SEL && (!FLOAT.mux(floatCmd.ready, intCmd.ready) || slots.full)
      layer.el.freezeWhen(freezeIt)

      when(isValid && SEL && !layer.el.isFreezed()) {
        slots.entries.onSel(slots.freeId) { e =>
          e.valid := True
          e.float := FLOAT
          e.phys  := rfa.PHYS
          e.uopId := Decode.UOP_ID
        }
      }

      floatCmd.valid  := isValid && SEL && FLOAT && !forked
      floatCmd.opcode := OPCODE
      floatCmd.arg    := ARG
      floatCmd.rs(0)  := layer.el(FloatRegFile, RS1)
      floatCmd.rs(1)  := layer.el(FloatRegFile, RS2)
      floatCmd.rs(2)  := layer.el(FloatRegFile, RS3)
      floatCmd.format := format

      floatCmd.roundMode.assignFromBits(roundMode)
      floatCmd.robId := forkPtr

      intCmd.valid := isValid && SEL && !FLOAT && !forked
      intCmd.opcode := OPCODE
      intCmd.arg := ARG
      intCmd.rs1 := layer.el(IntRegFile, RS1)
      intCmd.format := format
      intCmd.roundMode.assignFromBits(roundMode)
      intCmd.robId := forkPtr
    }

    val onFloatWb = new Area{
      val access = slots.entries.reader(floatWriteback.robId)
      when(floatWriteback.fire) {
        access.onSel { e => e.valid := False }
      }

      fpWb.valid := floatWriteback.fire
      fpWb.data := floatWriteback.value
      fpWb.address := access(_.phys)

      val fpWriter = Flow(RegFileWriter(FloatRegFile))
      fpWriter.valid := fpWb.valid
      fpWriter.data := fpWb.data
      fpWriter.uopId := access(_.uopId)

      api.floatCompletion.valid := floatWriteback.fire
      api.floatCompletion.hartId := 0; assert(Global.HART_COUNT.get == 1)
      api.floatCompletion.uopId := access(_.uopId)
      api.floatCompletion.trap := False
      api.floatCompletion.commit := True
    }


//    val onFloatWb = new Area {
//      val completionPort = Flow(CompletionPayload())
//    }

    //TODO
    integerWriteback.ready := False

    buildBefore.release()
  }
}
