package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute._
import vexiiriscv.regfile.{RegFileWriter, RegFileWriterService}
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class FpuPackerCmd(p : FloatUnpackedParam, ats : Seq[Int]) extends Bundle{
  val at = Bits(ats.size bits)
  val value = FloatUnpacked(p)
  val format = FpuFormat()
  val roundMode = FpuRoundMode()
  val hartId = Global.HART_ID()
  val uopId = Decode.UOP_ID()
  val flags = FpuFlags()
}

class FpuPackerPort(_cmd : FpuPackerCmd) extends Area{
  val uopsAt = ArrayBuffer[(UopLayerSpec, Int)]()
  val cmd = _cmd
}

class FpuPackerPlugin(val lane: ExecuteLanePlugin,
                      var wbAt : Int = 2) extends FiberPlugin with RegFileWriterService {
  val p = FpuUtils


//  override def getCompletions(): Seq[Flow[CompletionPayload]] = List(logic.completion)
  override def getRegFileWriters(): Seq[Flow[RegFileWriter]] = List(logic.s2.fpWriter)

  val elaborationLock = Retainer()

  val ports = ArrayBuffer[FpuPackerPort]()
  def createPort(ats : Seq[Int], p : FloatUnpackedParam): FpuPackerPort = {
    ports.addRet(new FpuPackerPort(FpuPackerCmd(p, ats)))
  }

  val logic = during setup new Area{
    val ffwbp = host.find[FpuFlagsWritebackPlugin](p => p.lane == lane)
    val wbp = host.find[WriteBackPlugin](p => p.lane == lane && p.rf == FloatRegFile)
    val buildBefore = retains(lane.pipelineLock, wbp.elaborationLock, ffwbp.elaborationLock)
    val uopLock = retains(lane.uopLock)
    awaitBuild()
    val latency = wbAt

    elaborationLock.await()

    val wbPorts = mutable.LinkedHashMap[Int, Flow[Bits]]()
    val uopsAt = mutable.LinkedHashMap[Int, ArrayBuffer[UopLayerSpec]]()
    for(port <- ports; (uop, at) <- port.uopsAt) uopsAt.getOrElseUpdate(at, ArrayBuffer[UopLayerSpec]()) += uop
    val flagsWb = ffwbp.createPort(uopsAt.keys.map(_ + latency).toList)
    for((at, uops) <- uopsAt) {
      val port = wbp.createPort(at+latency).setName("FpuPackerPlugin_wb_at_" + (at+latency))
      wbPorts(at) = port
      for(uop <- uops) {
        wbp.addMicroOp(port, uop)
        uop.setCompletion(at+latency)
        uop.reserve(FpuPackerPlugin.this, at)
        ffwbp.addUop(flagsWb, uop, at+latency)
      }
    }

    uopLock.release()

    val pip = new StagePipeline()
    val s0 = new pip.Area(0) {
      val exponentMin = ports.map(_.cmd.value.exponentMin).min
      val exponentMax = ports.map(_.cmd.value.exponentMax).max
      val remapped = ports.map(
        _.cmd.value.to(
          FloatUnpackedParam(
            exponentMax = exponentMax,
            exponentMin = exponentMin,
            mantissaWidth = p.mantissaWidth + 2
          )
        )
      )

      val reader = ports.map(_.cmd).reader(ports.map(_.cmd.at.orR))
      val VALUE = insert(OhMux.or(reader.oh.asBits, remapped.toSeq))
      val FORMAT = insert(reader(_.format))
      val ROUNDMODE = insert(reader(_.roundMode))
      val FLAGS = insert(reader(_.flags))
      Global.HART_ID := reader(_.hartId)
      Decode.UOP_ID := reader(_.uopId)
      valid := reader.oh.orR
      val GROUP_OH = Payload(Bits(uopsAt.size bits))
      assert(CountOne(GROUP_OH) <= 1)
      for((at, sel) <- (uopsAt.keys, GROUP_OH.asBools).zipped){
        sel := (for(port <- ports; (portAt, i) <- port.cmd.ats.zipWithIndex; if portAt == at) yield port.cmd.at(i)).orR
      }

      val EXP_SUBNORMAL = insert(AFix(p.muxDouble[SInt](FORMAT)(-1023)(-127)))
      val SUBNORMAL = insert(VALUE.exponent <= EXP_SUBNORMAL && VALUE.isNormal)
    }

    import s0._


    val s1 = new pip.Area(1) {
      // First we check if we are subnormal, in which case we need to denormalize the mantissa
      val EXP_DIF_PLUS_ONE = insert(U(EXP_SUBNORMAL - VALUE.exponent) + 1)

      val manShiftNoSat = EXP_DIF_PLUS_ONE
      val manShift = RegNext(manShiftNoSat.sat(widthOf(manShiftNoSat) - log2Up(p.mantissaWidth + 2)))
      val manShifter = RegNext(U(Shift.rightWithScrap(True ## VALUE.mantissa.raw, manShift).dropHigh(1)))
      val MAN_SHIFTED = insert(manShifter)
      when(!SUBNORMAL){
        MAN_SHIFTED := U(VALUE.mantissa.raw)
      }

      val counter = Reg(UInt(2 bits)) init(0)
      val freezeIt = isValid && SUBNORMAL && counter =/= 2
      lane.freezeWhen(freezeIt)
      when(freezeIt) { counter := counter + 1 }
      when(!lane.isFreezed()){ counter := 0 }

      val f32ManPos = p.mantissaWidth + 2 - 23
      val roundAdjusted = insert(p.muxDouble(FORMAT)(MAN_SHIFTED(0, 2 bits))(MAN_SHIFTED(f32ManPos - 2, 2 bits) | U(MAN_SHIFTED(f32ManPos - 2 - 1 downto 0).orR, 2 bits)))
      val manLsb = insert(p.muxDouble(FORMAT)(MAN_SHIFTED(2))(MAN_SHIFTED(f32ManPos)))

      // Then we apply the rounding necessary
      val ROUNDING_INCR = insert(VALUE.isNormal && ROUNDMODE.mux(
        FpuRoundMode.RNE -> (roundAdjusted(1) && (roundAdjusted(0) || manLsb)),
        FpuRoundMode.RTZ -> False,
        FpuRoundMode.RDN -> (roundAdjusted =/= 0 && VALUE.sign),
        FpuRoundMode.RUP -> (roundAdjusted =/= 0 && !VALUE.sign),
        FpuRoundMode.RMM -> (roundAdjusted(1))
      ))
      val incrBy = p.muxDouble(FORMAT)(U(1))(U(1) << p.mantissaWidth - 23)
      val manIncrWithCarry = (MAN_SHIFTED >> 2) +^ incrBy
      val MAN_CARRY = manIncrWithCarry.msb
      val MAN_INCR = (manIncrWithCarry.dropHigh(1))
      val EXP_INCR = insert(VALUE.exponent + AFix(U(MAN_CARRY)))

      val EXP_RESULT = insert(this(ROUNDING_INCR).mux[AFix](EXP_INCR, VALUE.exponent))
      val MAN_RESULT = insert(this(ROUNDING_INCR).mux(MAN_INCR, B(MAN_SHIFTED >> 2)))
    }
    import s1._

    val s2 = new pip.Area(wbAt) {
      val SUBNORMAL_FINAL = insert((EXP_SUBNORMAL - EXP_RESULT).isPositive())
      val EXP = insert(!SUBNORMAL_FINAL ? (EXP_RESULT - EXP_SUBNORMAL) | AFix(0))

      val EXP_MAX = insert(AFix(p.muxDouble[SInt](FORMAT)(1023)(127)))
      val EXP_MIN = insert(AFix(p.muxDouble[SInt](FORMAT)(-1023 - 52 + 1)(-127 - 23 + 1)))
      val EXP_OVERFLOW = insert(EXP_RESULT > EXP_MAX)
      val EXP_UNDERFLOW = insert(EXP_RESULT < EXP_MIN)

      val mr = VALUE.mantissa.raw
      val tinyRound = p.muxDouble(FORMAT) {
        mr.dropHigh(52).msb ## mr.dropHigh(53).orR
      } {
        mr.dropHigh(23).msb ## mr.dropHigh(24).orR
      }

      val tinyRoundingIncr = VALUE.isNormal && ROUNDMODE.mux(
        FpuRoundMode.RNE -> (tinyRound(1) && (tinyRound(0) || manLsb)),
        FpuRoundMode.RTZ -> False,
        FpuRoundMode.RDN -> (tinyRound =/= 0 && VALUE.sign),
        FpuRoundMode.RUP -> (tinyRound =/= 0 && !VALUE.sign),
        FpuRoundMode.RMM -> (tinyRound(1))
      )
      val tinyOverflow = p.muxDouble(FORMAT) {
        VALUE.mantissa.raw.takeHigh(52).andR
      } {
        VALUE.mantissa.raw.takeHigh(23).andR
      } && tinyRoundingIncr

      val expSet, expZero, expMax, manZero, manSet, manOne, manQuiet, positive = False
      val nx, of, uf = False
      switch(VALUE.mode) {
        is(FloatMode.ZERO) {
          expZero := True
          manZero := True
        }
        is(FloatMode.INF) {
          expSet := True
          manZero := True
        }
        is(FloatMode.NAN) {
          expSet := True
          positive := True
          manZero := True
          manQuiet := VALUE.quiet
        }
        is(FloatMode.NORMAL) {
          when(roundAdjusted =/= 0) {
            nx := True
            when(SUBNORMAL_FINAL || SUBNORMAL && !tinyOverflow) {
              uf := True
            }
          }
          when(EXP_OVERFLOW) {
            nx := True
            of := True
            val doMax = ROUNDMODE.mux(
              FpuRoundMode.RNE -> (False),
              FpuRoundMode.RTZ -> (True),
              FpuRoundMode.RDN -> (!VALUE.sign),
              FpuRoundMode.RUP -> (VALUE.sign),
              FpuRoundMode.RMM -> (False)
            )
            when(doMax) {
              expMax := True
              manSet := True
            } otherwise {
              expSet := True
              manZero := True
            }
          }.elsewhen(EXP_UNDERFLOW) {
            nx := True
            uf := True
            val doMin = ROUNDMODE.mux(
              FpuRoundMode.RNE -> (False),
              FpuRoundMode.RTZ -> (False),
              FpuRoundMode.RDN -> (VALUE.sign),
              FpuRoundMode.RUP -> (!VALUE.sign),
              FpuRoundMode.RMM -> (False)
            ) || ROUNDING_INCR
            when(doMin) {
              expZero := True
              manOne := True
            } otherwise {
              expZero := True
              manZero := True
            }
          }
        }
      }

      val fwb = new Area {
        val flags = FpuFlags()
        val value = Bits(Riscv.FLEN bits)
      }


      for((port, i) <- wbPorts.values.zipWithIndex){
        port.valid := GROUP_OH(i)
        port.payload := fwb.value
      }

      val fpWriter = Flow(RegFileWriter(FloatRegFile))
      fpWriter.valid := GROUP_OH.orR && valid
      fpWriter.data := fwb.value
      fpWriter.uopId := Decode.UOP_ID

      val csr = host[FpuCsrPlugin]
      flagsWb.ats := GROUP_OH.andMask(valid)
      flagsWb.flags.NX := FLAGS.NX || nx
      flagsWb.flags.UF := FLAGS.UF || uf
      flagsWb.flags.OF := FLAGS.OF || of
      flagsWb.flags.DZ := FLAGS.DZ
      flagsWb.flags.NV := FLAGS.NV


      p.whenDouble(FORMAT) {
        fwb.value := VALUE.sign ## EXP.raw.resize(11 bits) ## MAN_RESULT
      } {
        fwb.value(31 downto 0) := VALUE.sign ## EXP.raw.takeLow(8) ## MAN_RESULT.takeHigh(23)
        if (p.rvd) fwb.value(63 downto 32).setAll()
      }

      val wb = fwb.value
      when(expZero) {
        p.whenDouble(FORMAT)(wb(52, 11 bits).clearAll())(wb(23, 8 bits).clearAll())
      }
      when(expSet) {
        p.whenDouble(FORMAT)(wb(52, 11 bits).setAll())(wb(23, 8 bits).setAll())
      }
      when(expMax) {
        p.whenDouble(FORMAT)(wb(52, 11 bits) := 0x7FE)(wb(23, 8 bits) := 0xFE)
      }
      when(manZero) {
        p.whenDouble(FORMAT)(wb(0, 52 bits).clearAll())(wb(0, 23 bits).clearAll())
      }
      when(manOne) {
        p.whenDouble(FORMAT)(wb(0, 52 bits) := 1)(wb(0, 23 bits) := 1)
      }
      when(manSet) {
        p.whenDouble(FORMAT)(wb(0, 52 bits).setAll())(wb(0, 23 bits).setAll())
      }
      when(manQuiet) {
        p.whenDouble(FORMAT)(wb(51) := True)(wb(22) := True)
      }
      when(positive) {
        p.whenDouble(FORMAT)(wb(63) := False)(wb(31) := False)
      }
      if (p.rvd) when(FORMAT === FpuFormat.FLOAT) {
        wb(63 downto 32).setAll()
      }

      ready := !lane.isFreezed()
    }

    pip.build(withoutCollapse = true)
    buildBefore.release()
  }
}
