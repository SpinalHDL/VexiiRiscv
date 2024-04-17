package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute._
import vexiiriscv.riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class FpuPackerCmd(p : FloatUnpackedParam) extends Bundle{
  val valid = Bool()
  val value = FloatUnpacked(p)
  val format = FpuFormat()
  val roundMode = FpuRoundMode()
}

case class FpuPackerPort(cmd : FpuPackerCmd, at : Int){
  val uops = ArrayBuffer[UopLayerSpec]()
}

class FpuPackerPlugin(lane: ExecuteLanePlugin) extends FiberPlugin{
  val p = FpuUtils

  val elaborationLock = Retainer()

  val ports = ArrayBuffer[FpuPackerPort]()
  def createPort(at : Int, p : FloatUnpackedParam): FpuPackerPort = {
    ports.addRet(FpuPackerPort(FpuPackerCmd(p), at))
  }

  val logic = during setup new Area{
    val wbp = host.find[WriteBackPlugin](_.laneName == lane.laneName)
    val buildBefore = retains(lane.pipelineLock, wbp.elaborationLock)
    awaitBuild()
    val latency = ???

    elaborationLock.await()

    val wbPorts = mutable.LinkedHashMap[Int, Flow[Bits]]()
    val atGroups = ports.groupBy(_.at)
    for((at, l) <- atGroups) {
      val port = wbp.createPort(at+latency)
      wbPorts(at) = port
      for(e <- l; uop <- e.uops) wbp.addMicroOp(port, uop)
    }

    val backend = new Area {
      val pip = new StagePipeline()
      val s0 = new pip.Area(0) {
        val reader = ports.map(_.cmd).reader(ports.map(_.cmd.valid))
        val VALUE = insert(reader(_.value))
        val FORMAT = insert(reader(_.format))
        val ROUNDMODE = insert(reader(_.roundMode))
        valid := ports.map(_.cmd.valid).orR


        val EXP_SUBNORMAL = insert(AFix(p.muxDouble[SInt](FORMAT)(-1023)(-127)))
        val EXP_DIF = insert(EXP_SUBNORMAL - VALUE.exponent)
        val EXP_DIF_PLUS_ONE = insert(U(EXP_SUBNORMAL - VALUE.exponent) + 1)
      }

      import s0._


      val s1 = new pip.Area(1) {
        val SUBNORMAL = insert(EXP_DIF.isPositive() && VALUE.isNormal)
        val MAN_SHIFT_NO_SAT = insert(!SUBNORMAL ?[UInt] 0 | EXP_DIF_PLUS_ONE)
        val MAN_SHIFT = insert(MAN_SHIFT_NO_SAT.sat(widthOf(MAN_SHIFT_NO_SAT) - log2Up(p.mantissaWidth + 2)))
        val MAN_SHIFTED = insert(U(Shift.rightWithScrap(True ## VALUE.mantissa.raw, MAN_SHIFT).dropHigh(1)))

        val f32ManPos = p.mantissaWidth + 2 - 23
        val roundAdjusted = insert(p.muxDouble(FORMAT)(MAN_SHIFTED(0, 2 bits))(MAN_SHIFTED(f32ManPos - 2, 2 bits) | U(MAN_SHIFTED(f32ManPos - 2 - 1 downto 0).orR, 2 bits)))
        val manLsb = insert(p.muxDouble(FORMAT)(MAN_SHIFTED(2))(MAN_SHIFTED(f32ManPos)))
      }
      import s1._

      val s2 = new pip.Area(2) {
        val ROUNDING_INCR = insert(VALUE.isNormal && ROUNDMODE.mux(
          FpuRoundMode.RNE -> (roundAdjusted(1) && (roundAdjusted(0) || manLsb)),
          FpuRoundMode.RTZ -> False,
          FpuRoundMode.RDN -> (roundAdjusted =/= 0 && VALUE.sign),
          FpuRoundMode.RUP -> (roundAdjusted =/= 0 && !VALUE.sign),
          FpuRoundMode.RMM -> (roundAdjusted(1))
        ))
        val incrBy = p.muxDouble(FORMAT)(U(ROUNDING_INCR))(U(ROUNDING_INCR) << p.mantissaWidth - 23)
        val manIncrWithCarry = (MAN_SHIFTED >> 2) +^ incrBy
        val MAN_CARRY = manIncrWithCarry.msb
        val MAN_INCR = (manIncrWithCarry.dropHigh(1))
        val EXP_INCR = insert(VALUE.exponent + AFix(U(MAN_CARRY)))
        val EXP_MAX = insert(AFix(p.muxDouble[SInt](FORMAT)(1023)(127)))
        val EXP_MIN = insert(AFix(p.muxDouble[SInt](FORMAT)(-1023 - 52 + 1)(-127 - 23 + 1)))
        val EXP_OVERFLOW = insert(EXP_INCR > EXP_MAX)
        val EXP_UNDERFLOW = insert(EXP_INCR < EXP_MIN)
        val MAN_RESULT = insert(MAN_INCR)
      }
      import s2._

      val s3 = new pip.Area(3) {
        val SUBNORMAL_FINAL = insert((EXP_SUBNORMAL - EXP_INCR).isPositive())
        val EXP = insert(!SUBNORMAL_FINAL ? (EXP_INCR - EXP_SUBNORMAL) | AFix(0))

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

        val fwb = io.ports(0).floatWriteback
        fwb.valid := isFireing
        fwb.robId := merge.ROBID

        fwb.flags.NX := nx
        fwb.flags.UF := uf
        fwb.flags.OF := of
        fwb.flags.DZ := DZ
        fwb.flags.NV := NV

        p.whenDouble(FORMAT) {
          fwb.value := merge.VALUE.sign ## EXP.raw.resize(11 bits) ## MAN_RESULT
        } {
          fwb.value(31 downto 0) := merge.VALUE.sign ## EXP.raw.takeLow(8) ## MAN_RESULT.takeHigh(23)
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
        when(RAW_ENABLE) {
          wb := RAW
          fwb.flags.clear()
        }
        if (p.rvd) when(FORMAT === FpuFormat.FLOAT) {
          wb(63 downto 32).setAll()
        }
      }
    }

    backend.pip.build()
    buildBefore.release()
  }
}
