// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.prediction

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin, PcService}
import Fetch._
import vexiiriscv.Global._
import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}
import Prediction._
import vexiiriscv.execute.BranchPlugin

import scala.util.Random

class BtbPlugin(var entries : Int,
                var hashWidth : Int = 16,
                var readAt : Int = 0,
                var hitAt : Int = 1,
                var jumpAt : Int = 1) extends FiberPlugin with FetchWordPrediction {
  lazy val fpp = host[FetchPipelinePlugin]
  lazy val pcp = host[PcService]
  lazy val rp = host[ReschedulePlugin]
  lazy val dp = host[DispatchPlugin]
  buildBefore(fpp.elaborationLock)
  buildBefore(pcp.elaborationLock)
  setupRetain(rp.elaborationLock)
  setupRetain(dp.elaborationLock)

  val logic = during build new Area{
    val age = fpp.getAge(jumpAt, true)
    val pcPort = pcp.createJumpInterface(age,0, (jumpAt < 2).toInt)
    val flushPort = rp.newFlushPort(age, 0, false)

    dp.hmKeys += Prediction.ALIGNED_JUMPED
    dp.hmKeys += Prediction.ALIGNED_JUMPED_PC

    dp.elaborationLock.release()
    rp.elaborationLock.release()


    val wordBytesWidth = log2Up(Fetch.WORD_WIDTH/8)

    def getHash(value : UInt) = value(wordBytesWidth, hashWidth bits) //TODO better hash
    case class BtbEntry() extends Bundle {
      val hash = UInt(hashWidth bits)
      val slice  = UInt(log2Up(Fetch.SLICE_COUNT) bits)
      val pcTarget = PC()
      val isBranch = Bool()
      val taken = Bool() //TODO remove
    }

    val ENTRY = Payload(BtbEntry())
    val HIT = Payload(Bool())
    val mem = Mem.fill(entries)(BtbEntry()) //TODO bypass read durring write ?
    val rand = new Random(42)
    if(GenerationFlags.simulation){
      mem.initBigInt(List.fill(mem.wordCount)(BigInt(mem.width, rand)))
    }

    val onLearn = new Area{
      val cmd = host[BranchPlugin].logic.jumpLogic.btb.learn
      val hash = getHash(cmd.pcOnLastSlice)

      val port = mem.writePort
      port.valid := cmd.valid
      port.address := (cmd.pcOnLastSlice >> wordBytesWidth).resized
      port.data.hash := hash
      port.data.slice := (cmd.pcOnLastSlice >> SLICE_RANGE_LOW).resized
      port.data.pcTarget := cmd.pcTarget
      port.data.isBranch := cmd.isBranch
      port.data.taken := cmd.taken
    }

    val readCmd = new fpp.Fetch(readAt){
      val entryAddress = (WORD_PC >> wordBytesWidth).resize(mem.addressWidth)
    }

    val readRsp = new fpp.Fetch(readAt+1){
      ENTRY := mem.readSync(readCmd.entryAddress, readCmd.isReady) //TODO , readUnderWrite = readFirst
      KeepAttribute(this(ENTRY))
    }

    val hitCalc = new fpp.Fetch(hitAt){
      val postPcPrediction = WORD_PC(SLICE_RANGE.get) > ENTRY.slice
      HIT := ENTRY.hash === getHash(WORD_PC) && !postPcPrediction
    }

    val applyIt = new fpp.Fetch(jumpAt){
      val prediction = !ENTRY.isBranch || ENTRY.taken//TODO
//      val prediction = getServiceOption[FetchConditionalPrediction] match {
//        case Some(s) => s.getPredictionAt(jumpAt)(ENTRY.slice)
//        case None => True
//      }

      val skipTrigger = host[DecodePredictionPlugin].logic.flushPort
      val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
        val skip = RegInit(False)
      }
      when(up.isMoving) {
        harts.onSel(HART_ID)(_.skip := False)
      }
      when(skipTrigger.valid){
        harts.onSel(skipTrigger.hartId)(_.skip := True)
      }

      val gotSkip = harts.map(_.skip).read(HART_ID)
      val needIt = isValid && !gotSkip && HIT && !(ENTRY.isBranch && !prediction)
      val correctionSent = RegInit(False) setWhen(isValid) clearWhen(up.ready || up.cancel)
      val doIt = needIt && !correctionSent

      flushPort.valid := doIt
      flushPort.self := False
      flushPort.hartId := HART_ID

      pcPort.valid := doIt
      pcPort.pc := ENTRY.pcTarget

      WORD_JUMPED := needIt
      WORD_JUMP_SLICE := ENTRY.slice
      WORD_JUMP_PC := ENTRY.pcTarget
//
//      setup.historyPush.flush    := isValid && HIT && ENTRY.isBranch && !correctionSent
//      setup.historyPush.mask(0)  := setup.historyPush.flush
//      setup.historyPush.taken(0) := prediction
//
//      BRANCH_HISTORY_PUSH_VALID := setup.historyPush.flush
//      BRANCH_HISTORY_PUSH_SLICE := ENTRY.slice
//      BRANCH_HISTORY_PUSH_VALUE := prediction
    }
  }
}