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
                var rasDepth : Int = 4,
                var forceNotTaken: Boolean = false, //For debug/simulation purposes
                var forceTaken : Boolean = false, //For debug/simulation purposes
                var hashWidth : Int = 16,
                var readAt : Int = 0,
                var hitAt : Int = 1,
                var jumpAt : Int = 1) extends FiberPlugin with FetchWordPrediction {
  lazy val fpp = host[FetchPipelinePlugin]
  lazy val pcp = host[PcService]
  lazy val rp = host[ReschedulePlugin]
  lazy val dp = host[DispatchPlugin]
  lazy val hp = host[HistoryPlugin]
  buildBefore(fpp.elaborationLock)
  buildBefore(pcp.elaborationLock)
  setupRetain(rp.elaborationLock)
  setupRetain(dp.elaborationLock)
  setupRetain(hp.elaborationLock)

  val logic = during build new Area{
    val age = fpp.getAge(jumpAt, true)
    val pcPort = pcp.createJumpInterface(age,0, (jumpAt < 2).toInt)
    val flushPort = rp.newFlushPort(age, 0, false)
    val historyPort = hp.createPort(age)

    dp.hmKeys += Prediction.ALIGNED_JUMPED
    dp.hmKeys += Prediction.ALIGNED_JUMPED_PC

    dp.elaborationLock.release()
    rp.elaborationLock.release()
    hp.elaborationLock.release()



    val ras = new Area{
      assert(HART_COUNT.get == 1)
      val mem = new Area{
        val stack = Mem.fill(rasDepth)(PC)
        if(GenerationFlags.simulation){
          stack.initBigInt(List.fill(stack.wordCount)(BigInt(0)))
        }
      }
      val ptr = new Area{
        val push = Reg(UInt(log2Up(rasDepth) bits)) init(0)
        val pop = Reg(UInt(log2Up(rasDepth) bits)) init(rasDepth-1)
        val pushIt, popIt = False

        push := push + U(pushIt) - U(popIt)
        pop  := pop + U(pushIt) - U(popIt)
      }
      val readIt = Bool()
      val read = RegNextWhen(mem.stack.readAsync(ptr.pop.getAheadValue()), readIt)
      val write = mem.stack.writePort
      write.valid := ptr.pushIt
      write.address := ptr.push
      write.data.assignDontCare()

      //Restore the RAS ptr on reschedules
//      val reschedule = commit.reschedulingPort(onCommit = false)
//      val healPush = rob.readAsyncSingle(RAS_PUSH_PTR, reschedule.robId)
//      val healPop  = healPush-1
//      when(reschedule.valid){
//        ptr.push := healPush
//        ptr.pop  := healPop
//      }
    }

    val wordBytesWidth = log2Up(Fetch.WORD_WIDTH/8)

    def getHash(value : UInt) = value(wordBytesWidth + log2Up(entries), hashWidth bits) //TODO better hash
    case class BtbEntry() extends Bundle {
      val hash = UInt(hashWidth bits)
      val slice  = UInt(log2Up(Fetch.SLICE_COUNT) bits)
      val pcTarget = PC()
      val isBranch, isPush, isPop = Bool()
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
      val cmd = host[BranchPlugin].logic.jumpLogic.learn
      val hash = getHash(cmd.pcOnLastSlice)

      val port = mem.writePort
      port.valid := cmd.valid
      port.address := (cmd.pcOnLastSlice >> wordBytesWidth).resized
      port.data.hash := hash
      port.data.slice := (cmd.pcOnLastSlice >> SLICE_RANGE_LOW).resized
      port.data.pcTarget := cmd.pcTarget
      port.data.isBranch := cmd.isBranch
      port.data.isPush := cmd.isPush
      port.data.isPop := cmd.isPop
      port.data.taken := cmd.taken
    }

    val readPort = mem.readSyncPort()  //TODO , readUnderWrite = readFirst
    val readCmd = new fpp.Fetch(readAt){
      readPort.cmd.valid := isReady
      readPort.cmd.payload := (WORD_PC >> wordBytesWidth).resize(mem.addressWidth)
    }

    val readRsp = new fpp.Fetch(readAt+1){
      ENTRY := readPort.rsp
      KeepAttribute(this(ENTRY))
    }

    val hitCalc = new fpp.Fetch(hitAt){
      val postPcPrediction = WORD_PC(SLICE_RANGE.get) > ENTRY.slice
      HIT := ENTRY.hash === getHash(WORD_PC) && !postPcPrediction
    }

    ras.readIt := fpp.fetch(jumpAt-1).isReady
    val applyIt = new fpp.Fetch(jumpAt){
//      val prediction = True
//      val prediction = !ENTRY.isBranch || ENTRY.taken //TODO
      val prediction = host.get[FetchConditionalPrediction] match {
        case Some(s) => s.getPredictionAt(jumpAt)(ENTRY.slice)
        case None => True
      }

      val harts = for(hartId <- 0 until HART_COUNT) yield new Area{
        val skip = RegInit(False)
      }
      when(up.isMoving) {
        harts.onSel(HART_ID)(_.skip := False)
      }
      for(skipTrigger <- host[DecodePredictionPlugin].logic.flushPorts) {
        when(skipTrigger.valid) {
          harts.onSel(skipTrigger.hartId)(_.skip := True)
        }
      }

      val gotSkip = harts.map(_.skip).read(HART_ID)
      val needIt = isValid && !gotSkip && HIT && !(ENTRY.isBranch && !prediction)
      val correctionSent = RegInit(False) setWhen(isValid) clearWhen(up.ready || up.cancel)
      val doIt = needIt && !correctionSent
      val pcTarget = ENTRY.isPop.mux(ras.read, ENTRY.pcTarget)

      println("!!!!!!!!!!!!!!! OPTIMIZE ME !!!!!!!!!!!!")
      val pushPc = CombInit(this(WORD_PC))
      pushPc(SLICE_RANGE) := ENTRY.slice
      val pushValue = pushPc + SLICE_BYTES.get

      ras.write.data := pushValue
      when(isValid && !gotSkip && HIT){
        ras.ptr.pushIt := ENTRY.isPush
        ras.ptr.popIt := ENTRY.isPop
      }

      flushPort.valid := doIt
      flushPort.self := False
      flushPort.hartId := HART_ID

      pcPort.valid := doIt
      pcPort.pc := pcTarget

      historyPort.valid := isValid && !gotSkip && HIT && ENTRY.isBranch
      historyPort.history := (Prediction.BRANCH_HISTORY ## prediction).resized

      WORD_JUMPED := needIt
      WORD_JUMP_SLICE := ENTRY.slice
      WORD_JUMP_PC := pcTarget
//
//      setup.historyPush.flush    := isValid && HIT && ENTRY.isBranch && !correctionSent
//      setup.historyPush.mask(0)  := setup.historyPush.flush
//      setup.historyPush.taken(0) := prediction
//
//      BRANCH_HISTORY_PUSH_VALID := setup.historyPush.flush
//      BRANCH_HISTORY_PUSH_SLICE := ENTRY.slice
//      BRANCH_HISTORY_PUSH_VALUE := prediction

      if(forceTaken) prediction.removeAssignments() := True
      if(forceNotTaken) prediction.removeAssignments() := False
    }
  }
}