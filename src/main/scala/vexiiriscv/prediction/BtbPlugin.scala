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

class BtbPlugin(var sets : Int,
                var ways : Int,
                var rasDepth : Int = 4,
                var hashWidth : Int = 16,
                var readAt : Int = 0,
                var hitAt : Int = 1,
                var jumpAt : Int = 1) extends FiberPlugin with FetchWordPrediction {
  lazy val fpp = host[FetchPipelinePlugin]
  lazy val pcp = host[PcService]
  lazy val rp = host[ReschedulePlugin]
  lazy val dp = host[DispatchPlugin]
  lazy val hp = host.get[HistoryPlugin]
  buildBefore(fpp.elaborationLock)
  buildBefore(pcp.elaborationLock)
  setupRetain(rp.elaborationLock)
  setupRetain(dp.elaborationLock)
  during setup(hp.map(_.elaborationLock.retain()))

  def waysRange = 0 until ways

  val logic = during build new Area{
    val age = fpp.getAge(jumpAt, true)
    val pcPort = pcp.createJumpInterface(age,0, (jumpAt < 2).toInt)
    val flushPort = rp.newFlushPort(age, 0, false)
    val historyPort = hp.map(_.createPort(age))

    dp.hmKeys += Prediction.ALIGNED_JUMPED
    dp.hmKeys += Prediction.ALIGNED_JUMPED_PC

    dp.elaborationLock.release()
    rp.elaborationLock.release()
    hp.foreach(_.elaborationLock.release())

    val withRas = rasDepth > 0
    val ras = withRas generate new Area{
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

    assert(ways <= SLICE_COUNT)
    val SLICE_LOW_RANGE = SLICE_RANGE.get.high - log2Up(ways) downto SLICE_RANGE.get.low
    val SLICE_HIGH_RANGE = SLICE_RANGE.get.high downto SLICE_RANGE.get.high+1 - log2Up(ways)
    val SLICE_LOW = Payload(UInt(log2Up(Fetch.SLICE_COUNT/ways) bits))
    def getSlice(wayId : Int, low : UInt) = U(wayId, SLICE_HIGH_RANGE.size bits) @@ low


    def getHash(value : UInt) = value(wordBytesWidth + log2Up(sets), hashWidth bits) //TODO better hash
    case class BtbEntry() extends Bundle {
      val hash = UInt(hashWidth bits)
      val sliceLow  = SLICE_LOW()
      val pcTarget = PC()
      val isBranch, isPush, isPop = Bool()
      val taken = Bool() //TODO remove
    }

    val mem = Mem.fill(sets)(Vec.fill(ways)(BtbEntry())) //TODO bypass read durring write ?
    val rand = new Random(42)
    if(GenerationFlags.simulation){
      mem.initBigInt(List.fill(mem.wordCount)(BigInt(mem.width, rand)))
    }

    val onLearn = new Area{
      val cmd = host[BranchPlugin].logic.jumpLogic.learn
      val hash = getHash(cmd.pcOnLastSlice)

      val port = mem.writePortWithMask(ways)
      port.valid := cmd.valid
      port.address := (cmd.pcOnLastSlice >> wordBytesWidth).resized
      port.mask := UIntToOh(cmd.pcOnLastSlice(SLICE_HIGH_RANGE))
      for(data <- port.data) {
        data.hash := hash
        data.sliceLow := cmd.pcOnLastSlice(SLICE_LOW_RANGE)
        data.pcTarget := cmd.pcTarget
        data.isBranch := cmd.isBranch
        data.isPush := cmd.isPush
        data.isPop := cmd.isPop
        data.taken := cmd.taken
      }
    }

    val readPort = mem.readSyncPort()  //TODO , readUnderWrite = readFirst
    val readCmd = new fpp.Fetch(readAt){
      readPort.cmd.valid := isReady
      readPort.cmd.payload := (WORD_PC >> wordBytesWidth).resize(mem.addressWidth)
    }


    val predictions = host.get[FetchConditionalPrediction].map(s =>
      B((0 until SLICE_COUNT).map(s.getPredictionAt(jumpAt)(_)))
    )

    val waysLogic = for (wayId <- waysRange) yield new Area {
      val readRsp = new fpp.Fetch(readAt + 1) {
        val ENTRY = insert(readPort.rsp(wayId))
        KeepAttribute(this(ENTRY))
      }
      val hitCalc = new fpp.Fetch(hitAt) {
        val HIT = insert(readRsp.ENTRY.hash === getHash(WORD_PC) && getSlice(wayId, readRsp.ENTRY.sliceLow) >= WORD_PC(SLICE_RANGE.get))
      }
      val predict = new fpp.Fetch(jumpAt) {
        def pred = host.get[FetchConditionalPrediction] match {
          case Some(x) => predictions.get(getSlice(wayId, readRsp.ENTRY.sliceLow))
          case None => readRsp.ENTRY.taken
        }
        val TAKEN = insert(!readRsp.ENTRY.isBranch || pred)
      }
    }

    if(withRas) ras.readIt := fpp.fetch(jumpAt-1).isReady
    val applyIt = new fpp.Fetch(jumpAt) {
      // Find the first way which predicted a PC disruption
      val waysMask = B(for (self <- waysLogic) yield self.hitCalc.HIT && waysLogic.takeWhile(_ != self).map(other => other.hitCalc.HIT && other.predict.TAKEN).norR)
      val waysTakenOh = B(for (self <- waysLogic) yield apply(self.predict.TAKEN)) & waysMask


      val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
        val skip = RegInit(False)
      }
      when(up.isMoving) {
        harts.onSel(HART_ID)(_.skip := False)
      }
      for (skipTrigger <- host[DecodePredictionPlugin].logic.flushPorts) {
        when(skipTrigger.valid) {
          harts.onSel(skipTrigger.hartId)(_.skip := True)
        }
      }

      val gotSkip = harts.map(_.skip).read(HART_ID)
      val needIt = isValid && !gotSkip && waysTakenOh.orR
      val correctionSent = RegInit(False) setWhen (isValid) clearWhen (up.ready || up.cancel)
      val doIt = needIt && !correctionSent
      val entry = OHMux.or(waysTakenOh, waysLogic.map(_.readRsp.ENTRY).map(this (_)), bypassIfSingle = true)
      val pcTarget = CombInit(entry.pcTarget)
      if(withRas) when(entry.isPop){ pcTarget := ras.read }
      val doItSlice = OHToUInt(waysTakenOh) @@ entry.sliceLow

      val rasLogic = withRas generate new Area {
        val pushValid = (doIt && entry.isPush)
        val pushPc = CombInit(apply(WORD_PC))
        pushPc(SLICE_RANGE) := doItSlice
        ras.write.data := pushPc + SLICE_BYTES.get
        ras.ptr.pushIt setWhen (pushValid)
        ras.ptr.popIt setWhen(doIt && entry.isPop)
      }

//      val pushPc = CombInit(this (WORD_PC))
//      pushPc(SLICE_RANGE) := doItSlice
//      val pushValue = pushPc + SLICE_BYTES.get
//
//      if (withRas) {
//        ras.write.data := pushValue
//        when(doIt) {
//          ras.ptr.pushIt := entry.isPush
//          ras.ptr.popIt := entry.isPop
//        }
//      }


      flushPort.valid := doIt
      flushPort.self := False
      flushPort.hartId := HART_ID

      pcPort.valid := doIt
      pcPort.pc := pcTarget

      WORD_JUMPED := needIt
      WORD_JUMP_SLICE := doItSlice
      WORD_JUMP_PC := pcTarget

      val history = historyPort.map { port =>
        new Area {
          val layers = List.fill(ways + 1)(new Area {
            val history = Prediction.BRANCH_HISTORY()
            val valid = Bool()
          })
          layers(0).history := Prediction.BRANCH_HISTORY

          val layersLogic = for (i <- 0 until ways) yield new Area {
            def e = waysLogic(i)

            val doIt = waysMask(i) && e.readRsp.ENTRY.isBranch
            val shifted = layers(i).history.dropHigh(1) ## e.predict.TAKEN
            layers(i + 1).history := doIt.mux(shifted, layers(i).history)
          }

          port.valid := isValid && !gotSkip && !correctionSent && waysLogic.map(e => apply(e.hitCalc.HIT)).orR
          port.history := layers.last.history
        }
      }
    }
  }
}