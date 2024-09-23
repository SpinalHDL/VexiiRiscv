// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.prediction

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin, InitService, PcService}
import Fetch._
import vexiiriscv.Global._
import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}
import Prediction._
import spinal.core.fiber.Fiber
import vexiiriscv.decode.Decode
import vexiiriscv.execute.BranchPlugin

import scala.util.Random

class BtbPlugin(var sets : Int,
                var chunks : Int,
                var rasDepth : Int = 4,
                var hashWidth : Int = 16,
                var readAt : Int = 0,
                var hitAt : Int = 1,
                var jumpAt : Int = 1,
                var bootMemClear : Boolean) extends FiberPlugin with FetchWordPrediction with InitService  {


  override def useAccurateHistory: Boolean = jumpAt == 1

  def chunksRange = 0 until chunks

  override def initHold(): Bool = bootMemClear.mux(logic.initializer.busy, False)

  val logic = during setup new Area{
    val fpp = host[FetchPipelinePlugin]
    val pcp = host[PcService]
    val rp = host[ReschedulePlugin]
    val dp = host[DispatchPlugin]
    val hp = host.get[HistoryPlugin]

    val buildBefore = retains(fpp.elaborationLock, pcp.elaborationLock)
    val retainer = retains(List(rp.elaborationLock, dp.elaborationLock) ++ hp.map(_.elaborationLock))
    Fiber.awaitBuild()

    val withCondPrediction = host.get[FetchConditionalPrediction].nonEmpty

    val age = fpp.getAge(jumpAt, true)
    val pcPort = pcp.newJumpInterface(age,0, (jumpAt < 2).toInt)
    val historyPort = hp.map(_.newPort(age, 0))
    val flushPort = rp.newFlushPort(fpp.getAge(jumpAt), 0, false)

    dp.hmKeys += Prediction.ALIGNED_JUMPED
    dp.hmKeys += Prediction.ALIGNED_JUMPED_PC
    dp.hmKeys += Prediction.ALIGNED_SLICES_TAKEN
    dp.hmKeys += Prediction.ALIGNED_SLICES_BRANCH

    retainer.release()

    val withRas = rasDepth > 0
    val ras = withRas generate new Area{
      assert(HART_COUNT.get == 1)
      val mem = new Area{
        val stack = Mem.fill(rasDepth)(PC_TARGET)
        if(GenerationFlags.simulation){
          val rand = new Random(42)
          stack.initBigInt(List.fill(stack.wordCount)(BigInt(stack.width, rand)))
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

    assert(chunks <= SLICE_COUNT)
    val SLICE_LOW_RANGE = SLICE_RANGE.get.high - log2Up(chunks) downto SLICE_RANGE.get.low
    val SLICE_HIGH_RANGE = SLICE_RANGE.get.high downto SLICE_RANGE.get.high+1 - log2Up(chunks)
    val SLICE_LOW = Payload(UInt(log2Up(Fetch.SLICE_COUNT/chunks) bits))
    def getSlice(chunkId : Int, low : UInt) = U(chunkId, SLICE_HIGH_RANGE.size bits) @@ low


    def getHash(value : UInt) = value(wordBytesWidth + log2Up(sets), hashWidth bits) //TODO better hash
    case class BtbEntry() extends Bundle {
      val hash = UInt(hashWidth bits)
      val sliceLow  = SLICE_LOW()
      val pcTarget = PC_TARGET()
      val isBranch, isPush, isPop = Bool()
      val taken = !withCondPrediction generate Bool() //TODO remove
    }

    // This memory could be implemented as a single port ram, as that ram is only updated on miss predicted stuff
    val mem = Mem.fill(sets)(Vec.fill(chunks)(BtbEntry())).addAttribute("ram_style", "block")
    if(GenerationFlags.simulation){
      val rand = new Random(42)
      mem.initBigInt(List.fill(mem.wordCount)(BigInt(mem.width, rand)))
//      mem.initBigInt(List.fill(mem.wordCount)(BigInt(0))) //TODO integrate this in some regression, as it exercise bad prediction cutting RVC a lot <3
    }

    val onLearn = new Area{
      val cmd = host[LearnPlugin].getLearnPort()
      val hash = getHash(cmd.pcOnLastSlice)

      val port = mem.writePortWithMask(chunks)
      port.valid := cmd.valid && withCondPrediction.mux(cmd.badPredictedTarget && cmd.taken, cmd.wasWrong || cmd.badPredictedTarget)
      port.address := (cmd.pcOnLastSlice >> wordBytesWidth).resized
      port.mask := UIntToOh(cmd.pcOnLastSlice(SLICE_HIGH_RANGE))
      for(data <- port.data) {
        data.hash := hash
        data.sliceLow := cmd.pcOnLastSlice(SLICE_LOW_RANGE)
        data.pcTarget := cmd.pcTarget >> Fetch.SLICE_RANGE_LOW
        data.isBranch := cmd.isBranch
        data.isPush := cmd.isPush
        data.isPop := cmd.isPop
        if(!withCondPrediction) data.taken := cmd.taken
      }
    }

    val onForget = new Area{
      val cmd = host[ForgetSource].getForgetPort()
      val hash = getHash(cmd.pcOnLastSlice)

      import onLearn.port
      when(cmd.valid){
        port.valid := cmd.valid
        port.address := (cmd.pcOnLastSlice >> wordBytesWidth).resized
        port.mask := UIntToOh(cmd.pcOnLastSlice(SLICE_HIGH_RANGE))
        for(data <- port.data) {
          data.hash := ~hash
          data.sliceLow := cmd.pcOnLastSlice(SLICE_LOW_RANGE)
          data.isBranch := False
          data.isPush := False
          data.isPop := False
          if(!withCondPrediction) data.taken := False
        }
      }
    }

    val readPort = mem.readSyncPort()  //TODO , readUnderWrite = readFirst would save area/ipc/fmax on FPGA which support it, same for gshare
    val readCmd = new fpp.Fetch(readAt){
      readPort.cmd.valid := isReady
      readPort.cmd.payload := (WORD_PC >> wordBytesWidth).resize(mem.addressWidth)
//      val HAZARDS = insert(onLearn.port.mask.andMask(onLearn.port.valid && onLearn.port.address === readPort.cmd.payload))
//      haltWhen(onLearn.port.valid && onLearn.port.address === readPort.cmd.payload) //That create a too long combinatorial path

      val HAZARDS = insert(onLearn.port.mask.andMask(onLearn.port.valid && onLearn.port.address === readPort.cmd.payload))
      haltWhen(onLearn.port.valid) //Omit readPort.cmd.payload to avoid creating long path. Also, this is inline with a single port RW BTB memory.
    }


    val predictions = host.get[FetchConditionalPrediction].map(s =>
      B((0 until SLICE_COUNT).map(s.getPredictionAt(jumpAt)(_)))
    )

    val chunksLogic = for (chunkId <- chunksRange) yield new Area {
      val readRsp = new fpp.Fetch(readAt + 1) {
        val ENTRY = insert(readPort.rsp(chunkId))
        KeepAttribute(this(ENTRY))
      }
      val hitCalc = new fpp.Fetch(hitAt) {
        val HIT = insert(readRsp.ENTRY.hash === getHash(WORD_PC) && getSlice(chunkId, readRsp.ENTRY.sliceLow) >= WORD_PC(SLICE_RANGE.get))
//        HIT clearWhen(readCmd.HAZARDS(chunkId))
        assert(!(isValid && readCmd.HAZARDS(chunkId)))
      }
      val predict = new fpp.Fetch(jumpAt) {
        def pred = withCondPrediction.mux(
          predictions.get(getSlice(chunkId, readRsp.ENTRY.sliceLow)),
          readRsp.ENTRY.taken
        )
        val TAKEN = insert(!readRsp.ENTRY.isBranch || pred)
      }
    }

    if(withRas) ras.readIt := fpp.fetch(jumpAt-1).isReady
    val applyIt = new fpp.Fetch(jumpAt) {
      // Find the first chunk which predicted a PC disruption
      val chunksMask = B(for (self <- chunksLogic) yield self.hitCalc.HIT && chunksLogic.takeWhile(_ != self).map(other => other.hitCalc.HIT && other.predict.TAKEN).norR)
      val chunksTakenOh = B(for (self <- chunksLogic) yield apply(self.predict.TAKEN)) & chunksMask

      val needIt = isValid && chunksTakenOh.orR
      val correctionSent = RegInit(False) setWhen (isValid) clearWhen (up.isReady || up.isCancel)
      val doIt = needIt && !correctionSent
      val entry = OHMux.or(chunksTakenOh, chunksLogic.map(_.readRsp.ENTRY).map(this (_)), bypassIfSingle = true)
      val pcTarget = CombInit(entry.pcTarget)
      if(withRas) when(entry.isPop){ pcTarget := ras.read }
      val doItSlice = OHToUInt(chunksTakenOh) @@ entry.sliceLow

      val rasLogic = withRas generate new Area {
        val pushValid = (doIt && entry.isPush)
        val pushPc = CombInit(apply(WORD_PC))
        pushPc(SLICE_RANGE) := doItSlice
        ras.write.data := (pushPc + SLICE_BYTES.get) >> Fetch.SLICE_RANGE_LOW
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
      pcPort.fault := False
      pcPort.pc := pcTarget << Fetch.SLICE_RANGE_LOW

      WORD_JUMPED := needIt
      WORD_JUMP_SLICE := doItSlice
      WORD_JUMP_PC := pcTarget << Fetch.SLICE_RANGE_LOW

      val history = historyPort.map { port =>
        new Area {
          val layers = List.fill(chunks + 1)(new Area {
            val history = Prediction.BRANCH_HISTORY()
            val valid = Bool()
          })
          layers(0).history := Prediction.BRANCH_HISTORY

          val layersLogic = for (i <- 0 until chunks) yield new Area {
            def e = chunksLogic(i)
            val doIt = chunksMask(i) && e.readRsp.ENTRY.isBranch
            val shifted = layers(i).history.dropHigh(1) ## e.predict.TAKEN
            layers(i + 1).history := doIt.mux(shifted, layers(i).history)
          }

          port.valid := isValid && !correctionSent && chunksLogic.map(e => apply(e.hitCalc.HIT)).orR
          port.history := layers.last.history
        }
      }

      val slicePerChunk = SLICE_COUNT / chunks
      for (chunk <- 0 until chunks) {
        val cl = chunksLogic(chunk)
        for (slice <- 0 until slicePerChunk) {
          val i = slice + chunk * slicePerChunk
          WORD_SLICES_BRANCH(i) := cl.hitCalc.HIT && cl.readRsp.ENTRY.isBranch && cl.readRsp.ENTRY.sliceLow === slice
          WORD_SLICES_TAKEN(i) := cl.predict.TAKEN
        }
      }
    }

    val initializer = bootMemClear generate new Area {
      val counter = Reg(UInt(log2Up(sets max rasDepth) + 1 bits)) init (0)
      val busy = !counter.msb
      when(busy) {
        counter := counter + 1
        onLearn.port.valid := True
        onLearn.port.mask.setAll
        onLearn.port.address := counter.resized
        for (data <- onLearn.port.data) {
          data.hash.setAll
          data.sliceLow := 0
          data.isBranch := False
          data.isPush := False
          data.isPop := False
          if (!withCondPrediction) data.taken := False
        }

        if (withRas) {
          ras.write.valid := True
          ras.write.address := counter.resized
          ras.write.data := 0
        }
      }
    }
    buildBefore.release()
  }
}