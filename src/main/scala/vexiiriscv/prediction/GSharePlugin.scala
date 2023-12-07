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
import Fetch._
import vexiiriscv.decode.AlignerPlugin

class GSharePlugin(var historyWidth : Int,
                   var entries : Int = 0,
                   var memBytes : BigInt = null,
                   var readAt : Int = 0,
                   var counterWidth : Int = 2,
                   var readAsync : Boolean = false) extends FiberPlugin with FetchConditionalPrediction with HistoryUser{
  lazy val fpp = host[FetchPipelinePlugin]
  lazy val ls = host[LearnService]
  lazy val dp = host[DispatchPlugin]
  lazy val ap = host[AlignerPlugin]
  buildBefore(fpp.elaborationLock)
  setupRetain(ls.learnLock)
  setupRetain(dp.elaborationLock)
  setupRetain(ap.elaborationLock)

  override def useHistoryAt = readAt
  override def historyWidthUsed = historyWidth
  override def getPredictionAt(stageId: Int) = fpp.fetch(stageId)(GSHARE_COUNTER).map(_.msb)

  val GSHARE_COUNTER = Payload(Vec.fill(SLICE_COUNT)(UInt(counterWidth bits)))

  val logic = during build new Area{
    assert(readAsync == false, "The issue with read async is that it may change while btb stage is stuck, producing transiants => missmatch between pc correction and announced prediction done to BranchPlugin")

    ls.addLearnCtx(GSHARE_COUNTER)
    ls.learnLock.release()

    dp.hmKeys += GSHARE_COUNTER
    dp.elaborationLock.release()

    ap.lastSliceData += GSHARE_COUNTER
    ap.elaborationLock.release()

    var words = entries/SLICE_COUNT
    if(memBytes != null) words = (1 <<(log2Up(memBytes.toInt*8/counterWidth+1)-1)) / SLICE_COUNT
    assert(words != 0)
    assert(isPow2(words))


    def hashWidth = log2Up(words)
    def gshareHash(address : UInt, history : Bits) = address(SLICE_RANGE.get.high + 1, hashWidth bits).reversed ^ U(history).resized

    val mem = new Area{ //TODO bypass read durring write ?
      val counter = Mem.fill(words)(GSHARE_COUNTER)
      val write = counter.writePort
      if(GenerationFlags.simulation){
        counter.initBigInt(List.fill(counter.wordCount)(BigInt(0)))
      }
    }

    val BYPASS = Payload(mem.write)
    val HASH = Payload(UInt(hashWidth bits))

    val readCmd = new fpp.Fetch(readAt){
      HASH := gshareHash(WORD_PC, BRANCH_HISTORY)
      this(BYPASS) := mem.write
    }

    val readRsp = new fpp.Fetch(readAt+1){
      def readMem[T <: Data](mem : Mem[T]) = readAsync match {
        case false => mem.readSync(readCmd(HASH), readCmd.isReady)
        case true  => mem.readAsync(readCmd(HASH))
      }
      this(GSHARE_COUNTER) := readMem(mem.counter)
      when(BYPASS.valid && this(BYPASS).address === HASH){
        this(GSHARE_COUNTER) := this(BYPASS).data
      }

      KeepAttribute(this(GSHARE_COUNTER))
    }

    val onLearn = new Area{
      val cmd = host[LearnService].getLearnPort()
      val hash = gshareHash(cmd.pcOnLastSlice, cmd.history)


      //val counters = mem.counter.readAsync(hash); println("!!!!!!!!!!!! REMOVE THAT READ ASYNC <3 !!!!!!!!!!!!") //This is a 100% accurate implementation
      val counters = cmd.ctx(GSHARE_COUNTER)
      val updated = GSHARE_COUNTER()
      val incrValue = cmd.taken ? U(1) | U((1 << counterWidth)-1)
      val overflow = False
      for(sliceId <- 0 until SLICE_COUNT){
        updated(sliceId) := counters(sliceId) + incrValue.andMask(cmd.pcOnLastSlice(SLICE_RANGE) === sliceId)
        overflow setWhen(cmd.taken && counters(sliceId).msb && !updated(sliceId).msb || !cmd.taken && !counters(sliceId).msb && updated(sliceId).msb)
      }

      mem.write.valid := cmd.valid && cmd.isBranch && !overflow
      mem.write.address := hash
      mem.write.data := updated
    }
  }
}
