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
import vexiiriscv.execute.BranchPlugin
import Fetch._
import vexiiriscv.decode.{AlignerPlugin, AlignerService}

import scala.util.Random

/**
 * Implements conditional branch prediction using a RAM of taken/non-taken bias indexed by the PC xor branch history
 */
class GSharePlugin(var historyWidth : Int,
                   var entries : Int = 0,
                   var memBytes : BigInt = null,
                   var readAt : Int = 0,
                   var counterWidth : Int = 2,
                   var readAsync : Boolean = false,
                   var banksCount : Int = 1,
                   var bootMemClear: Boolean) extends FiberPlugin with FetchConditionalPrediction with HistoryUser with InitService {

  override def useHistoryAt = readAt
  override def historyWidthUsed = historyWidth
  override def getPredictionAt(stageId: Int) = host[FetchPipelinePlugin].fetch(stageId)(GSHARE_COUNTER).map(_.msb)
  override def initHold(): Bool = bootMemClear.mux(logic.initializer.busy, False)

  val GSHARE_COUNTER = Payload(Vec.fill(SLICE_COUNT)(UInt(counterWidth bits)))

  val logic = during setup new Area{
    val fpp = host[FetchPipelinePlugin]
    val ls = host[LearnService]
    val dp = host[DispatchPlugin]
    val ap = host[AlignerService]
    val buildBefore = retains(fpp.elaborationLock)
    val retainer = retains(ls.learnLock, dp.elaborationLock, ap.elaborationLock)
    awaitBuild()

    assert(readAsync == false, "The issue with read async is that it may change while btb stage is stuck, producing transiants => missmatch between pc correction and announced prediction done to BranchPlugin")

    // Specify to the pipeline plugins to carry the GSHARE_COUNTER all the way
    // This will be used when a branch instruction is commited to make RAM learn. (increament/decrement)
    ls.addLearnCtx(GSHARE_COUNTER)
    dp.addDispatchCtx(GSHARE_COUNTER)
    ap.addLastSliceDataCtx(GSHARE_COUNTER)

    retainer.release()

    var words = entries/SLICE_COUNT
    if(memBytes != null) words = (1 <<(log2Up(memBytes.toInt*8/counterWidth+1)-1)) / SLICE_COUNT
    assert(words != 0)
    assert(isPow2(words))


    def hashWidth = log2Up(words)
    def gshareHash(address : UInt, history : Bits) = address(SLICE_RANGE.get.high + 1, hashWidth bits).reversed ^ U(history).resized

    val bankRange = log2Up(words/banksCount) + log2Up(banksCount) - 1 downto log2Up(words/banksCount)
    val mem = new Area{
      assert(isPow2(banksCount))
      val banks = Array.fill(banksCount)(Mem.fill(words/banksCount)(GSHARE_COUNTER))
      val write = Flow(MemWriteCmd(GSHARE_COUNTER, log2Up(words)))
      val writes = banks.map(_.writePort)
      for((sink, i) <- writes.zipWithIndex){
        sink.valid := write.valid && write.address(bankRange) === i
        sink.address := write.address.resized
        sink.data := write.data
      }
      if (GenerationFlags.simulation) {
        val rand = new Random(42)
        banks.foreach(counter => counter.initBigInt(List.fill(counter.wordCount)(BigInt(counter.width, rand))))
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
        case false => mem.readSync(readCmd(HASH).resized, readCmd.isReady)
        case true  => mem.readAsync(readCmd(HASH).resized)
      }

      val readed = Vec(for(counter <- mem.banks) yield readMem(counter))

      this (GSHARE_COUNTER) := readed.read(this(HASH)(bankRange))
      // Unlike the BTB, here we handle the read durring write using a bypass mux.
      when(BYPASS.valid && this(BYPASS).address === HASH){
        this(GSHARE_COUNTER) := this(BYPASS).data
      }

      KeepAttribute(this(GSHARE_COUNTER))
    }

    // Make the a RAM bias increment/decrement using the learn port.
    // To avoid having to read the RAM to know the current bias value, we use the GSHARE_COUNTER provided by the learn port.
    val onLearn = new Area{
      val cmd = host[LearnService].getLearnPort()
      val hash = gshareHash(cmd.pcOnLastSlice, cmd.history)

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

    // Ensure the ram is cleared on reset to avoid x-prop
    val initializer = bootMemClear generate new Area {
      val counter = Reg(UInt(log2Up(words) + 1 bits)) init (0)
      val busy = !counter.msb
      when(busy) {
        counter := counter + 1
        mem.write.valid := True
        mem.write.address := counter.resized
        mem.write.data.clearAll()
      }
    }
    buildBefore.release()
  }
}
