//package vexiiriscv.prediction
//
//import spinal.core._
//import spinal.lib.KeepAttribute
//import spinal.lib.misc.plugin.FiberPlugin
//import spinal.lib.misc.pipeline._
//import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin, PcService}
//import Fetch._
//import vexiiriscv.Global._
//import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}
//import Prediction._
//import vexiiriscv.execute.BranchPlugin
//
//class GSharePlugin(var historyWidth : Int,
//                   var entries : Int = 0,
//                   var memBytes : BigInt = null,
//                   var readAt : Int = 0,
//                   var counterWidth : Int = 2,
//                   var readAsync : Boolean = false) extends FiberPlugin with FetchConditionalPrediction with HistoryUser{
//  lazy val fpp = host[FetchPipelinePlugin]
//  buildBefore(fpp.elaborationLock)
//
//  override def useHistoryAt = readAt
//  override def historyWidthUsed = historyWidth
//  override def getPredictionAt(stageId: Int) = getService[FetchPlugin].getStage(stageId)(setup.GSHARE_COUNTER).map(_.msb)
//
//  val GSHARE_COUNTER = Payload(Vec.fill(SLICE_COUNT)(UInt(counterWidth bits)))
//
//  val logic = during build new Area{
////    val fetch = getService[FetchPlugin]
////    val frontend = getService[FrontendPlugin]
////    val branchContext = getService[BranchContextPlugin]
////    val aligner = getService[AlignerPlugin]
////
////    fetch.retain()
////    frontend.retain()
////    branchContext.retain()
////
////
////    val keys = new AreaRoot {
////      val GSHARE_COUNTER = Stageable(Vec.fill(SLICE_COUNT)(UInt(counterWidth bits)))
////    }
////
////    aligner.addLastWordContext(
////      GSHARE_COUNTER
////    )
////    branchContext.dispatchWrite(
////      GSHARE_COUNTER
////    )
//
//
//    var words = entries/SLICE_COUNT
//    if(memBytes != null) words = (1 <<(log2Up(memBytes.toInt*8/counterWidth+1)-1)) / SLICE_COUNT
//    assert(words != 0)
//    assert(isPow2(words))
//
//
//    def hashWidth = log2Up(words)
//    def gshareHash(address : UInt, history : Bits) = address(SLICE_RANGE.get.high + 1, hashWidth bits).reversed ^ U(history).resized
//
//    val mem = new Area{ //TODO bypass read durring write ?
//      val counter = Mem.fill(words)(GSHARE_COUNTER)
//      val write = counter.writePort
//      if(GenerationFlags.simulation){
//        counter.initBigInt(List.fill(counter.wordCount)(BigInt(0)))
//      }
//    }
//
//    val BYPASS = Stageable(cloneOf(mem.write))
//    val HASH = Stageable(UInt(hashWidth bits))
//
//    val readCmd = new Area{
//      val stage = fetch.getStage(readAt)
//      import stage._
//
//      HASH := gshareHash(FETCH_PC, BRANCH_HISTORY)
//      stage(BYPASS) := mem.write
//    }
//
//    val readRsp = new Area{
//      val stage = fetch.getStage(readAt+1)
//      import stage._
//
//      def readMem[T <: Data](mem : Mem[T], address : UInt = readCmd.stage(HASH)) = readAsync match {
//        case false => mem.readSync(address, readCmd.stage.isReady)
//        case true  => mem.readAsync(address)
//      }
//      stage(GSHARE_COUNTER) := readMem(mem.counter)
//      when(BYPASS.valid && stage(BYPASS).address === HASH){
//        stage(GSHARE_COUNTER) := stage(BYPASS).data
//      }
//
//      KeepAttribute(stage(GSHARE_COUNTER))
//    }
//
//    val onDecompressed = new Area{
//      val stage = frontend.pipeline.decompressed
//
//      for(slotId <- 0 until Frontend.DECODE_COUNT) {
//        stage(CONDITIONAL_TAKE_IT, slotId) := stage(GSHARE_COUNTER, slotId).map(_.msb).asBits
//      }
//    }
//
//    val onLearn = new Area{
//      val ctx = branchContext.learnRead(branchContext.keys.BRANCH_FINAL)
//      val hash = gshareHash(ctx.pcOnLastSlice, branchContext.learnRead(BRANCH_HISTORY))
//
//      val counters = branchContext.learnRead(GSHARE_COUNTER)
//      val updated = GSHARE_COUNTER()
//      val incrValue = ctx.taken ? U(1) | U((1 << counterWidth)-1)
//      val overflow = False
//      for(sliceId <- 0 until SLICE_COUNT){
//        updated(sliceId) := counters(sliceId) + incrValue.andMask(ctx.pcOnLastSlice(SLICE_RANGE) === sliceId)
//        overflow setWhen(ctx.taken && counters(sliceId).msb && !updated(sliceId).msb || !ctx.taken && !counters(sliceId).msb && updated(sliceId).msb)
//      }
//
//      mem.write.valid := branchContext.learnValid && branchContext.learnRead(IS_BRANCH) && !overflow
//      mem.write.address := hash
//      mem.write.data := updated
//    }
//
//    fetch.release()
//    frontend.release()
//    branchContext.release()
//  }
//}
