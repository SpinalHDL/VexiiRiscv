package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.fetch.{Fetch, InitService}
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.Global._
import vexiiriscv.execute.{CsrAccessPlugin, CsrService}
import vexiiriscv.schedule.DispatchPlugin

case class PrefetchCmd() extends Bundle {
  val address = LsuL1.MIXED_ADDRESS()
  val unique = Bool()
}

case class LsuCommitProbe() extends Bundle {
  val pc = Global.PC()
  val address = LsuL1.MIXED_ADDRESS()
  val load, store, trap, io, prefetchFailed, miss = Bool()
}


abstract class PrefetcherPlugin extends FiberPlugin {
  val io = during build Stream(PrefetchCmd())
}

class PrefetcherNextLinePlugin extends PrefetcherPlugin {
  val logic = during build new Area {
    val lsu = host[LsuService]
    val probe = lsu.lsuCommitProbe
    val converted = Stream(PrefetchCmd())
    converted.arbitrationFrom(probe.toStream)
    converted.address := probe.address + lsu.getBlockSize
    converted.unique := probe.store
    io << converted.stage()
  }
}


/**
 * Implements load/store predictor for the LSU.
 * See https://spinalhdl.github.io/VexiiRiscv-RTD/master/VexiiRiscv/Memory/index.html#prefetchrptplugin for more details
 */
case class PrefetcherRptParam (
  var sets : Int = 128,
  var readAt : Int = 0,
  var tagAt: Int = 1,
  var ctrlAt: Int = 2,
  var addAt: Int = 1,
  var prefetchAt: Int = 1,
  var tagWidth: Int = 15,
  var addressWidth: Int = 16,
  var strideWidth: Int = 12,
  var blockAheadMax: Int = 4,
  var scoreMax: Int = 31,
  var scorePass: Int = 1,
  var scoreFailShift: Int = 1,
  var scoreConflict: Int = 2,
  var scoreOffset: Int = 3,
  var scoreShift: Int = 0,
  var queueSize: Int = 4
)
class PrefetcherRptPlugin(p : PrefetcherRptParam,  bootMemClear : Boolean = false) extends PrefetcherPlugin  with InitService {
  import p._

  override def initHold(): Bool = bootMemClear.mux(logic.initializer.busy, False)

  val logic = during setup new Area {
    val lsu = host[LsuService]
    val cp = host[CsrService]
    val dp = host[DispatchPlugin]
    val earlyLock = retains(cp.csrLock)
    awaitBuild()

    val csr = new Area{
      val disable = RegInit(False)
      cp.readWrite(0x7FF, 1 -> disable)
    }

    earlyLock.release()

    val TAG = Payload(UInt(tagWidth bits))
    val STRIDE = Payload(SInt(strideWidth bits))
    val STRIDE_EXTENDED = Payload(SInt(addressWidth bits))
    val SCORE = Payload(UInt(log2Up(scoreMax + 1) bits))
    val ADDRESS = Payload(UInt(addressWidth bits))
    val ADVANCE = Payload(UInt(log2Up(blockAheadMax + 1) bits))
    val PROBE = Payload(LsuCommitProbe())
    val ENTRY = Payload(Entry())
    val TAG_HIT, STRIDE_HIT, NEW_BLOCK = Payload(Bool())

    case class PrefetchPacked() extends Bundle {
      val address = LsuL1.MIXED_ADDRESS()
      val unique = Bool()
      val from = ADVANCE()
      val to = ADVANCE()
      val stride = STRIDE()
    }

    val order = Stream(PrefetchPacked())
    val (queue, occupancy) = order.queueWithOccupancy(queueSize, latency = 1)
    val queued = queue.combStage()
    val counter = Reg(ADVANCE) init(0)
    val advanceAt = (queued.from + counter)
    val done = advanceAt === queued.to
    val pip2 = new StagePipeline(){
      node(0).arbitrateFrom(queued.forkSerial(done))
      counter := (counter + U(node(0).isFiring)).andMask(!queued.ready)
      val CMD = node(0).insert(queued.payload)
      val MUL = node(0).insert(advanceAt.intoSInt * queued.stride)
      val adder = new Area(addAt){
        val ADDR = insert(U(S(CMD.address) + MUL))
      }
      val result = new Area(prefetchAt){
        val serialized = Stream(PrefetchCmd())
        arbitrateTo(io)
        io.get.address := adder.ADDR
        io.get.unique := CMD.unique
      }
    }
    pip2.build()


    //Dispatch throttling to ensure some prefetching goes through when the instruction stream is very heavy in load/store
    dp.haltDispatchWhen(RegNext(occupancy > queueSize/2) init(False))

    def hashAddress(pc: UInt) = pc(Fetch.SLICE_RANGE_LOW, log2Up(sets) bits)
    def hashTag(pc: UInt) = pc(Fetch.SLICE_RANGE_LOW.get + log2Up(sets), tagWidth bits)

    case class Entry() extends Bundle {
      val tag     = TAG()
      val address = ADDRESS()
      val stride  = STRIDE()
      val score   = SCORE()
      val advance = ADVANCE()
      val missed  = Bool()
    }
    val storage = new Area {
      val ram = Mem.fill(sets)(Entry())
      val read = ram.readSyncPort()
      val write = ram.writePort()
    }

    // Define the pipeline which will be used to access the RPT ram and process it.
    val pip = new StagePipeline()
    val insert = new pip.Area(0){
      arbitrateFrom(lsu.lsuCommitProbe.throwWhen(lsu.lsuCommitProbe.io))
      PROBE := lsu.lsuCommitProbe.payload
    }

    val onRead0 = new pip.Area(readAt){
      storage.read.cmd.valid := isFiring
      storage.read.cmd.payload := hashAddress(PROBE.pc)
      val WRITTEN = insert(storage.write)
    }
    val onRead1 = new pip.Area(readAt+1) {
      ENTRY := storage.read.rsp
      when(onRead0.WRITTEN.valid && onRead0.WRITTEN.payload.address === hashAddress(PROBE.pc)){
        ENTRY := onRead0.WRITTEN.payload.data
      }
    }
    val onTag = new pip.Area(tagAt) {
      TAG_HIT := ENTRY.tag === hashTag(PROBE.pc)
      STRIDE_EXTENDED := S(PROBE.address - ENTRY.address).resized
      NEW_BLOCK := (PROBE.address.resized ^ ENTRY.address) >> log2Up(lsu.getBlockSize) =/= 0
    }
    val onCtrl = new pip.Area(ctrlAt){
      STRIDE_HIT := STRIDE_EXTENDED.resized === ENTRY.stride && STRIDE_EXTENDED.dropLow(strideWidth).asBools.map(_ === ENTRY.stride.msb).andR
      STRIDE := STRIDE_EXTENDED.resized

      val unfiltred = cloneOf(order)
      val lastOrder = order.toFlowFire.toReg()
      val tagRange = PHYSICAL_WIDTH-1 downto log2Up(lsu.getBlockSize)
      order << unfiltred .throwWhen(lastOrder.address(tagRange) === order.address(tagRange) && lastOrder.unique && order.unique && lastOrder.from === order.from && lastOrder.to === order.to)

      val add, sub = SCORE()
      add := 0
      sub := 0
      val score = ENTRY.score -| sub +| add

      val advanceSubed = (ENTRY.advance.andMask(!PROBE.miss) -| U(NEW_BLOCK))
      val advanceAllowed = (ENTRY.score -| scoreOffset) >> scoreShift
      val orderAsk = False

      storage.write.valid         := isFiring && !PROBE.prefetchFailed
      storage.write.address       := hashAddress(PROBE.pc)
      storage.write.data.tag      := ENTRY.tag
      storage.write.data.address  := PROBE.address.resized
      storage.write.data.stride   := (ENTRY.score < scoreOffset).mux[SInt](STRIDE, ENTRY.stride)
      storage.write.data.score    := score
      storage.write.data.advance  := unfiltred.fire.mux(unfiltred.to, advanceSubed).resized
      storage.write.data.missed   := PROBE.miss || ENTRY.missed && STRIDE_HIT

      unfiltred.valid   := isFiring && orderAsk && !PROBE.prefetchFailed && storage.write.data.missed
      unfiltred.address := PROBE.address
      unfiltred.unique  := PROBE.store
      unfiltred.from    := PROBE.miss.mux(U(0), advanceSubed+1)
      unfiltred.to      := advanceAllowed.min(blockAheadMax).resized
      unfiltred.stride  := ENTRY.stride.msb.mux(ENTRY.stride min -lsu.getBlockSize, ENTRY.stride max lsu.getBlockSize)

      when(!TAG_HIT){
        when(STRIDE =/= 0) {
          when(ENTRY.score =/= 0) {
            sub := scoreConflict
          } otherwise {
            storage.write.data.tag := hashTag(PROBE.pc)
            storage.write.data.score := 0
            storage.write.data.stride := 0
            storage.write.data.advance := 0
          }
        }
      } otherwise {
        when(!STRIDE_HIT){
          sub := ENTRY.score |>> scoreFailShift
          advanceSubed := 0
        } otherwise {
          when(NEW_BLOCK){
            add := scorePass
          }
        }
        when(advanceSubed < blockAheadMax && advanceSubed < advanceAllowed){
          orderAsk := True
        }
      }

      when(csr.disable){
        order.valid := False
        storage.write.valid := False
      }
    }

    // Initialize the ram to avoid simulation x-prop
    val initializer = bootMemClear generate new Area {
      val counter = Reg(UInt(log2Up(sets) + 1 bits)) init (0)
      val busy = !counter.msb
      when(busy) {
        counter := counter + 1
        storage.write.valid := True
        storage.write.address := counter.resized
        storage.write.data.clearAll()
      }
    }

    pip.build()
  }
}


/*
Benchmark from ext/NaxSoftware/baremetal/prefetch_c :
--load-elf ext/NaxSoftware/baremetal/prefetch_c/build/rv64ima/prefetch_c.elf --memory-latency=30 --no-rvls-check --xlen=64 --max-ipc --performance-counters=0 --lsu-l1-mem-data-width-min=128 --with-lsu-l1 --with-rvm
L 1x416s 1.05 B/cyc 3884 cyc
L 1x512s 0.86 B/cyc 4752 cyc
L 1x512ms 0.84 B/cyc 4838 cyc
L 1x 2.56 B/cyc 6395 cyc
L 1x 2.58 B/cyc 6342 cyc
L 4x 4.88 B/cyc 3352 cyc
L 16x 5.47 B/cyc 2994 cyc
L 16x 6.84 B/cyc 9575 cyc
S 1x512s 0.71 B/cyc 5713 cyc
S 1x512ms 0.67 B/cyc 6111 cyc
S 1x 2.62 B/cyc 6245 cyc
S 4x 3.95 B/cyc 4145 cyc
S 16x 4.32 B/cyc 15147 cyc
LLS 4x 1.50 B/cyc 10855 cyc
 */