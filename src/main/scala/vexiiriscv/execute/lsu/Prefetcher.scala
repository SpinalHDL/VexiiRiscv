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
class PrefetcherRptPlugin(sets : Int,
                          bootMemClear : Boolean,
                          readAt : Int = 0,
                          tagAt: Int = 1,
                          ctrlAt: Int = 2,
                          addAt: Int = 1,
                          prefetchAt: Int = 1,
                          tagWidth: Int = 15,
                          addressWidth: Int = 16,
                          strideWidth: Int = 12,
                          blockAheadMax: Int = 4,
                          scoreMax: Int = 31,
                          scorePass: Int = 1,
                          scoreFailShift: Int = 1,
                          scoreConflict: Int = 2,
                          scoreOffset: Int = 3,
                          scoreShift: Int = 0) extends PrefetcherPlugin  with InitService {

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
    val queued = order.queue(4, latency = 1).combStage()
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
    dp.haltDispatchWhen(RegNext(!order.ready) init(False))

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
      order << unfiltred

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
      storage.write.data.address  := PROBE.trap.mux(ENTRY.address, PROBE.address.resized)
      storage.write.data.stride   := (ENTRY.score < scoreOffset).mux[SInt](STRIDE, ENTRY.stride)
      storage.write.data.score    := score
      storage.write.data.advance  := unfiltred.fire.mux(unfiltred.to, advanceSubed).resized
      storage.write.data.missed   := PROBE.miss || ENTRY.missed && STRIDE_HIT

      unfiltred.valid   := isFiring && orderAsk && !PROBE.prefetchFailed && storage.write.data.missed
      unfiltred.address := PROBE.address
      unfiltred.unique  := PROBE.store
      unfiltred.from    := advanceSubed+1
      unfiltred.to      := advanceAllowed.min(blockAheadMax).resized
      unfiltred.stride  := STRIDE.msb.mux(STRIDE min -lsu.getBlockSize, STRIDE max lsu.getBlockSize)

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
L 1x416s 0.74 B/cyc 5513 cyc
L 1x512s 0.77 B/cyc 5272 cyc
L 1x512ms 0.78 B/cyc 5248 cyc
L 1x 2.55 B/cyc 6417 cyc
L 1x 2.60 B/cyc 6293 cyc
L 4x 4.86 B/cyc 3368 cyc
L 16x 5.52 B/cyc 2963 cyc
L 16x 6.82 B/cyc 9605 cyc
S 1x512s 0.64 B/cyc 6329 cyc
S 1x512ms 0.65 B/cyc 6285 cyc
S 1x 2.64 B/cyc 6202 cyc
S 4x 3.21 B/cyc 5092 cyc
S 16x 3.55 B/cyc 18433 cyc
LLS 4x 1.33 B/cyc 12246 cyc
 */