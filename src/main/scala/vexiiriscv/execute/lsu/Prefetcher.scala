package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.fetch.{Fetch, InitService, LsuService}
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
      order << unfiltred //.throwWhen(filter.hit)

      val add, sub = SCORE()
      add := 0
      sub := 0
      val score = ENTRY.score -| sub +| add //not great

      val advanceSubed = (ENTRY.advance.andMask(!PROBE.miss) -| U(NEW_BLOCK))
      val advanceAllowed = (ENTRY.score -| scoreOffset) >> scoreShift
      val orderAsk = False

      //TODO maybe only start to realocate entries when the new one progress forward ? not sure
      //TODO on failure the score penality may need to be propotionaly reduced.
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

https://zsmith.co/bandwidth.php

Write speed: 182.0MiB/s
 Read speed: 332.3MiB/s

L 1x 2.24 B/cyc 7301 cyc
L 4x 4.56 B/cyc 3591 cyc
L 16x 2.42 B/cyc 27013 cyc
LLS 4x 0.97 B/cyc 16841 cyc
L 1x 1.09 B/cyc 14977 cyc
L 4x 1.38 B/cyc 11833 cyc
L 16x 1.48 B/cyc 44087 cyc
LLS 4x 0.63 B/cyc 25852 cyc

none
0000000000000621
0000000000000279
0000000000000315
00000000000002c0
00000000000006d5
00000000000004d1

next line wo trap
00000000000004ae
000000000000024c
0000000000000300
00000000000002ac
0000000000000617
00000000000004a6

next line with trap
0000000000000341
000000000000024c
00000000000002f1
00000000000002ac
000000000000056c
00000000000004b0


//None
0000000000000624
0000000000000642
0000000000000333
00000000000002fd
0000000000000769
00000000000007be

//soft + hard
0000000000000359
0000000000000237
00000000000002f9
00000000000002a0
000000000000056f
00000000000004a0


0000000000000527

00000000000001e2
0000000000000221
00000000000002d8
000000000000029a
00000000000005b1
00000000000004a9


00000000000008a3

00000000000001d9
0000000000000347
00000000000001d9
0000000000000408
000000000000071c
00000000000006f2


.Lr1:
  # Total of 32 transfers x 8 bytes = 256 bytes.
  # 8 transfers, 8 bytes each
  ld	a3, (a0)
  ld	a4, 8(a0)
  ld	a5, 16(a0)
  ld	a6, 24(a0)
  ld	a7, 32(a0)
  ld	t2, 40(a0)
  ld	t3, 48(a0)
  ld	t4, 56(a0)
  addi	a0, a0, 64
  # 8 transfers, 8 bytes each
  ld	a3, (a0)
  ld	a4, 8(a0)
  ld	a5, 16(a0)
  ld	a6, 24(a0)
  ld	a7, 32(a0)
  ld	t2, 40(a0)
  ld	t3, 48(a0)
  ld	t4, 56(a0)
  addi	a0, a0, 64
  # 8 transfers, 8 bytes each
  ld	a3, (a0)
  ld	a4, 8(a0)
  ld	a5, 16(a0)
  ld	a6, 24(a0)
  ld	a7, 32(a0)
  ld	t2, 40(a0)
  ld	t3, 48(a0)
  ld	t4, 56(a0)
  addi	a0, a0, 64
  # 8 transfers, 8 bytes each
  ld	a3, (a0)
  ld	a4, 8(a0)
  ld	a5, 16(a0)
  ld	a6, 24(a0)
  ld	a7, 32(a0)
  ld	t2, 40(a0)
  ld	t3, 48(a0)
  ld	t4, 56(a0)
  addi	a0, a0, 64

  addi	a1, a1, -256
  bnez	a1, .Lr1

  addi	a2, a2, -1
  bnez	a2, .Lr0

  ret


Write speed: 166.7MiB/s
 Read speed: 113.3MiB/s

Write speed: 165.8MiB/s
 Read speed: 204.4MiB/s

none
litex> mem_speed 0x40000000 0x100000
Memspeed at 0x40000000 (Sequential, 1.0MiB)...
  Write speed: 1.7MiB/s
   Read speed: 1.6MiB/s


litex> mem_speed 0x40000000 0x100000
Memspeed at 0x40000000 (Sequential, 1.0MiB)...
  Write speed: 1.6MiB/s
   Read speed: 4.7MiB/s

none 1c =>
Startup finished in 11.664s (kernel) + 3min 35.746s (userspace) = 3min 47.410s
graphical.target reached after 3min 34.513s in userspace.

rpt hardware prefetcher 1c
Startup finished in 11.646s (kernel) + 3min 4.291s (userspace) = 3min 15.937s
graphical.target reached after 3min 2.782s in userspace.

rpt hardware prefetcher 2c
Startup finished in 11.084s (kernel) + 1min 50.167s (userspace) = 2min 1.251s
graphical.target reached after 1min 49.208s in userspace.
timed 5026 gametics in 8234 realtics (21.363857 fps)

rpt hardware prefetcher 4c
Startup finished in 8.245s (kernel) + 1min 17.500s (userspace) = 1min 25.746s
graphical.target reached after 1min 16.651s in userspace.
timed 5026 gametics in 8005 realtics (21.975016 fps)

[    0.000000] clocksource: riscv_clocksource: mask: 0xffffffffffffffff max_cycles: 0x171024e7e0, max_idle_ns: 440795205315 ns
[    0.000064] sched_clock: 64 bits at 100MHz, resolution 10ns, wraps every 4398046511100ns
[    0.001594] Console: colour dummy device 80x25
[    0.002135] printk: console [hvc0] enabled
[    0.002135] printk: console [hvc0] enabled
[    0.002978] printk: bootconsole [sbi0] disabled
[    0.002978] printk: bootconsole [sbi0] disabled
[    0.004014] Calibrating delay loop (skipped), value calculated using timer frequency.. 200.00 BogoMIPS (lpj=400000)
[    0.005248] pid_max: default: 32768 minimum: 301
[    0.006755] Mount-cache hash table entries: 512 (order: 0, 4096 bytes, linear)
[    0.007583] Mountpoint-cache hash table entries: 512 (order: 0, 4096 bytes, linear)
[    0.017582] rcu: Hierarchical SRCU implementation.
[    0.020062] smp: Bringing up secondary CPUs ...
[    0.020601] smp: Brought up 1 node, 1 CPU
[    0.022926] devtmpfs: initialized
[    0.027859] clocksource: jiffies: mask: 0xffffffff max_cycles: 0xffffffff, max_idle_ns: 7645041785100000 ns
[    0.028924] futex hash table entries: 256 (order: 2, 16384 bytes, linear)
[    0.031451] NET: Registered protocol family 16
[    0.071113] clocksource: Switched to clocksource riscv_clocksource
[    0.124171] NET: Registered protocol family 2
[    0.129262] tcp_listen_portaddr_hash hash table entries: 256 (order: 0, 4096 bytes, linear)
[    0.130273] TCP established hash table entries: 2048 (order: 2, 16384 bytes, linear)
[    0.131533] TCP bind hash table entries: 2048 (order: 3, 32768 bytes, linear)
[    0.132629] TCP: Hash tables configured (established 2048 bind 2048)
[    0.133867] UDP hash table entries: 256 (order: 1, 8192 bytes, linear)
[    0.134737] UDP-Lite hash table entries: 256 (order: 1, 8192 bytes, linear)
[    0.138491] Unpacking initramfs...
[    0.346114] Initramfs unpacking failed: invalid magic at start of compressed archive
[    0.395678] Freeing initrd memory: 8192K
[    0.398889] workingset: timestamp_bits=62 max_order=16 bucket_order=0
[    0.459122] Block layer SCSI generic (bsg) driver version 0.4 loaded (major 254)
[    0.459916] io scheduler mq-deadline registered
[    0.460437] io scheduler kyber registered
[    0.793762] NET: Registered protocol family 10
[    0.799108] Segment Routing with IPv6
[    0.800025] sit: IPv6, IPv4 and MPLS over IPv4 tunneling driver
[    0.804881] NET: Registered protocol family 17
[    0.809217] Freeing unused kernel memory: 176K
[    0.809733] Kernel memory protection not selected by kernel config.
[    0.810458] Run /init as init process
Starting syslogd: OK
Starting klogd: OK
Running sysctl: OK
Saving random seed: [    1.568969] random: dd: uninitialized urandom read (512 bytes read)
OK
Starting network: OK


Max Delay Paths
--------------------------------------------------------------------------------------
Slack (VIOLATED) :        -1.363ns  (required time - arrival time)
  Source:                 VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_0_iBus_noDecoder_toDown_d_rData_data_reg[54]/C
                            (rising edge-triggered cell FDRE clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Destination:            VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FetchL1Plugin_logic_banks_3_mem_reg/DIBDI[22]
                            (rising edge-triggered cell RAMB36E1 clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Path Group:             crg_s7mmcm0_clkout0
  Path Type:              Setup (Max at Slow Process Corner)
  Requirement:            10.000ns  (crg_s7mmcm0_clkout0 rise@10.000ns - crg_s7mmcm0_clkout0 rise@0.000ns)
  Data Path Delay:        10.920ns  (logic 0.518ns (4.744%)  route 10.402ns (95.256%))
  Logic Levels:           0
  Clock Path Skew:        -0.135ns (DCD - SCD + CPR)
    Destination Clock Delay (DCD):    6.138ns = ( 16.138 - 10.000 )
    Source Clock Delay      (SCD):    6.518ns
    Clock Pessimism Removal (CPR):    0.245ns
  Clock Uncertainty:      0.067ns  ((TSJ^2 + DJ^2)^1/2) / 2 + PE
    Total System Jitter     (TSJ):    0.071ns
    Discrete Jitter          (DJ):    0.114ns
    Phase Error              (PE):    0.000ns

    Location             Delay type                Incr(ns)  Path(ns)    Netlist Resource(s)
  -------------------------------------------------------------------    -------------------
                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                      0.000     0.000 r
    R4                                                0.000     0.000 r  clk100 (IN)
                         net (fo=0)                   0.000     0.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.475     1.475 r  clk100_IBUF_inst/O
                         net (fo=11, routed)          1.233     2.708    crg_s7mmcm0_clkin
    MMCME2_ADV_X1Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.088     2.796 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.808     4.605    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.096     4.701 r  BUFG/O
                         net (fo=51864, routed)       1.817     6.518    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/out
    SLICE_X80Y93         FDRE                                         r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_0_iBus_noDecoder_toDown_d_rData_data_reg[54]/C
  -------------------------------------------------------------------    -------------------
    SLICE_X80Y93         FDRE (Prop_fdre_C_Q)         0.518     7.036 r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_0_iBus_noDecoder_toDown_d_rData_data_reg[54]/Q
                         net (fo=16, routed)         10.402    17.438    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/vexiis_0_iBus_noDecoder_toDown_d_rData_data[54]
    RAMB36_X7Y28         RAMB36E1                                     r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FetchL1Plugin_logic_banks_3_mem_reg/DIBDI[22]
  -------------------------------------------------------------------    -------------------

                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                     10.000    10.000 r
    R4                                                0.000    10.000 r  clk100 (IN)
                         net (fo=0)                   0.000    10.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.405    11.405 r  clk100_IBUF_inst/O
                         net (fo=11, routed)          1.162    12.567    crg_s7mmcm0_clkin
    MMCME2_ADV_X1Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.083    12.650 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.723    14.373    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.091    14.464 r  BUFG/O
                         net (fo=51864, routed)       1.675    16.138    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/out
    RAMB36_X7Y28         RAMB36E1                                     r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FetchL1Plugin_logic_banks_3_mem_reg/CLKBWRCLK
                         clock pessimism              0.245    16.383
                         clock uncertainty           -0.067    16.316
    RAMB36_X7Y28         RAMB36E1 (Setup_ramb36e1_CLKBWRCLK_DIBDI[22])
                                                     -0.241    16.075    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FetchL1Plugin_logic_banks_3_mem_reg
  -------------------------------------------------------------------
                         required time                         16.075
                         arrival time                         -17.438
  -------------------------------------------------------------------
                         slack                                 -1.363

Slack (VIOLATED) :        -1.358ns  (required time - arrival time)
  Source:                 VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/vexiis_3_logic_core_toplevel_execute_ctrl3_up_LsuL1_PHYSICAL_ADDRESS_lane0_reg[20]/C
                            (rising edge-triggered cell FDRE clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Destination:            VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/LsuL1Plugin_logic_shared_mem_reg_1/ENARDEN
                            (rising edge-triggered cell RAMB18E1 clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Path Group:             crg_s7mmcm0_clkout0
  Path Type:              Setup (Max at Slow Process Corner)
  Requirement:            10.000ns  (crg_s7mmcm0_clkout0 rise@10.000ns - crg_s7mmcm0_clkout0 rise@0.000ns)
  Data Path Delay:        10.834ns  (logic 1.882ns (17.371%)  route 8.952ns (82.629%))
  Logic Levels:           11  (LUT3=1 LUT5=3 LUT6=7)
  Clock Path Skew:        -0.014ns (DCD - SCD + CPR)
    Destination Clock Delay (DCD):    6.031ns = ( 16.031 - 10.000 )
    Source Clock Delay      (SCD):    6.362ns
    Clock Pessimism Removal (CPR):    0.317ns
  Clock Uncertainty:      0.067ns  ((TSJ^2 + DJ^2)^1/2) / 2 + PE
    Total System Jitter     (TSJ):    0.071ns
    Discrete Jitter          (DJ):    0.114ns
    Phase Error              (PE):    0.000ns

    Location             Delay type                Incr(ns)  Path(ns)    Netlist Resource(s)
  -------------------------------------------------------------------    -------------------
                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                      0.000     0.000 r
    R4                                                0.000     0.000 r  clk100 (IN)
                         net (fo=0)                   0.000     0.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.475     1.475 r  clk100_IBUF_inst/O
                         net (fo=11, routed)          1.233     2.708    crg_s7mmcm0_clkin
    MMCME2_ADV_X1Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.088     2.796 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.808     4.605    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.096     4.701 r  BUFG/O
                         net (fo=51864, routed)       1.661     6.362    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/out
    SLICE_X88Y146        FDRE                                         r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/vexiis_3_logic_core_toplevel_execute_ctrl3_up_LsuL1_PHYSICAL_ADDRESS_lane0_reg[20]/C
  -------------------------------------------------------------------    -------------------
    SLICE_X88Y146        FDRE (Prop_fdre_C_Q)         0.518     6.880 f  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/vexiis_3_logic_core_toplevel_execute_ctrl3_up_LsuL1_PHYSICAL_ADDRESS_lane0_reg[20]/Q
                         net (fo=9, routed)           1.026     7.906    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/integer_RegFilePlugin_logic_regfile_fpga/LsuPlugin_logic_onCtrl_io_doItReg_i_6_0[13]
    SLICE_X88Y148        LUT5 (Prop_lut5_I1_O)        0.124     8.030 r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/integer_RegFilePlugin_logic_regfile_fpga/LsuPlugin_logic_onCtrl_io_doItReg_i_13/O
                         net (fo=3, routed)           1.013     9.043    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/integer_RegFilePlugin_logic_regfile_fpga/LsuPlugin_logic_onCtrl_io_doItReg_i_13_n_0
    SLICE_X88Y146        LUT6 (Prop_lut6_I5_O)        0.124     9.167 r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/integer_RegFilePlugin_logic_regfile_fpga/LsuPlugin_logic_onCtrl_io_doItReg_i_4/O
                         net (fo=4, routed)           0.600     9.767    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/integer_RegFilePlugin_logic_regfile_fpga/LsuPlugin_logic_onCtrl_io_doItReg_i_4_n_0
    SLICE_X89Y144        LUT6 (Prop_lut6_I2_O)        0.124     9.891 f  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/integer_RegFilePlugin_logic_regfile_fpga/LsuPlugin_logic_onCtrl_io_doItReg_i_2__0/O
                         net (fo=6, routed)           0.729    10.620    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FpuSqrtPlugin_logic_sqrt/LsuPlugin_logic_onCtrl_io_doItReg_reg
    SLICE_X81Y143        LUT3 (Prop_lut3_I0_O)        0.124    10.744 f  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FpuSqrtPlugin_logic_sqrt/LsuPlugin_logic_onCtrl_io_doItReg_i_1__1/O
                         net (fo=2, routed)           0.302    11.045    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FpuSqrtPlugin_logic_sqrt/p_1120_in
    SLICE_X78Y143        LUT6 (Prop_lut6_I3_O)        0.124    11.169 f  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FpuSqrtPlugin_logic_sqrt/vexiis_3_logic_core_toplevel_execute_ctrl1_up_float_RS2_lane0[63]_i_9/O
                         net (fo=1, routed)           0.444    11.613    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FpuSqrtPlugin_logic_sqrt/vexiis_3_logic_core_toplevel_execute_ctrl1_up_float_RS2_lane0[63]_i_9_n_0
    SLICE_X78Y143        LUT6 (Prop_lut6_I1_O)        0.124    11.737 f  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/FpuSqrtPlugin_logic_sqrt/vexiis_3_logic_core_toplevel_execute_ctrl1_up_float_RS2_lane0[63]_i_3/O
                         net (fo=1, routed)           0.300    12.037    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/_zz_vexiis_3_logic_core_toplevel_execute_ctrl1_down_FpuUnpack_RS3_badBoxing_HIT_lane0_reg[0]_rep__1
    SLICE_X78Y142        LUT6 (Prop_lut6_I5_O)        0.124    12.161 r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/vexiis_3_logic_core_toplevel_execute_ctrl1_up_float_RS2_lane0[63]_i_1/O
                         net (fo=4542, routed)        1.602    13.763    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/_zz_LsuPlugin_logic_onCtrl_rva_nc_age
    SLICE_X100Y140       LUT5 (Prop_lut5_I2_O)        0.124    13.887 f  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/LsuL1Plugin_logic_ways_1_mem_reg_1_i_136/O
                         net (fo=1, routed)           0.423    14.310    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/LsuL1Plugin_logic_ways_1_mem_reg_1_i_136_n_0
    SLICE_X100Y139       LUT5 (Prop_lut5_I0_O)        0.124    14.434 f  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/LsuL1Plugin_logic_ways_1_mem_reg_1_i_133/O
                         net (fo=1, routed)           0.553    14.987    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/LsuL1Plugin_logic_ways_1_mem_reg_1_i_133_n_0
    SLICE_X101Y136       LUT6 (Prop_lut6_I3_O)        0.124    15.111 r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/LsuL1Plugin_logic_ways_1_mem_reg_1_i_97/O
                         net (fo=12, routed)          0.652    15.763    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/LsuL1Plugin_logic_ways_1_mem_reg_1_i_97_n_0
    SLICE_X98Y137        LUT6 (Prop_lut6_I4_O)        0.124    15.887 r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div/LsuL1Plugin_logic_shared_mem_reg_1_i_1__1/O
                         net (fo=18, routed)          1.309    17.196    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/early0_DivPlugin_logic_processing_div_n_438
    RAMB18_X5Y48         RAMB18E1                                     r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/LsuL1Plugin_logic_shared_mem_reg_1/ENARDEN
  -------------------------------------------------------------------    -------------------

                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                     10.000    10.000 r
    R4                                                0.000    10.000 r  clk100 (IN)
                         net (fo=0)                   0.000    10.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.405    11.405 r  clk100_IBUF_inst/O
                         net (fo=11, routed)          1.162    12.567    crg_s7mmcm0_clkin
    MMCME2_ADV_X1Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.083    12.650 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.723    14.373    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.091    14.464 r  BUFG/O
                         net (fo=51864, routed)       1.568    16.031    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/out
    RAMB18_X5Y48         RAMB18E1                                     r  VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/LsuL1Plugin_logic_shared_mem_reg_1/CLKARDCLK
                         clock pessimism              0.317    16.348
                         clock uncertainty           -0.067    16.281
    RAMB18_X5Y48         RAMB18E1 (Setup_ramb18e1_CLKARDCLK_ENARDEN)
                                                     -0.443    15.838    VexiiRiscvLitex_11904983958a42a0662d5abbc8b20dcf/vexiis_3_logic_core/LsuL1Plugin_logic_shared_mem_reg_1
  -------------------------------------------------------------------
                         required time                         15.838
                         arrival time                         -17.196
  -------------------------------------------------------------------
                         slack                                 -1.358











Max Delay Paths
--------------------------------------------------------------------------------------
Slack (VIOLATED) :        -1.492ns  (required time - arrival time)
  Source:                 VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/vexiis_2_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0_reg[30]/C
                            (rising edge-triggered cell FDRE clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Destination:            VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/LsuL1Plugin_logic_banks_1_mem/ram_block_reg/ENBWREN
                            (rising edge-triggered cell RAMB36E1 clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Path Group:             crg_s7mmcm0_clkout0
  Path Type:              Setup (Max at Slow Process Corner)
  Requirement:            10.000ns  (crg_s7mmcm0_clkout0 rise@10.000ns - crg_s7mmcm0_clkout0 rise@0.000ns)
  Data Path Delay:        10.693ns  (logic 2.192ns (20.499%)  route 8.501ns (79.501%))
  Logic Levels:           14  (LUT3=5 LUT4=3 LUT5=1 LUT6=5)
  Clock Path Skew:        -0.289ns (DCD - SCD + CPR)
    Destination Clock Delay (DCD):    6.208ns = ( 16.208 - 10.000 )
    Source Clock Delay      (SCD):    6.750ns
    Clock Pessimism Removal (CPR):    0.253ns
  Clock Uncertainty:      0.067ns  ((TSJ^2 + DJ^2)^1/2) / 2 + PE
    Total System Jitter     (TSJ):    0.071ns
    Discrete Jitter          (DJ):    0.114ns
    Phase Error              (PE):    0.000ns

    Location             Delay type                Incr(ns)  Path(ns)    Netlist Resource(s)
  -------------------------------------------------------------------    -------------------
                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                      0.000     0.000 r
    R4                                                0.000     0.000 r  clk100 (IN)
                         net (fo=0)                   0.000     0.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.475     1.475 r  clk100_IBUF_inst/O
                         net (fo=11, routed)          1.233     2.708    crg_s7mmcm0_clkin
    MMCME2_ADV_X1Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.088     2.796 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.808     4.605    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.096     4.701 r  BUFG/O
                         net (fo=52363, routed)       2.049     6.750    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/out
    SLICE_X127Y38        FDRE                                         r  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/vexiis_2_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0_reg[30]/C
  -------------------------------------------------------------------    -------------------
    SLICE_X127Y38        FDRE (Prop_fdre_C_Q)         0.456     7.206 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/vexiis_2_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0_reg[30]/Q
                         net (fo=7, routed)           0.715     7.921    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/vexiis_2_logic_core_toplevel_execute_ctrl2_up_lane0_integer_WriteBackPlugin_logic_DATA_lane0[63]_i_9_0[30]
    SLICE_X128Y37        LUT4 (Prop_lut4_I2_O)        0.124     8.045 r  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/vexiis_2_logic_core_toplevel_execute_ctrl2_up_FpuUnpack_RS1_RS_lane0_mode[0]_i_19/O
                         net (fo=1, routed)           0.575     8.621    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/vexiis_2_logic_core_toplevel_execute_ctrl2_up_FpuUnpack_RS1_RS_lane0_mode[0]_i_19_n_0
    SLICE_X129Y37        LUT4 (Prop_lut4_I0_O)        0.124     8.745 r  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/vexiis_2_logic_core_toplevel_execute_ctrl2_up_FpuUnpack_RS1_RS_lane0_mode[0]_i_11/O
                         net (fo=2, routed)           0.489     9.233    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/vexiis_2_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0_reg[26]
    SLICE_X129Y37        LUT4 (Prop_lut4_I2_O)        0.124     9.357 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/vexiis_2_logic_core_toplevel_execute_ctrl2_up_FpuUnpack_RS1_RS_lane0_mode[0]_i_4/O
                         net (fo=5, routed)           0.602     9.959    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/_zz_vexiis_2_logic_core_toplevel_execute_ctrl1_down_FpuUnpack_RS3_badBoxing_HIT_lane0_reg[0]_0
    SLICE_X128Y36        LUT5 (Prop_lut5_I2_O)        0.124    10.083 r  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/_zz_5[0]_i_5/O
                         net (fo=3, routed)           0.570    10.653    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/FpuUnpack_RS1_normalizer_freezeIt
    SLICE_X127Y34        LUT3 (Prop_lut3_I1_O)        0.124    10.777 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/busy_i_2/O
                         net (fo=3, routed)           0.303    11.080    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/FpuUnpackerPlugin_logic_unpackDone
    SLICE_X127Y36        LUT3 (Prop_lut3_I0_O)        0.124    11.204 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/early0_DivPlugin_logic_processing_a_delay_1[63]_i_1__0/O
                         net (fo=270, routed)         0.497    11.701    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/when_FpuDivPlugin_l62
    SLICE_X127Y38        LUT3 (Prop_lut3_I0_O)        0.124    11.825 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/early0_DivPlugin_logic_processing_relaxer_hadRequest_i_2__0/O
                         net (fo=4, routed)           0.553    12.378    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/early0_DivPlugin_logic_processing_request
    SLICE_X125Y39        LUT6 (Prop_lut6_I2_O)        0.124    12.502 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/early0_DivPlugin_logic_processing_div/CsrAccessPlugin_logic_fsm_inject_sampled_i_3/O
                         net (fo=2, routed)           0.517    13.019    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/LsuPlugin_logic_onCtrl_io_tooEarly_reg
    SLICE_X126Y39        LUT6 (Prop_lut6_I5_O)        0.124    13.143 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/CsrAccessPlugin_logic_fsm_inject_sampled_i_1__2/O
                         net (fo=110, routed)         0.434    13.577    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/execute_freeze_valid
    SLICE_X126Y39        LUT3 (Prop_lut3_I0_O)        0.124    13.701 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/LsuL1Plugin_logic_ways_3_mem_reg_1_i_128__0/O
                         net (fo=2, routed)           0.642    14.343    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/LsuL1Plugin_logic_ways_3_mem_reg_1_i_128__0_n_0
    SLICE_X115Y40        LUT6 (Prop_lut6_I2_O)        0.124    14.467 f  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/LsuL1Plugin_logic_ways_3_mem_reg_1_i_125__0/O
                         net (fo=1, routed)           0.263    14.729    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/LsuL1Plugin_logic_ways_3_mem_reg_1_i_125__0_n_0
    SLICE_X115Y40        LUT6 (Prop_lut6_I4_O)        0.124    14.853 r  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/LsuL1Plugin_logic_ways_3_mem_reg_1_i_92__0/O
                         net (fo=13, routed)          1.283    16.136    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/LsuL1Plugin_logic_ways_3_mem_reg_1_i_92__0_n_0
    SLICE_X98Y54         LUT3 (Prop_lut3_I2_O)        0.124    16.260 r  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/ram_block_reg_i_14__2/O
                         net (fo=1, routed)           0.295    16.555    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/ram_block_reg_i_14__2_n_0
    SLICE_X96Y54         LUT6 (Prop_lut6_I5_O)        0.124    16.679 r  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/FpuSqrtPlugin_logic_sqrt/ram_block_reg_i_2__2/O
                         net (fo=1, routed)           0.764    17.443    VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/LsuL1Plugin_logic_banks_1_mem/LsuL1Plugin_logic_banks_1_write_valid
    RAMB36_X5Y12         RAMB36E1                                     r  VexiiRiscvLitex_d3790b4b6be84c304ac0232f29cd22b0/vexiis_2_logic_core/LsuL1Plugin_logic_banks_1_mem/ram_block_reg/ENBWREN



 Slack (VIOLATED) :        -1.130ns  (required time - arrival time)
  Source:                 VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/vexiis_1_logic_core_toplevel_execute_ctrl3_up_Decode_UOP_lane0_reg[31]/C
                            (rising edge-triggered cell FDRE clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Destination:            VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[40]/D
                            (rising edge-triggered cell FDRE clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Path Group:             crg_s7mmcm0_clkout0
  Path Type:              Setup (Max at Slow Process Corner)
  Requirement:            10.000ns  (crg_s7mmcm0_clkout0 rise@10.000ns - crg_s7mmcm0_clkout0 rise@0.000ns)
  Data Path Delay:        10.847ns  (logic 3.809ns (35.114%)  route 7.038ns (64.886%))
  Logic Levels:           21  (CARRY4=16 LUT2=1 LUT4=2 LUT6=2)
  Clock Path Skew:        -0.292ns (DCD - SCD + CPR)
    Destination Clock Delay (DCD):    6.174ns = ( 16.174 - 10.000 )
    Source Clock Delay      (SCD):    6.719ns
    Clock Pessimism Removal (CPR):    0.253ns
  Clock Uncertainty:      0.067ns  ((TSJ^2 + DJ^2)^1/2) / 2 + PE
    Total System Jitter     (TSJ):    0.071ns
    Discrete Jitter          (DJ):    0.114ns
    Phase Error              (PE):    0.000ns

    Location             Delay type                Incr(ns)  Path(ns)    Netlist Resource(s)
  -------------------------------------------------------------------    -------------------
                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                      0.000     0.000 r
    R4                                                0.000     0.000 r  clk100 (IN)
                         net (fo=0)                   0.000     0.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.475     1.475 r  clk100_IBUF_inst/O
                         net (fo=11, routed)          1.233     2.708    crg_s7mmcm0_clkin
    MMCME2_ADV_X1Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.088     2.796 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.808     4.605    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.096     4.701 r  BUFG/O
                         net (fo=53450, routed)       2.018     6.719    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/out
    SLICE_X44Y18         FDRE                                         r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/vexiis_1_logic_core_toplevel_execute_ctrl3_up_Decode_UOP_lane0_reg[31]/C
  -------------------------------------------------------------------    -------------------
    SLICE_X44Y18         FDRE (Prop_fdre_C_Q)         0.518     7.237 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/vexiis_1_logic_core_toplevel_execute_ctrl3_up_Decode_UOP_lane0_reg[31]/Q
                         net (fo=129, routed)         0.501     7.738    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_alu_compare
    SLICE_X45Y19         LUT2 (Prop_lut2_I0_O)        0.124     7.862 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer[3]_i_4__2/O
                         net (fo=1, routed)           0.472     8.334    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/_zz_LsuPlugin_logic_onCtrl_rva_alu_addSub_3[0]
    SLICE_X45Y22         CARRY4 (Prop_carry4_CYINIT_CO[3])
                                                      0.580     8.914 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[3]_i_3__2/CO[3]
                         net (fo=1, routed)           0.000     8.914    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[3]_i_3__2_n_0
    SLICE_X45Y23         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.028 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[7]_i_3__2/CO[3]
                         net (fo=1, routed)           0.000     9.028    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[7]_i_3__2_n_0
    SLICE_X45Y24         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.142 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[11]_i_3__2/CO[3]
                         net (fo=1, routed)           0.009     9.151    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[11]_i_3__2_n_0
    SLICE_X45Y25         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.265 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[15]_i_3__2/CO[3]
                         net (fo=1, routed)           0.000     9.265    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[15]_i_3__2_n_0
    SLICE_X45Y26         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.379 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[19]_i_3__2/CO[3]
                         net (fo=1, routed)           0.000     9.379    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[19]_i_3__2_n_0
    SLICE_X45Y27         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.493 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[23]_i_3__2/CO[3]
                         net (fo=1, routed)           0.000     9.493    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[23]_i_3__2_n_0
    SLICE_X45Y28         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.607 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[27]_i_3__2/CO[3]
                         net (fo=1, routed)           0.000     9.607    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[27]_i_3__2_n_0
    SLICE_X45Y29         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.721 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[31]_i_4__1/CO[3]
                         net (fo=1, routed)           0.000     9.721    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[31]_i_4__1_n_0
    SLICE_X45Y30         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.835 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[35]_i_4__1/CO[3]
                         net (fo=1, routed)           0.000     9.835    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[35]_i_4__1_n_0
    SLICE_X45Y31         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114     9.949 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[39]_i_4__1/CO[3]
                         net (fo=1, routed)           0.000     9.949    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[39]_i_4__1_n_0
    SLICE_X45Y32         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114    10.063 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[43]_i_4__1/CO[3]
                         net (fo=1, routed)           0.000    10.063    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[43]_i_4__1_n_0
    SLICE_X45Y33         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114    10.177 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[47]_i_4__1/CO[3]
                         net (fo=1, routed)           0.000    10.177    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[47]_i_4__1_n_0
    SLICE_X45Y34         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114    10.291 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[51]_i_4__1/CO[3]
                         net (fo=1, routed)           0.000    10.291    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[51]_i_4__1_n_0
    SLICE_X45Y35         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114    10.405 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[55]_i_4__1/CO[3]
                         net (fo=1, routed)           0.000    10.405    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[55]_i_4__1_n_0
    SLICE_X45Y36         CARRY4 (Prop_carry4_CI_CO[3])
                                                      0.114    10.519 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[59]_i_4__1/CO[3]
                         net (fo=1, routed)           0.000    10.519    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[59]_i_4__1_n_0
    SLICE_X45Y37         CARRY4 (Prop_carry4_CI_O[3])
                                                      0.313    10.832 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[63]_i_4__1/O[3]
                         net (fo=2, routed)           0.700    11.531    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/_zz_LsuPlugin_logic_onCtrl_rva_alu_addSub[63]
    SLICE_X51Y44         LUT4 (Prop_lut4_I3_O)        0.306    11.837 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer[31]_i_5__2/O
                         net (fo=1, routed)           0.403    12.240    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer[31]_i_5__2_n_0
    SLICE_X51Y44         LUT6 (Prop_lut6_I1_O)        0.124    12.364 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer[31]_i_3__1/O
                         net (fo=64, routed)          2.704    15.068    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer[31]_i_3__1_n_0
    SLICE_X114Y31        LUT6 (Prop_lut6_I4_O)        0.124    15.192 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer[40]_i_2__2/O
                         net (fo=1, routed)           2.251    17.442    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer[40]_i_2__2_n_0
    SLICE_X64Y50         LUT4 (Prop_lut4_I0_O)        0.124    17.566 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer[40]_i_1__1/O
                         net (fo=1, routed)           0.000    17.566    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_alu_result[40]
    SLICE_X64Y50         FDRE                                         r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[40]/D
  -------------------------------------------------------------------    -------------------

                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                     10.000    10.000 r
    R4                                                0.000    10.000 r  clk100 (IN)
                         net (fo=0)                   0.000    10.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.405    11.405 r  clk100_IBUF_inst/O
                         net (fo=11, routed)          1.162    12.567    crg_s7mmcm0_clkin
    MMCME2_ADV_X1Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.083    12.650 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.723    14.373    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.091    14.464 r  BUFG/O
                         net (fo=53450, routed)       1.710    16.174    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/out
    SLICE_X64Y50         FDRE                                         r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[40]/C
                         clock pessimism              0.253    16.427
                         clock uncertainty           -0.067    16.360
    SLICE_X64Y50         FDRE (Setup_fdre_C_D)        0.077    16.437    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/vexiis_1_logic_core/LsuPlugin_logic_onCtrl_rva_aluBuffer_reg[40]
  -------------------------------------------------------------------
                         required time                         16.437
                         arrival time                         -17.566
  -------------------------------------------------------------------
                         slack                                 -1.130


Max Delay Paths
--------------------------------------------------------------------------------------
Slack (VIOLATED) :        -0.939ns  (required time - arrival time)
  Source:                 VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/gs_ctxDownD_ram_reg/CLKARDCLK
                            (rising edge-triggered cell RAMB18E1 clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Destination:            VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/fromUpA_buffer_ram_reg/ENARDEN
                            (rising edge-triggered cell RAMB36E1 clocked by crg_s7mmcm0_clkout0  {rise@0.000ns fall@5.000ns period=10.000ns})
  Path Group:             crg_s7mmcm0_clkout0
  Path Type:              Setup (Max at Slow Process Corner)
  Requirement:            10.000ns  (crg_s7mmcm0_clkout0 rise@10.000ns - crg_s7mmcm0_clkout0 rise@0.000ns)
  Data Path Delay:        10.300ns  (logic 3.446ns (33.457%)  route 6.854ns (66.543%))
  Logic Levels:           8  (LUT2=2 LUT3=1 LUT6=5)
  Clock Path Skew:        -0.128ns (DCD - SCD + CPR)
    Destination Clock Delay (DCD):    9.648ns = ( 19.648 - 10.000 )
    Source Clock Delay      (SCD):    10.783ns
    Clock Pessimism Removal (CPR):    1.007ns
  Clock Uncertainty:      0.067ns  ((TSJ^2 + DJ^2)^1/2) / 2 + PE
    Total System Jitter     (TSJ):    0.071ns
    Discrete Jitter          (DJ):    0.114ns
    Phase Error              (PE):    0.000ns

    Location             Delay type                Incr(ns)  Path(ns)    Netlist Resource(s)
  -------------------------------------------------------------------    -------------------
                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                      0.000     0.000 r
    R4                                                0.000     0.000 r  clk100 (IN)
                         net (fo=0)                   0.000     0.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.475     1.475 r  clk100_IBUF_inst/O
                         net (fo=3, routed)           1.227     2.702    clk100_IBUF
    SLICE_X158Y131       LUT1 (Prop_lut1_I0_O)        0.124     2.826 r  crg_s7mmcm0_clkin_inst/O
                         net (fo=9, routed)           4.239     7.065    crg_s7mmcm0_clkin
    MMCME2_ADV_X0Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.088     7.153 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.843     8.996    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.096     9.092 r  BUFG/O
                         net (fo=54042, routed)       1.690    10.783    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/logic_ram_reg
    RAMB18_X3Y64         RAMB18E1                                     r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/gs_ctxDownD_ram_reg/CLKARDCLK
  -------------------------------------------------------------------    -------------------
    RAMB18_X3Y64         RAMB18E1 (Prop_ramb18e1_CLKARDCLK_DOBDO[13])
                                                      2.454    13.237 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/gs_ctxDownD_ram_reg/DOBDO[13]
                         net (fo=5, routed)           1.478    14.715    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/writeBackend_putMerges_fifo/DOBDO[13]
    SLICE_X59Y168        LUT6 (Prop_lut6_I4_O)        0.124    14.839 f  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/writeBackend_putMerges_fifo/maskLocked_1_i_11/O
                         net (fo=1, routed)           0.597    15.436    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/writeBackend_putMerges_fifo/maskLocked_1_i_11_n_0
    SLICE_X56Y168        LUT6 (Prop_lut6_I4_O)        0.124    15.560 f  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/writeBackend_putMerges_fifo/maskLocked_1_i_7/O
                         net (fo=2, routed)           0.318    15.878    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/maskLocked_0_reg_0
    SLICE_X56Y170        LUT6 (Prop_lut6_I1_O)        0.124    16.002 f  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/maskLocked_1_i_5/O
                         net (fo=6, routed)           0.482    16.485    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/fromDownD_process_toUpD_valid
    SLICE_X57Y173        LUT6 (Prop_lut6_I4_O)        0.124    16.609 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/maskLocked_3_i_1__3/O
                         net (fo=5, routed)           0.616    17.224    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/maskLocked_3_i_1__3_n_0
    SLICE_X55Y176        LUT3 (Prop_lut3_I2_O)        0.124    17.348 f  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/fromUpA_buffer_ram_reg_i_13/O
                         net (fo=5, routed)           0.309    17.657    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/splited_wc_l2_cache_up_bus_d_rValidN_reg
    SLICE_X55Y176        LUT6 (Prop_lut6_I2_O)        0.124    17.781 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/fromUpA_buffer_ram_reg_i_3/O
                         net (fo=64, routed)          0.766    18.547    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/writeBackend_stages_2_valid_reg
    SLICE_X54Y172        LUT2 (Prop_lut2_I0_O)        0.124    18.671 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/writeBackend_stages_1_CMD_address[31]_i_1/O
                         net (fo=63, routed)          0.650    19.321    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/writeBackend_stages_1_valid_reg
    SLICE_X55Y171        LUT2 (Prop_lut2_I0_O)        0.124    19.445 r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/toUpD_arbiter/fromUpA_buffer_ram_reg_i_1/O
                         net (fo=2, routed)           1.638    21.083    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/writeBackend_stages_0_isFireing
    RAMB36_X5Y33         RAMB36E1                                     r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/fromUpA_buffer_ram_reg/ENARDEN
  -------------------------------------------------------------------    -------------------

                         (clock crg_s7mmcm0_clkout0 rise edge)
                                                     10.000    10.000 r
    R4                                                0.000    10.000 r  clk100 (IN)
                         net (fo=0)                   0.000    10.000    clk100
    R4                   IBUF (Prop_ibuf_I_O)         1.405    11.405 r  clk100_IBUF_inst/O
                         net (fo=3, routed)           1.044    12.448    clk100_IBUF
    SLICE_X158Y131       LUT1 (Prop_lut1_I0_O)        0.100    12.548 r  crg_s7mmcm0_clkin_inst/O
                         net (fo=9, routed)           3.610    16.158    crg_s7mmcm0_clkin
    MMCME2_ADV_X0Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                      0.083    16.241 r  MMCME2_ADV/CLKOUT0
                         net (fo=1, routed)           1.760    18.002    crg_s7mmcm0_clkout0
    BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.091    18.093 r  BUFG/O
                         net (fo=54042, routed)       1.555    19.648    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/logic_ram_reg
    RAMB36_X5Y33         RAMB36E1                                     r  VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/fromUpA_buffer_ram_reg/CLKARDCLK
                         clock pessimism              1.007    20.654
                         clock uncertainty           -0.067    20.587
    RAMB36_X5Y33         RAMB36E1 (Setup_ramb36e1_CLKARDCLK_ENARDEN)
                                                     -0.443    20.144    VexiiRiscvLitex_a3b11cf0ae64f7eed4a33651ce7e09a8/splited_wc_l2_cache_logic_cache/fromUpA_buffer_ram_reg
  -------------------------------------------------------------------
                         required time                         20.144
                         arrival time                         -21.083
  -------------------------------------------------------------------
                         slack                                 -0.939

-------------------------------------------------------------------    -------------------
  SLICE_X50Y41         FDRE (Prop_fdre_C_Q)         0.456    11.215 r  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_c_pip_ctrl_2_up_SHARED_dirty_reg[3]/Q
                       net (fo=2, routed)           0.513    11.728    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_bus_toTilelink_coherent_onC_rspFifo/logic_ram/logic_ptr_push[1]_i_4_1[3]
  SLICE_X50Y41         LUT4 (Prop_lut4_I2_O)        0.124    11.852 r  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_bus_toTilelink_coherent_onC_rspFifo/logic_ram/logic_ptr_push[1]_i_5/O
                       net (fo=1, routed)           0.291    12.143    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_bus_toTilelink_coherent_onC_rspFifo/logic_ram/logic_ptr_push[1]_i_5_n_0
  SLICE_X51Y43         LUT5 (Prop_lut5_I4_O)        0.124    12.267 f  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_bus_toTilelink_coherent_onC_rspFifo/logic_ram/logic_ptr_push[1]_i_4/O
                       net (fo=2, routed)           0.447    12.714    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_bus_toTilelink_coherent_onC_rspFifo/logic_ram/LsuL1Plugin_logic_c_pip_ctrl_2_up_WAYS_HITS_reg[0]
  SLICE_X51Y46         LUT3 (Prop_lut3_I0_O)        0.124    12.838 f  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_bus_toTilelink_coherent_onC_rspFifo/logic_ram/logic_ptr_push[1]_i_3/O
                       net (fo=22, routed)          0.691    13.529    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_bus_toTilelink_coherent_onC_rspFifo/logic_ram/LsuL1Plugin_logic_c_pip_ctrl_2_up_ALLOW_PROBE_DATA_reg
  SLICE_X50Y53         LUT6 (Prop_lut6_I5_O)        0.124    13.653 r  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/LsuL1Plugin_logic_bus_toTilelink_coherent_onC_rspFifo/logic_ram/ram_block_reg_0_1_36_41_i_4/O
                       net (fo=3, routed)           0.653    14.307    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuL1Plugin_logic_writeback_slots_2_busy_reg
  SLICE_X54Y53         LUT6 (Prop_lut6_I0_O)        0.124    14.431 f  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuL1Plugin_logic_ways_3_mem_reg_1_i_38/O
                       net (fo=137, routed)         0.604    15.035    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuL1Plugin_logic_c_pip_ctrl_2_up_WAYS_HIT_reg_0
  SLICE_X59Y53         LUT6 (Prop_lut6_I0_O)        0.124    15.159 f  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuL1Plugin_logic_shared_mem_reg_1_i_18/O
                       net (fo=5, routed)           0.190    15.348    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuL1Plugin_logic_c_pip_ctrl_2_up_WAYS_HIT_reg
  SLICE_X59Y53         LUT2 (Prop_lut2_I1_O)        0.124    15.472 f  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuPlugin_logic_onAddress0_access_waiter_refill[3]_i_3/O
                       net (fo=10, routed)          0.303    15.776    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuPlugin_logic_onCtrl_traps_l1Failed0
  SLICE_X59Y53         LUT3 (Prop_lut3_I2_O)        0.124    15.900 f  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuPlugin_logic_storeBuffer_ops_mem_reg_0_i_96/O
                       net (fo=3, routed)           0.373    16.272    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/when_LsuPlugin_l6300
  SLICE_X58Y53         LUT6 (Prop_lut6_I5_O)        0.124    16.396 r  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/PrefetchRptPlugin_logic_pip_node_1_PROBE_trap_i_2/O
                       net (fo=4, routed)           0.714    17.110    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/LsuPlugin_logic_storeBuffer_slots_2_valid_reg
  SLICE_X47Y47         LUT6 (Prop_lut6_I5_O)        0.124    17.234 f  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram_1/GSharePlugin_logic_mem_counter_reg_i_31/O
                       net (fo=70, routed)          1.030    18.264    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/early0_DivPlugin_logic_processing_div/FpuUnpackerPlugin_logic_onCvt_asked_reg_1
  SLICE_X35Y51         LUT6 (Prop_lut6_I1_O)        0.124    18.388 r  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/early0_DivPlugin_logic_processing_div/ram_block_reg_0_31_0_5_i_13/O
                       net (fo=1, routed)           0.491    18.879    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/early0_DivPlugin_logic_processing_div/lane0_integer_WriteBackPlugin_logic_write_port_valid
  SLICE_X34Y51         LUT2 (Prop_lut2_I1_O)        0.124    19.003 r  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/early0_DivPlugin_logic_processing_div/ram_block_reg_0_31_0_5_i_1__26/O
                       net (fo=176, routed)         1.142    20.145    VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/ram_block_reg_0_31_48_53/WE
  SLICE_X22Y56         RAMD32                                       r  VexiiRiscvLitex_ae5efec20a1108c1704e91b039083d92/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/ram_block_reg_0_31_48_53/RAMA/WE
-------------------------------------------------------------------    -------------------



Location             Delay type                Incr(ns)  Path(ns)    Netlist Resource(s)
-------------------------------------------------------------------    -------------------
                     (clock crg_s7mmcm0_clkout0 rise edge)
                                                  0.000     0.000 r
R4                                                0.000     0.000 r  clk100 (IN)
                     net (fo=0)                   0.000     0.000    clk100
R4                   IBUF (Prop_ibuf_I_O)         1.475     1.475 r  clk100_IBUF_inst/O
                     net (fo=3, routed)           0.995     2.470    clk100_IBUF
SLICE_X160Y123       LUT1 (Prop_lut1_I0_O)        0.124     2.594 r  crg_s7mmcm0_clkin_inst/O
                     net (fo=9, routed)           4.111     6.706    crg_s7mmcm0_clkin
MMCME2_ADV_X0Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                  0.088     6.794 r  MMCME2_ADV/CLKOUT0
                     net (fo=1, routed)           1.843     8.637    crg_s7mmcm0_clkout0
BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.096     8.733 r  BUFG/O
                     net (fo=55330, routed)       1.812    10.545    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/out
SLICE_X77Y71         FDRE                                         r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/vexiis_0_logic_core_toplevel_execute_ctrl3_up_LsuL1Plugin_logic_WAYS_HITS_lane0_reg[3]/C
-------------------------------------------------------------------    -------------------
SLICE_X77Y71         FDRE (Prop_fdre_C_Q)         0.456    11.001 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/vexiis_0_logic_core_toplevel_execute_ctrl3_up_LsuL1Plugin_logic_WAYS_HITS_lane0_reg[3]/Q
                     net (fo=86, routed)          0.958    11.959    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[63]_i_3_0[3]
SLICE_X73Y70         LUT4 (Prop_lut4_I3_O)        0.124    12.083 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[63]_i_4/O
                     net (fo=1, routed)           0.524    12.607    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[63]_i_4_n_0
SLICE_X68Y75         LUT5 (Prop_lut5_I4_O)        0.124    12.731 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[63]_i_3/O
                     net (fo=1, routed)           0.593    13.325    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/vexiis_0_logic_core_toplevel_execute_ctrl3_down_LsuL1Plugin_logic_MUXED_DATA_lane0[63]
SLICE_X69Y75         LUT4 (Prop_lut4_I0_O)        0.124    13.449 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[63]_i_2/O
                     net (fo=1, routed)           0.407    13.856    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[63]_i_2_n_0
SLICE_X69Y75         LUT6 (Prop_lut6_I5_O)        0.124    13.980 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[63]_i_1/O
                     net (fo=5, routed)           1.105    15.084    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/D[49]
SLICE_X59Y71         LUT6 (Prop_lut6_I2_O)        0.124    15.208 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[15]_i_1/O
                     net (fo=4, routed)           0.972    16.181    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/D[9]
SLICE_X45Y69         LUT6 (Prop_lut6_I1_O)        0.124    16.305 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/LsuPlugin_logic_onCtrl_rva_srcBuffer[7]_i_1/O
                     net (fo=3, routed)           0.796    17.101    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/D[3]
SLICE_X35Y66         LUT6 (Prop_lut6_I1_O)        0.124    17.225 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/vexiis_0_logic_core_toplevel_execute_ctrl1_up_early0_SrcPlugin_SRC1_lane0[7]_i_4/O
                     net (fo=4, routed)           0.615    17.840    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/vexiis_0_logic_core_toplevel_execute_ctrl1_up_early0_SrcPlugin_SRC1_lane0[7]_i_4_n_0
SLICE_X29Y63         LUT3 (Prop_lut3_I2_O)        0.124    17.964 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/vexiis_0_logic_core_toplevel_execute_ctrl1_up_integer_RS2_lane0[31]_i_4/O
                     net (fo=16, routed)          0.644    18.608    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/vexiis_0_logic_core_toplevel_execute_ctrl1_up_integer_RS2_lane0[31]_i_4_n_0
SLICE_X25Y65         LUT5 (Prop_lut5_I2_O)        0.124    18.732 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/vexiis_0_logic_core_toplevel_execute_ctrl1_up_integer_RS2_lane0[31]_i_3/O
                     net (fo=4, routed)           0.789    19.521    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/lane0_integer_WriteBackPlugin_logic_stages_2_muxed[31]
SLICE_X21Y62         LUT3 (Prop_lut3_I2_O)        0.124    19.645 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/ram_block_reg_0_31_30_35_i_1/O
                     net (fo=2, routed)           1.236    20.881    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/ram_block_reg_0_31_30_35/DIA1
SLICE_X36Y67         RAMD32                                       r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/ram_block_reg_0_31_30_35/RAMA_D1/I
-------------------------------------------------------------------    -------------------

                     (clock crg_s7mmcm0_clkout0 rise edge)
                                                 10.000    10.000 r
R4                                                0.000    10.000 r  clk100 (IN)
                     net (fo=0)                   0.000    10.000    clk100
R4                   IBUF (Prop_ibuf_I_O)         1.405    11.405 r  clk100_IBUF_inst/O
                     net (fo=3, routed)           0.855    12.260    clk100_IBUF
SLICE_X160Y123       LUT1 (Prop_lut1_I0_O)        0.100    12.360 r  crg_s7mmcm0_clkin_inst/O
                     net (fo=9, routed)           3.511    15.871    crg_s7mmcm0_clkin
MMCME2_ADV_X0Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                  0.083    15.954 r  MMCME2_ADV/CLKOUT0
                     net (fo=1, routed)           1.760    17.714    crg_s7mmcm0_clkout0
BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.091    17.805 r  BUFG/O
                     net (fo=55330, routed)       1.795    19.600    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/ram_block_reg_0_31_30_35/WCLK
SLICE_X36Y67         RAMD32                                       r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/ram_block_reg_0_31_30_35/RAMA_D1/CLK
                     clock pessimism              1.016    20.616
                     clock uncertainty           -0.067    20.549
SLICE_X36Y67         RAMD32 (Setup_ramd32_CLK_I)
                                                 -0.258    20.291    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/integer_RegFilePlugin_logic_regfile_fpga/asMem_ram/ram_block_reg_0_31_30_35/RAMA_D1
-------------------------------------------------------------------
                     required time                         20.291
                     arrival time                         -20.881
-------------------------------------------------------------------
                     slack                                 -0.590


Location             Delay type                Incr(ns)  Path(ns)    Netlist Resource(s)
-------------------------------------------------------------------    -------------------
                     (clock crg_s7mmcm0_clkout0 rise edge)
                                                  0.000     0.000 r
R4                                                0.000     0.000 r  clk100 (IN)
                     net (fo=0)                   0.000     0.000    clk100
R4                   IBUF (Prop_ibuf_I_O)         1.475     1.475 r  clk100_IBUF_inst/O
                     net (fo=3, routed)           0.899     2.374    clk100_IBUF
SLICE_X161Y125       LUT1 (Prop_lut1_I0_O)        0.124     2.498 r  crg_s7mmcm0_clkin_inst/O
                     net (fo=9, routed)           3.892     6.390    crg_s7mmcm0_clkin
MMCME2_ADV_X0Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                  0.088     6.478 r  MMCME2_ADV/CLKOUT0
                     net (fo=1, routed)           1.843     8.322    crg_s7mmcm0_clkout0
BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.096     8.418 r  BUFG/O
                     net (fo=56537, routed)       2.117    10.535    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/out
SLICE_X152Y38        FDRE                                         r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/fetch_logic_ctrls_1_up_Fetch_WORD_PC_reg[22]/C
-------------------------------------------------------------------    -------------------
SLICE_X152Y38        FDRE (Prop_fdre_C_Q)         0.478    11.013 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/fetch_logic_ctrls_1_up_Fetch_WORD_PC_reg[22]/Q
                     net (fo=3, routed)           1.159    12.172    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/BtbPlugin_logic_ras_read_reg[37][20]
SLICE_X155Y35        LUT6 (Prop_lut6_I4_O)        0.296    12.468 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/fetch_logic_ctrls_2_up_Prediction_WORD_JUMPED_i_7/O
                     net (fo=1, routed)           0.000    12.468    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/fetch_logic_ctrls_2_up_Prediction_WORD_JUMPED_i_7_n_0
SLICE_X155Y35        CARRY4 (Prop_carry4_S[3]_CO[3])
                                                  0.401    12.869 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/fetch_logic_ctrls_2_up_Prediction_WORD_JUMPED_reg_i_4/CO[3]
                     net (fo=1, routed)           0.000    12.869    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/fetch_logic_ctrls_2_up_Prediction_WORD_JUMPED_reg_i_4_n_0
SLICE_X155Y36        CARRY4 (Prop_carry4_CI_CO[1])
                                                  0.157    13.026 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/fetch_logic_ctrls_2_up_Prediction_WORD_JUMPED_reg_i_3/CO[1]
                     net (fo=3, routed)           0.455    13.482    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/CO[0]
SLICE_X152Y36        LUT5 (Prop_lut5_I2_O)        0.329    13.811 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/ram_block_reg_0_3_0_5_i_4/O
                     net (fo=15, routed)          0.996    14.807    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/BtbPlugin_logic_applyIt_correctionSent_reg
SLICE_X147Y37        LUT5 (Prop_lut5_I0_O)        0.124    14.931 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack/BtbPlugin_logic_ras_ptr_pop[1]_i_2/O
                     net (fo=7, routed)           1.135    16.067    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_ras_mem_stack_n_2
SLICE_X149Y43        LUT6 (Prop_lut6_I4_O)        0.124    16.191 f  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/PcPlugin_logic_harts_0_aggregator_target_inferred_i_35_comp/O
                     net (fo=5, routed)           0.423    16.614    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_mem/FetchL1Plugin_logic_banks_3_mem_reg_0[3]
SLICE_X149Y41        LUT5 (Prop_lut5_I0_O)        0.124    16.738 f  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_mem/ram_block_reg_0_63_15_17_i_5/O
                     net (fo=121, routed)         0.909    17.647    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/BtbPlugin_logic_mem_n_13
SLICE_X145Y34        LUT4 (Prop_lut4_I3_O)        0.124    17.771 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/PcPlugin_logic_harts_0_self_increment_i_7/O
                     net (fo=2, routed)           0.574    18.345    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/FetchL1Plugin_logic_refill_onRsp_holdHarts4
SLICE_X147Y34        LUT5 (Prop_lut5_I3_O)        0.124    18.469 f  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/PcPlugin_logic_harts_0_self_increment_i_10/O
                     net (fo=1, routed)           0.584    19.052    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/PcPlugin_logic_harts_0_self_increment_i_10_n_0
SLICE_X145Y34        LUT6 (Prop_lut6_I5_O)        0.124    19.176 f  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/PcPlugin_logic_harts_0_self_increment_i_6/O
                     net (fo=2, routed)           0.777    19.953    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/FetchL1Plugin_logic_refill_onRsp_holdHarts0
SLICE_X141Y40        LUT6 (Prop_lut6_I3_O)        0.124    20.077 f  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/PcPlugin_logic_harts_0_self_state[1]_i_2__0/O
                     net (fo=2, routed)           0.985    21.063    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/early0_DivPlugin_logic_processing_div/PcPlugin_logic_harts_0_self_state_reg[1]_0
SLICE_X139Y36        LUT5 (Prop_lut5_I1_O)        0.119    21.182 r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/early0_DivPlugin_logic_processing_div/fetch_logic_ctrls_1_up_valid_i_1/O
                     net (fo=1, routed)           0.000    21.182    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/early0_DivPlugin_logic_processing_div_n_158
SLICE_X139Y36        FDCE                                         r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/fetch_logic_ctrls_1_up_valid_reg/D
-------------------------------------------------------------------    -------------------

                     (clock crg_s7mmcm0_clkout0 rise edge)
                                                 10.000    10.000 r
R4                                                0.000    10.000 r  clk100 (IN)
                     net (fo=0)                   0.000    10.000    clk100
R4                   IBUF (Prop_ibuf_I_O)         1.405    11.405 r  clk100_IBUF_inst/O
                     net (fo=3, routed)           0.773    12.178    clk100_IBUF
SLICE_X161Y125       LUT1 (Prop_lut1_I0_O)        0.100    12.278 r  crg_s7mmcm0_clkin_inst/O
                     net (fo=9, routed)           3.337    15.615    crg_s7mmcm0_clkin
MMCME2_ADV_X0Y2      MMCME2_ADV (Prop_mmcme2_adv_CLKIN1_CLKOUT0)
                                                  0.083    15.698 r  MMCME2_ADV/CLKOUT0
                     net (fo=1, routed)           1.760    17.458    crg_s7mmcm0_clkout0
BUFGCTRL_X0Y0        BUFG (Prop_bufg_I_O)         0.091    17.549 r  BUFG/O
                     net (fo=56537, routed)       1.912    19.460    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/out
SLICE_X139Y36        FDCE                                         r  VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/fetch_logic_ctrls_1_up_valid_reg/C
                     clock pessimism              0.966    20.426
                     clock uncertainty           -0.067    20.359
SLICE_X139Y36        FDCE (Setup_fdce_C_D)        0.075    20.434    VexiiRiscvLitex_1bfb18e34cfdfd7820076885afabe9e6/vexiis_0_logic_core/fetch_logic_ctrls_1_up_valid_reg
-------------------------------------------------------------------
                     required time                         20.434
                     arrival time                         -21.182
-------------------------------------------------------------------
                       slack                                 -0.748
 */