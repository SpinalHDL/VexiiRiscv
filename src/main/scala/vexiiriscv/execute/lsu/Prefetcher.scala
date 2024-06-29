package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.fetch.LsuService

case class PrefetchCmd() extends Bundle {
  val address = LsuL1.MIXED_ADDRESS()
  val unique = Bool()
}

case class LsuCommitProbe() extends Bundle {
  val address = LsuL1.MIXED_ADDRESS()
  val load, store, trap = Bool()
}


abstract class PrefetcherPlugin extends FiberPlugin {
  val io = during build Stream(PrefetchCmd())
}

class PrefetchNextLinePlugin extends PrefetcherPlugin {
  val logic = during build new Area {
    val probe = host[LsuService].lsuCommitProbe
    val converted = Stream(PrefetchCmd())
    converted.arbitrationFrom(probe.toStream)
    converted.address := probe.address + 64
    converted.unique := probe.store
    io << converted.stage()
  }
}


/*
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

Write speed: 166.7MiB/s
 Read speed: 113.3MiB/s

Write speed: 165.8MiB/s
 Read speed: 204.4MiB/s



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
 */