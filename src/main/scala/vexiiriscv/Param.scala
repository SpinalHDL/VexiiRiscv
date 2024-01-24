package vexiiriscv

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.Hostable
import vexiiriscv._
import vexiiriscv.decode.DecoderPlugin
import vexiiriscv.execute._
import vexiiriscv.memory.{MmuPortParameter, MmuSpec, MmuStorageLevel, MmuStorageParameter}
import vexiiriscv.misc._
import vexiiriscv.prediction.{LearnCmd, LearnPlugin}
import vexiiriscv.riscv.IntRegFile
import vexiiriscv.schedule.DispatchPlugin
import vexiiriscv.test.WhiteboxerPlugin

import scala.collection.mutable.ArrayBuffer

class ParamSimple(){
  var xlen = 32
  var withRvc = false
  var withAlignerBuffer = false
  var hartCount = 1
  var withMmu = false
  var resetVector = 0x80000000l
  var decoders = 1
  var lanes = 1
  var regFileSync = true
  var ioRange    : UInt => Bool = a => a(31 downto 28) === 0x1
  var fetchRange : UInt => Bool = a => a(31 downto 28) =/= 0x1
  var withGShare = false
  var withBtb = false
  var withRas = false
  var withLateAlu = false
  var withMul = true
  var withDiv = true
  var withRva = false
  var privParam = PrivilegedParam.base
  var relaxedBranch = false
  var relaxedShift = false
  var relaxedSrc = true
  var allowBypassFrom = 100 //100 => disabled
  var performanceCounters = 0
  var withFetchL1 = false
  var withLsuL1 = false

  //  Debug modifiers
  val debugParam = sys.env.getOrElse("VEXIIRISCV_DEBUG_PARAM", "0").toInt.toBoolean
  if(debugParam) {
    decoders = 2
    lanes = 2
    regFileSync = false
    withGShare = true
    withBtb = true
    withRas = true
//    withMul = false
//    withDiv = false
    withLateAlu = true
    allowBypassFrom = 0
    relaxedBranch = false
    relaxedShift = false
    relaxedSrc = true
    performanceCounters = 4
    privParam.withSupervisor = true
    privParam.withUser = true
    withMmu = true
    withRva = true
    withRvc = false
    withAlignerBuffer = withRvc
    withFetchL1 = false
    withLsuL1 = true
    xlen = 32
  }


  def getName() : String = {
    def opt(that : Boolean, v : String) = that.mux(v, "")
    var isa = s"rv${xlen}i"
    if (withMul) isa += "m"
    if (withRva) isa += "a"
    if (withRvc) isa += "c"
    if (privParam.withSupervisor) isa += "s"
    if (privParam.withUser) isa += "u"
    val r = new ArrayBuffer[String]()
    r += isa
    r += s"d${decoders}"
    r += s"l${lanes}"
    r += regFileSync.mux("rfs","rfa")
    if (withFetchL1) r += "fl1"
    if (withLsuL1) r += "lsul1"
    if(allowBypassFrom < 100) r += s"bp$allowBypassFrom"
    if (withBtb) r += "btb"
    if (withRas) r += "ras"
    if (withGShare) r += "gshare"
    if (withLateAlu) r += "la"
    if (withAlignerBuffer) r += "ab"
    if (relaxedBranch) r += "rbra"
    if (relaxedShift) r += "rsft"
    if (relaxedSrc) r += "rsrc"
    if(performanceCounters != 0) r += s"pc$performanceCounters"
    r.mkString("_")
  }

  def addOptions(parser: scopt.OptionParser[Unit]): Unit = {
    import parser._
    opt[Int]("xlen") action { (v, c) => xlen = v }
    opt[Int]("decoders") action { (v, c) => decoders = v }
    opt[Int]("lanes") action { (v, c) => lanes = v }
    opt[Unit]("relaxed-branch") action { (v, c) => relaxedBranch = true }
    opt[Unit]("relaxed-shift") action { (v, c) => relaxedShift = true }
    opt[Unit]("relaxed-src") action { (v, c) => relaxedSrc = true }
    opt[Unit]("with-mul") unbounded() action { (v, c) => withMul = true }
    opt[Unit]("with-div") unbounded() action { (v, c) => withDiv = true }
    opt[Unit]("with-rva") action { (v, c) => withRva = true }
    opt[Unit]("with-rvc") action { (v, c) => withRvc = true; withAlignerBuffer = true }
    opt[Unit]("with-supervisor") action { (v, c) => privParam.withSupervisor = true; privParam.withUser = true; withMmu = true }
    opt[Unit]("with-user") action { (v, c) => privParam.withUser = true }
    opt[Unit]("without-mul") action { (v, c) => withMul = false }
    opt[Unit]("without-div") action { (v, c) => withDiv = false }
    opt[Unit]("with-mul") action { (v, c) => withMul = true }
    opt[Unit]("with-div") action { (v, c) => withDiv = true }
    opt[Unit]("with-gshare") action { (v, c) => withGShare = true }
    opt[Unit]("with-btb") action { (v, c) => withBtb = true }
    opt[Unit]("with-ras") action { (v, c) => withRas = true }
    opt[Unit]("with-late-alu") action { (v, c) => withLateAlu = true; allowBypassFrom = 0 }
    opt[Unit]("regfile-async") action { (v, c) => regFileSync = false }
    opt[Unit]("regfile-sync") action { (v, c) => regFileSync = true }
    opt[Int]("allow-bypass-from") action { (v, c) => allowBypassFrom = v }
    opt[Int]("performance-counters") action { (v, c) => performanceCounters = v }
    opt[Unit]("with-fetch-l1") action { (v, c) => withFetchL1 = true }
    opt[Unit]("with-lsu-l1") action { (v, c) => withLsuL1 = true }
  }

  def plugins() = pluginsArea.plugins
  def pluginsArea() = new Area {
    val plugins = ArrayBuffer[Hostable]()
    if(withLateAlu) assert(allowBypassFrom == 0)

    plugins += new riscv.RiscvPlugin(xlen, hartCount, rvc = withRvc)
    withMmu match {
      case false => plugins += new memory.StaticTranslationPlugin(32, ioRange, fetchRange)
      case true => plugins += new memory.MmuPlugin(
        spec = if (xlen == 32) MmuSpec.sv32 else MmuSpec.sv39,
        ioRange = ioRange,
        fetchRange = fetchRange,
        physicalWidth = 32
      )
    }

    plugins += new misc.PipelineBuilderPlugin()
    plugins += new schedule.ReschedulePlugin()

    plugins += new LearnPlugin()
    if(withRas) assert(withBtb)
    if(withGShare) assert(withBtb)
    if(withBtb) {
      plugins += new prediction.BtbPlugin(
        sets = 512 / decoders,
        chunks = decoders,
        rasDepth = if(withRas) 4 else 0,
        hashWidth = 16,
        readAt = 0,
        hitAt = 1,
        jumpAt = 1
      )
      plugins += new prediction.DecodePredictionPlugin(
        decodeAt = 1,
        jumpAt = 1
      )
    }
    if(withGShare) {
      plugins += new prediction.GSharePlugin (
        memBytes = 4 KiB,
        historyWidth = 12,
        readAt = 0
      )
      plugins += new prediction.HistoryPlugin()
    }


    plugins += new fetch.PcPlugin(resetVector)
    plugins += new fetch.FetchPipelinePlugin()
    if(!withFetchL1) plugins += new fetch.FetchCachelessPlugin(
      forkAt = 0,
      joinAt = 1, //You can for instance allow the external memory to have more latency by changing this
      wordWidth = 32*decoders,
      translationStorageParameter = MmuStorageParameter(
        levels = List(
          MmuStorageLevel(
            id = 0,
            ways = 4,
            depth = 32
          ),
          MmuStorageLevel(
            id = 1,
            ways = 2,
            depth = 32
          )
        ),
        priority = 0
      ),
      translationPortParameter = withMmu match {
        case false => null
        case true => MmuPortParameter(
          readAt = 0,
          hitsAt = 0,
          ctrlAt = 0,
          rspAt = 0
        )
      }
    )
    if(withFetchL1) plugins += new fetch.FetchL1Plugin(
      lineSize = 64,
      setCount = 64,
      wayCount = 4,
      fetchDataWidth = 32*decoders,
      memDataWidth = 32*decoders,
      reducedBankWidth = false,
      hitsWithTranslationWays = true,
      tagsReadAsync = false,
      translationStorageParameter = MmuStorageParameter(
        levels = List(
          MmuStorageLevel(
            id = 0,
            ways = 4,
            depth = 32
          ),
          MmuStorageLevel(
            id = 1,
            ways = 2,
            depth = 32
          )
        ),
        priority = 0
      ),
      translationPortParameter = withMmu match {
        case false => null
        case true => MmuPortParameter(
          readAt = 1,
          hitsAt = 1,
          ctrlAt = 1,
          rspAt = 1
        )
      }
    )

    plugins += new decode.DecodePipelinePlugin()
    plugins += new decode.AlignerPlugin(
      fetchAt = withFetchL1.mux(2, 1),
      lanes = decoders,
      withBuffer = withAlignerBuffer
    )
    plugins += new decode.DecoderPlugin(
      decodeAt = 1
    )
    plugins += new schedule.DispatchPlugin(
      dispatchAt = 1,
      trapLayer = null
    )

    plugins += new regfile.RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 32,
      preferedWritePortForInit = "lane0",
      syncRead = regFileSync
    )

    def newExecuteLanePlugin(name : String) = new execute.ExecuteLanePlugin(
      name,
      rfReadAt = 0,
      decodeAt = regFileSync.toInt,
      executeAt = regFileSync.toInt + 1,
      withBypasses = allowBypassFrom == 0
    )

    plugins += new execute.ExecutePipelinePlugin()

    val lane0 = newExecuteLanePlugin("lane0")
    val early0 = new LaneLayer("early0", lane0, priority = 0)
    plugins += lane0

//    plugins += new RedoPlugin("lane0")
    plugins += new SrcPlugin(early0, executeAt = 0, relaxedRs = relaxedSrc)
    plugins += new IntAluPlugin(early0, formatAt = 0)
    plugins += new BarrelShifterPlugin(early0, formatAt = relaxedShift.toInt)
    plugins += new IntFormatPlugin("lane0")
    plugins += new BranchPlugin(layer=early0, aluAt=0, jumpAt=relaxedBranch.toInt, wbAt=0)
    if(!withLsuL1) plugins += new LsuCachelessPlugin(
      layer     = early0,
      withAmo   = withRva,
      withSpeculativeLoadFlush = true,
      addressAt = 0,
      forkAt    = 0,
      joinAt    = 1,
      wbAt      = 2,
      translationStorageParameter = MmuStorageParameter(
        levels = List(
          MmuStorageLevel(
            id = 0,
            ways = 4,
            depth = 32
          ),
          MmuStorageLevel(
            id = 1,
            ways = 2,
            depth = 32
          )
        ),
        priority = 1
      ),
      translationPortParameter = withMmu match {
        case false => null
        case true => MmuPortParameter(
          readAt = 0,
          hitsAt = 0,
          ctrlAt = 0,
          rspAt = 0
        )
      }
    )
    if(withLsuL1){
      plugins += new LsuPlugin(
        layer = early0,
        withRva = withRva
      )
      plugins += new LsuL1Plugin(
        lane           = lane0,
        memDataWidth   = xlen,
        cpuDataWidth   = xlen,
        refillCount    = 1,
        writebackCount = 1,
        setCount       = 64,
        wayCount       = 1
      )
    }

    if(withMul) {
      plugins += new MulPlugin(early0)
    }
    if(withDiv) {
      plugins += new RsUnsignedPlugin("lane0")
      plugins += new DivPlugin(early0)
    }

    plugins += new CsrRamPlugin()
    plugins += new PerformanceCounterPlugin(additionalCounterCount = performanceCounters)
    plugins += new CsrAccessPlugin(early0, writeBackKey =  if(lanes == 1) "lane0" else "lane1")
    plugins += new PrivilegedPlugin(privParam, 0 until hartCount)
    plugins += new TrapPlugin(trapAt = 2)
    plugins += new EnvPlugin(early0, executeAt = 0)

    if(withLateAlu) {
      val late0 = new LaneLayer("late0", lane0, priority = -5)
      plugins += new SrcPlugin(late0, executeAt = 2, relaxedRs = relaxedSrc)
      plugins += new IntAluPlugin(late0, aluAt = 2, formatAt = 2)
      plugins += new BarrelShifterPlugin(late0, shiftAt = 2, formatAt = 2)
      plugins += new BranchPlugin(late0, aluAt = 2, jumpAt = 2/*+relaxedBranch.toInt*/, wbAt = 2)
    }

    plugins += new WriteBackPlugin("lane0", IntRegFile, writeAt = 2, allowBypassFrom = allowBypassFrom)


    if(lanes >= 2) {
      val lane1 = newExecuteLanePlugin("lane1")
      val early1 = new LaneLayer("early1", lane1, priority = 10)
      plugins += lane1

      plugins += new SrcPlugin(early1, executeAt = 0, relaxedRs = relaxedSrc)
      plugins += new IntAluPlugin(early1, formatAt = 0)
      plugins += new BarrelShifterPlugin(early1, formatAt = relaxedShift.toInt)
      plugins += new IntFormatPlugin("lane1")
      plugins += new BranchPlugin(early1, aluAt = 0, jumpAt = relaxedBranch.toInt, wbAt = 0)

      if(withLateAlu) {
        val late1 = new LaneLayer("late1", lane1, priority = -3)
        plugins += new SrcPlugin(late1, executeAt = 2, relaxedRs = relaxedSrc)
        plugins += new IntAluPlugin(late1, aluAt = 2, formatAt = 2)
        plugins += new BarrelShifterPlugin(late1, shiftAt = 2, formatAt = 2)
        plugins += new BranchPlugin(late1, aluAt = 2, jumpAt = 2/*+relaxedBranch.toInt*/, wbAt = 2)
      }

      plugins += new WriteBackPlugin("lane1", IntRegFile, writeAt = 2, allowBypassFrom = allowBypassFrom)
    }

    plugins.foreach {
      case p: DispatchPlugin => p.trapLayer = early0
      case _ =>
    }

    plugins += new WhiteboxerPlugin()
  }
}

/*
jump at 0 :
1l btb gshare ras => 1.64 dhrystone 3.26 coremark 1.04 embench
       + late alu => 1.72 dhrystone 3.54 coremark 1.10
2l btb gshare ras => 1.92 dhrystone 3.93 coremark 1.34 embench
       + late alu => 2.09 dhrystone 4.39 coremark
    + 4b late alu => 2.24 dhrystone 4.55 coremark 1.47 embench



jump at 1
1l btb gshare ras => 1.64 dhrystone 3.21 coremark 1.03 embench
2l btb gshare ras => 1.91 dhrystone 3.83 coremark 1.32 embench

 coremark 2l => Branch : 523630 47815   9.1%

btb ras gshare at 1 :
vexii_1i ->
Artix 7 -> 90 Mhz 1619 LUT 1038 FF
Artix 7 -> 166 Mhz 2423 LUT 1091 FF
vexii_2i ->
Artix 7 -> 90 Mhz 2658 LUT 1314 FF
Artix 7 -> 134 Mhz 3136 LUT 1370 FF

btb ras gshare at 2 :
vexii_1i ->
Artix 7 -> 90 Mhz 1554 LUT 1045 FF
Artix 7 -> 196 Mhz 1705 LUT 1045 FF
vexii_2i -> embench 1.21
Artix 7 -> 90 Mhz 2808 LUT 1349 FF
Artix 7 -> 137 Mhz 2949 LUT 1352 FF

  -mtune=sifive-7-series
1.51
3.11

1.51
3.20

dual issue, btb, no gshare =>

1.74 Dhrystone/MHz
3.66 Coremark/MHz
aha-mont64           1.41
crc32                1.00
cubic                0.60
edn                  1.08
huffbench            1.46
matmult-int          1.01
md5sum               1.82
minver               0.81
nbody                1.01
nettle-aes           1.53
nettle-sha256        1.61
nsichneu             0.79
picojpeg             1.14
primecount           1.46
qrduino              1.41
sglib-combined       1.21
slre                 1.54
st                   1.21
statemate            1.96
tarfind              2.05
ud                   1.15
wikisort             1.78
---------           -----
Geometric mean       1.26


vexii_1i ->
Artix 7 -> 90 Mhz 1466 LUT 907 FF
Artix 7 -> 189 Mhz 1946 LUT 960 FF
vexii_2i ->
Artix 7 -> 90 Mhz 2596 LUT 1153 FF
Artix 7 -> 135 Mhz 3136 LUT 1207 FF

vexii_1i ->
Artix 7 -> 90 Mhz 1054 LUT 737 FF
Artix 7 -> 193 Mhz 1498 LUT 789 FF
vexii_2i ->
Artix 7 -> 90 Mhz 2271 LUT 980 FF
Artix 7 -> 133 Mhz 2777 LUT 1033 FF




lane micro op spec
- For each impl
  - readRsFrom
  - rd bypass from (executeId)
  - completion at (probe)
  - mayFlushUpTo
  - dontFlushFrom
 */

