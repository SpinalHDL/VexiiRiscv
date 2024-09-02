package vexiiriscv

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.{M2sTransfers, SizeRange}
import spinal.lib.cpu.riscv.debug.DebugTransportModuleParameter
import spinal.lib.misc.plugin.Hostable
import spinal.lib.system.tag.{PmaRegion, PmaRegionImpl}
import vexiiriscv._
import vexiiriscv.decode.DecoderPlugin
import vexiiriscv.execute._
import vexiiriscv.execute.lsu._
import vexiiriscv.fetch.{FetchCachelessPlugin, FetchL1Plugin, PrefetcherNextLinePlugin}
import vexiiriscv.memory.{MmuPortParameter, MmuSpec, MmuStorageLevel, MmuStorageParameter}
import vexiiriscv.misc._
import vexiiriscv.prediction.{LearnCmd, LearnPlugin}
import vexiiriscv.riscv.{FloatRegFile, IntRegFile}
import vexiiriscv.schedule.DispatchPlugin
import vexiiriscv.test.WhiteboxerPlugin

import scala.collection.mutable.ArrayBuffer

object ParamSimple{

  def addptionRegion(parser: scopt.OptionParser[Unit], regions : ArrayBuffer[PmaRegion]): Unit = {
    import parser._
    opt[Map[String, String]]("region") unbounded() action { (v, c) =>
      regions += PmaRegionImpl(
        mapping = SizeMapping(BigInt(v("base"), 16), BigInt(v("size"), 16)),
        transfers = M2sTransfers.all,
        isMain = v("main") == "1",
        isExecutable = v("exe") == "1"
      )
    } text ("Specify a memory region, for instance : --region base=80000000,size=80000000,main=1,exe=1 --region base=10000000,size=10000000,main=0,exe=0")
  }

  val defaultPma = List[PmaRegion](
    new PmaRegionImpl(
      mapping = SizeMapping(0x80000000l, 0x80000000l),
      isMain = true,
      isExecutable = true,
      transfers = M2sTransfers(
        get = SizeRange.all,
        putFull = SizeRange.all
      )
    ),
    new PmaRegionImpl(
      mapping = SizeMapping(0x10000000l, 0x10000000l),
      isMain = false,
      isExecutable = true,
      transfers = M2sTransfers(
        get = SizeRange.all,
        putFull = SizeRange.all
      )
    )
  )

  def setPma(plugins : Seq[Hostable], regions : Seq[PmaRegion] = defaultPma) = {
    val array = ArrayBuffer(regions :_*)
    plugins.foreach {
      case p: FetchCachelessPlugin => p.regions.load(array)
      case p: LsuCachelessPlugin => p.regions.load(array)
      case p: FetchL1Plugin => p.regions.load(array)
      case p: LsuPlugin => p.ioRegions.load(array)
      case p: LsuL1Plugin => p.regions.load(array)
      case _ =>
    }
    plugins
  }
}

class ParamSimple(){
  var xlen = 32
  var withRvc = false
  var withAlignerBuffer = false
  var withDispatcherBuffer = false
  var hartCount = 1
  var withMmu = false
  var physicalWidth = 32
  var resetVector = 0x80000000l
  var decoders = 1
  var lanes = 1
  var decoderAt = 1
  var dispatcherAt = 1
  var regFileSync = true
  var regFileDualPortRam = true
  var withGShare = false
  var withBtb = false
  var withRas = false
  var withLateAlu = false
  var storeRs2Late = false
  var withMul = false
  var withDiv = false
  var withRva = false
  var withRvf = false
  var skipFma = false
  var fpuFmaFullAccuracy = true
  var fpuIgnoreSubnormal = false
  var withRvd = false
  var withRvZb = false
  var privParam = PrivilegedParam.base
  var lsuForkAt = 0
  var lsuPmaAt = 0
  var relaxedBranch = false
  var relaxedShift = false
  var relaxedSrc = true
  var relaxedBtb = false
  var relaxedDiv = false
  var allowBypassFrom = 100 //100 => disabled
  var additionalPerformanceCounters = 0
  var withPerformanceCounters = false
  var fetchL1Enable = false
  var fetchL1Sets = 64
  var fetchL1Ways = 1
  var fetchL1ReducedBank = false
  var fetchMemDataWidthMin = 32
  var fetchL1RefillCount = 1
  var fetchL1Prefetch = "none"
  var lsuSoftwarePrefetch = false
  var lsuHardwarePrefetch = "none"
  var lsuStoreBufferSlots = 0
  var lsuStoreBufferOps = 0
  var lsuL1Enable = false
  var lsuL1Sets = 64
  var lsuL1Ways = 1
  var lsuL1RefillCount = 1
  var lsuL1WritebackCount = 1
  var lsuL1Coherency = false
  var lsuMemDataWidthMin = 32
  var withLsuBypass = false
  var withIterativeShift = false
  var divRadix = 2
  var divImpl = ""
  var divArea = true
  var fetchForkAt = 0
  var btbSets = 512
  var btbHashWidth = 16
  var embeddedJtagTap = false
  var embeddedJtagInstruction = false
  var embeddedJtagCd: ClockDomain = null
  var embeddedJtagNoTapCd: ClockDomain = null
  var bootMemClear = false
  var mulKeepSrc = false

  var fetchTsp = MmuStorageParameter(
    levels = List(
      MmuStorageLevel(
        id = 0,
        ways = 2,
        depth = 32
      ),
      MmuStorageLevel(
        id = 1,
        ways = 1,
        depth = 32
      )
    ),
    priority = 0
  )

  var lsuTsp = MmuStorageParameter(
    levels = List(
      MmuStorageLevel(
        id = 0,
        ways = 3,
        depth = 32
      ),
      MmuStorageLevel(
        id = 1,
        ways = 1,
        depth = 32
      )
    ),
    priority = 1
  )

  var fetchTpp = MmuPortParameter(
    readAt = 1,
    hitsAt = 1,
    ctrlAt = 1,
    rspAt = 1
  )

  var lsuTpp = MmuPortParameter(
    readAt = 0,
    hitsAt = 0,
    ctrlAt = 1,
    rspAt = 1
  )


  def fetchMemDataWidth = 32*decoders max fetchMemDataWidthMin
  def lsuMemDataWidth = xlen max lsuMemDataWidthMin
  def memDataWidth = List(fetchMemDataWidth, lsuMemDataWidth).max

  //  Debug modifiers
  val debugParam = sys.env.getOrElse("VEXIIRISCV_DEBUG_PARAM", "0").toInt.toBoolean
  if(debugParam) {
    withPerformanceCounters = true
    additionalPerformanceCounters = 4
    regFileSync = false
    allowBypassFrom = 0

    withGShare = true
    withBtb = true
    withRas = true
    //    relaxedBranch = true  // !!
//    relaxedBtb = true     // !!
    fetchL1Enable = true
    fetchL1Sets = 64
    fetchL1Ways = 4
    fetchL1ReducedBank = true
    fetchMemDataWidthMin = 64
    fetchL1RefillCount = 2
    fetchL1Prefetch = "nl"
    lsuL1Enable = true
    lsuMemDataWidthMin = 64
    lsuL1Sets = 64
    lsuL1Ways = 4
    lsuL1RefillCount = 8
    lsuL1WritebackCount = 8
    lsuL1Coherency = false
//    lsuStoreBufferSlots = 2
//    lsuStoreBufferOps = 32
    lsuStoreBufferSlots = 4
    lsuStoreBufferOps = 32
    withLsuBypass = true
//    lsuSoftwarePrefetch = true
    lsuHardwarePrefetch = "rpt"

    //    lsuForkAt = 1
    divArea = false
    divRadix = 2
//    decoders = 2
//    lanes = 2
//    storeRs2Late = true
//    withLateAlu = true
    withMul = true
    withDiv = true
    withDispatcherBuffer = true
    withAlignerBuffer = true
    withRvc = true
    withRva = true

    withRvf = true
    withRvd = true
//    fpuIgnoreSubnormal = true
//    fpuFmaFullAccuracy = false

    withMmu = true
    privParam.withSupervisor = true
    privParam.withUser = true
    xlen = 64
    physicalWidth = 38


    privParam.withDebug = true
    privParam.debugTriggers = 4
    privParam.debugTriggersLsu = true
    embeddedJtagTap = true


//    decoders = 2
//    lanes = 2
//    regFileSync = false
//    withGShare = true
//    withBtb = true
//    withRas = true
////    withMul = false
////    withDiv = false
//    withLateAlu = true
//    allowBypassFrom = 0
//    relaxedBranch = false
//    relaxedShift = false
//    relaxedSrc = true
//    additionalPerformanceCounters = 4
//    privParam.withSupervisor = true
//    privParam.withUser = true
//    withMmu = true
//    withRva = true
//    withRvc = false
//    withAlignerBuffer = withRvc
//    withFetchL1 = false
//    withLsuL1 = false
//    xlen = 32
//    fetchL1Sets = 64
//    fetchL1Ways = 4
//    lsuL1Sets = 64
//    lsuL1Ways = 4
//    withLsuBypass = true
//    divImpl = ""
//    divRadix = 2
//    divArea = true
//    lsuForkAt = 1
  }


  def getName() : String = {
    def opt(that : Boolean, v : String) = that.mux(v, "")
    var isa = s"rv${xlen}i"
    if (withMul) isa += s"m"
    if (withRva) isa += "a"
    if (withRvf) isa += "f"
    if (withRvd) isa += "d"
    if (withRvc) isa += "c"
    if (withRvZb) isa += "ZbaZbbZbcZbs"
    if (privParam.withSupervisor) isa += "s"
    if (privParam.withUser) isa += "u"
    val r = new ArrayBuffer[String]()
    r += isa
    r += s"d${decoders}At${decoderAt}"
    r += s"l${lanes}"
    r += s"disAt${dispatcherAt}"
    r += regFileSync.mux("rfs","rfa") + regFileDualPortRam.mux("Dp","Mem")
    if (fetchL1Enable) r += s"fl1xW${lsuL1Ways}xS${lsuL1Sets}Dwm$fetchMemDataWidth${fetchL1ReducedBank.mux("Rb", "")}" else r += s"fclF${fetchForkAt}dw${fetchMemDataWidth}"
    if (lsuL1Enable) r += s"lsul1xW${lsuL1Ways}xS${lsuL1Sets}${withLsuBypass.mux("xBp","")}Sb${lsuStoreBufferSlots}w${lsuStoreBufferOps}dw${lsuMemDataWidth}rc${lsuL1RefillCount}wc${lsuL1WritebackCount}${lsuL1Coherency.mux("Co", "")}" else r += s"lsuP${lsuPmaAt}F${lsuForkAt}dw$lsuMemDataWidth"
    if(allowBypassFrom < 100) r += s"bp$allowBypassFrom"
    if (withBtb) r += s"btbS${btbSets}H${btbHashWidth}${if(relaxedBtb)"R" else ""}"
    if (withRas) r += "ras"
    if (withGShare) r += "gshare"
    if (withLateAlu) r += "la"
    if (withAlignerBuffer) r += "ab"
    if (withDispatcherBuffer) r += "db"
    if (relaxedBranch) r += "rbra"
    if (relaxedShift) r += "rsft"
    if (relaxedSrc) r += "rsrc"
    if (withPerformanceCounters) r += s"pc$additionalPerformanceCounters"
    if (withIterativeShift) r += "isft"
    if (withDiv) r += s"d${divRadix}${divImpl}${if(divArea)"Area" else ""}"
    if (privParam.withDebug) r += s"pdbg"
    if (embeddedJtagTap) r += s"jtagt"
    if (embeddedJtagInstruction) r += s"jtagi"
    r.mkString("_")
  }

  def addOptions(parser: scopt.OptionParser[Unit]): Unit = {
    import parser._
    opt[Int]("xlen") action { (v, c) => xlen = v }
    opt[Int]("decoders") action { (v, c) => decoders = v }
    opt[Int]("lanes") action { (v, c) => lanes = v }
    opt[Int]("decoder-at") action { (v, c) => decoderAt = v }
    opt[Int]("dispatcher-at") action { (v, c) => dispatcherAt = v }
    opt[Long]("reset-vector") unbounded() action { (v, c) => resetVector = v }
    opt[Unit]("relaxed-div") action { (v, c) => relaxedDiv = true }
    opt[Unit]("relaxed-branch") action { (v, c) => relaxedBranch = true }
    opt[Unit]("relaxed-shift") action { (v, c) => relaxedShift = true }
    opt[Unit]("relaxed-src") action { (v, c) => relaxedSrc = true }
    opt[Unit]("relaxed-btb") action { (v, c) => relaxedBtb = true }
    opt[Unit]("stressed-btb") action { (v, c) => relaxedBtb = false }
    opt[Unit]("stressed-div") action { (v, c) => relaxedDiv = false }
    opt[Unit]("stressed-branch") action { (v, c) => relaxedBranch = false }
    opt[Unit]("stressed-shift") action { (v, c) => relaxedShift = false }
    opt[Unit]("stressed-src") action { (v, c) => relaxedSrc = false }
    opt[Unit]("with-mul") unbounded() action { (v, c) => withMul = true }
    opt[Unit]("with-div") unbounded() action { (v, c) => withDiv = true }
    opt[Unit]("with-rva") action { (v, c) => withRva = true }
    opt[Unit]("with-rvf") action { (v, c) => withRvf = true }
    opt[Unit]("with-rvd") action { (v, c) => withRvd = true; withRvf = true }
    opt[Unit]("with-rvc") action { (v, c) => withRvc = true; withAlignerBuffer = true }
    opt[Unit]("with-rvZb") action { (v, c) => withRvZb = true }
    opt[Unit]("fma-reduced-accuracy") action { (v, c) => fpuFmaFullAccuracy = false }
    opt[Unit]("fpu-ignore-subnormal") action { (v, c) => fpuIgnoreSubnormal = true }
    opt[Unit]("with-aligner-buffer") unbounded() action { (v, c) => withAlignerBuffer = true }
    opt[Unit]("with-dispatcher-buffer") action { (v, c) => withDispatcherBuffer = true }
    opt[Unit]("with-supervisor") action { (v, c) => privParam.withSupervisor = true; privParam.withUser = true; withMmu = true }
    opt[Unit]("with-user") action { (v, c) => privParam.withUser = true }
    opt[Unit]("without-mul") action { (v, c) => withMul = false }
    opt[Unit]("without-div") action { (v, c) => withDiv = false }
    opt[Unit]("with-mul") action { (v, c) => withMul = true }
    opt[Unit]("with-div") action { (v, c) => withDiv = true }
    opt[Unit]("with-gshare") action { (v, c) => withGShare = true }
    opt[Unit]("with-btb") action { (v, c) => withBtb = true }
    opt[Unit]("with-ras") action { (v, c) => withRas = true }
    opt[Unit]("with-late-alu") action { (v, c) => withLateAlu = true; allowBypassFrom = 0; storeRs2Late = true }
    opt[Unit]("with-store-rs2-late") action { (v, c) => storeRs2Late = true }
    opt[Int]("btb-sets") action { (v, c) => btbSets = v }
    opt[Int]("btb-hash-width") action { (v, c) => btbHashWidth = v }
    opt[Unit]("regfile-async") action { (v, c) => regFileSync = false }
    opt[Unit]("regfile-sync") action { (v, c) => regFileSync = true }
    opt[Unit]("regfile-dual-ports") action { (v, c) => regFileDualPortRam = true }
    opt[Unit]("regfile-infer-ports") action { (v, c) => regFileDualPortRam = false }
    opt[Int]("allow-bypass-from") action { (v, c) => allowBypassFrom = v }
    opt[Int]("performance-counters") unbounded() action { (v, c) => withPerformanceCounters = true; additionalPerformanceCounters = v }
    opt[Unit]("with-fetch-l1") unbounded() action { (v, c) => fetchL1Enable = true }
    opt[Unit]("with-lsu-l1") action { (v, c) => lsuL1Enable = true }
    opt[Unit]("fetch-l1") action { (v, c) => fetchL1Enable = true }
    opt[Unit]("lsu-l1") action { (v, c) => lsuL1Enable = true }
    opt[Int]("fetch-l1-sets") unbounded() action { (v, c) => fetchL1Sets = v }
    opt[Int]("fetch-l1-ways") unbounded() action { (v, c) => fetchL1Ways = v }
    opt[Int]("fetch-l1-refill-count") unbounded() action { (v, c) => fetchL1RefillCount = v }
    opt[String]("fetch-l1-hardware-prefetch") action { (v, c) => fetchL1Prefetch = v }
    opt[Int]("fetch-l1-mem-data-width-min") unbounded() action { (v, c) => fetchMemDataWidthMin = v }
    opt[Unit]("fetch-reduced-bank") action { (v, c) => fetchL1ReducedBank = true }
    opt[Int]("lsu-l1-sets") unbounded() action { (v, c) => lsuL1Sets = v }
    opt[Int]("lsu-l1-ways") unbounded() action { (v, c) => lsuL1Ways = v }
    opt[Int]("lsu-l1-store-buffer-slots") action { (v, c) => lsuStoreBufferSlots = v }
    opt[Int]("lsu-l1-store-buffer-ops") action { (v, c) => lsuStoreBufferOps = v }
    opt[String]("lsu-hardware-prefetch") action { (v, c) => lsuHardwarePrefetch = v }
    opt[Unit]("lsu-software-prefetch") action { (v, c) => lsuSoftwarePrefetch = true }
    opt[Int]("lsu-l1-refill-count") action { (v, c) => lsuL1RefillCount = v }
    opt[Int]("lsu-l1-writeback-count") action { (v, c) => lsuL1WritebackCount = v }
    opt[Int]("lsu-l1-mem-data-width-min") unbounded() action { (v, c) => lsuMemDataWidthMin = v }
    opt[Unit]("lsu-l1-coherency") action { (v, c) => lsuL1Coherency = true}
    opt[Unit]("with-lsu-bypass") action { (v, c) => withLsuBypass = true }
    opt[Unit]("with-iterative-shift") action { (v, c) => withIterativeShift = true }
    opt[Int]("div-radix") action { (v, c) => divRadix = v }
    opt[String]("div-impl") action { (v, c) => divImpl = v }
    opt[Unit]("div-ipc") action { (v, c) => divArea = false }
    opt[Int]("fetch-fork-at") action { (v, c) => fetchForkAt = v }
    opt[Int]("lsu-fork-at") action { (v, c) => lsuForkAt = v }
    opt[Int]("lsu-pma-at") action { (v, c) => lsuPmaAt = v }
    opt[Unit]("debug-privileged") action { (v, c) => privParam.withDebug = true }
    opt[Int] ("debug-triggers") action { (v, c) => privParam.debugTriggers = v }
    opt[Unit]("debug-triggers-lsu") action { (v, c) => privParam.debugTriggersLsu = true }
    opt[Unit]("debug-jtag-tap") action { (v, c) => embeddedJtagTap = true }
    opt[Unit]("with-boot-mem-init") action { (v, c) => bootMemClear = true }
    opt[Int]("physical-width") action { (v, c) => physicalWidth = v }
    opt[Unit]("mul-keep-src") action { (v, c) => mulKeepSrc = true }
    opt[Unit]("mmu-sync-read") action { (v, c) =>
      fetchTsp = MmuStorageParameter(
        levels = List(
          MmuStorageLevel(
            id = 0,
            ways = 2,
            depth = 64
          ),
          MmuStorageLevel(
            id = 1,
            ways = 1,
            depth = 64
          )
        ),
        priority = 0
      )

      lsuTsp = MmuStorageParameter(
        levels = List(
          MmuStorageLevel(
            id = 0,
            ways = 2,
            depth = 64
          ),
          MmuStorageLevel(
            id = 1,
            ways = 1,
            depth = 64
          )
        ),
        priority = 1
      )

      fetchTpp = MmuPortParameter(
        readAt = 1,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt = 1
      )

      lsuTpp = MmuPortParameter(
        readAt = 1,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt = 1
      )
    }
  }


  def plugins(hartId : Int = 0) = pluginsArea(hartId).plugins
  def pluginsArea(hartId : Int = 0) = new Area {
    val plugins = ArrayBuffer[Hostable]()
    if(withLateAlu) assert(allowBypassFrom == 0)

    val intWritebackAt = 2 //Alias for "trap at" aswell

    plugins += new riscv.RiscvPlugin(xlen, hartCount, rvf = withRvf, rvd = withRvd, rvc = withRvc)
    withMmu match {
      case false => plugins += new memory.StaticTranslationPlugin(physicalWidth)
      case true => plugins += new memory.MmuPlugin(
        spec = if (xlen == 32) MmuSpec.sv32 else MmuSpec.sv39,
        physicalWidth = physicalWidth
      )
    }

    plugins += new misc.PipelineBuilderPlugin()
    plugins += new schedule.ReschedulePlugin()

    plugins += new LearnPlugin()
    if(withRas) assert(withBtb)
    if(withGShare) assert(withBtb)
    if(withBtb) {
      plugins += new prediction.BtbPlugin(
        sets = btbSets / decoders,
        chunks = decoders,
        rasDepth = if(withRas) 4 else 0,
        hashWidth = btbHashWidth,
        readAt = 0,
        hitAt = 1,
        jumpAt = 1+relaxedBtb.toInt,
        bootMemClear = bootMemClear
      )
//      plugins += new prediction.DecodePredictionPlugin(
//        decodeAt = decoderAt,
//        jumpAt = decoderAt
//      )
    }
    if(withGShare) {
      plugins += new prediction.GSharePlugin (
        memBytes = 4 KiB,
        historyWidth = 12,
        readAt = 0,
        bootMemClear = bootMemClear
      )
      plugins += new prediction.HistoryPlugin()
    }
    def shifter(layer: LaneLayer, shiftAt: Int = 0, formatAt: Int = 0) = withIterativeShift match {
      case false => new BarrelShifterPlugin(layer, with_slli_uw=withRvZb, shiftAt=shiftAt, formatAt=formatAt)
      case true => new IterativeShifterPlugin(layer, with_slli_uw=withRvZb, shiftAt=shiftAt, formatAt=formatAt)
    }


    plugins += new fetch.PcPlugin(resetVector)
    plugins += new fetch.FetchPipelinePlugin()
    if(!fetchL1Enable) plugins += new fetch.FetchCachelessPlugin(
      forkAt = fetchForkAt,
      joinAt = fetchForkAt+1, //You can for instance allow the external memory to have more latency by changing this
      wordWidth = fetchMemDataWidth,
      translationStorageParameter = fetchTsp,
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
    if(fetchL1Enable) {
      plugins += new fetch.FetchL1Plugin(
        lineSize = 64,
        setCount = fetchL1Sets,
        wayCount = fetchL1Ways,
        refillCount = fetchL1RefillCount,
        fetchDataWidth = 32*decoders,
        memDataWidth = fetchMemDataWidth,
        reducedBankWidth = fetchL1ReducedBank,
        hitsWithTranslationWays = true,
        tagsReadAsync = false,
        bootMemClear = bootMemClear,
        translationStorageParameter = fetchTsp,
        translationPortParameter = withMmu match {
          case false => null
          case true => fetchTpp
        }
      )

      fetchL1Prefetch match {
        case "none" =>
        case "nl" => {
          plugins += new fetch.PrefetcherNextLinePlugin(64)
          assert(fetchL1RefillCount > 1, "Fetch prefetch require fetchL1RefillCount > 1")
        }
      }
    }

    plugins += new decode.DecodePipelinePlugin()
    plugins += new decode.AlignerPlugin(
      fetchAt = fetchL1Enable.mux(2, 1+fetchForkAt),
      lanes = decoders,
      withBuffer = withAlignerBuffer
    )
    plugins += new decode.DecoderPlugin(
      decodeAt = decoderAt
    )
    plugins += new schedule.DispatchPlugin(
      dispatchAt = dispatcherAt,
      trapLayer = null,
      withBuffer = withDispatcherBuffer
    )

    plugins += new regfile.RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 32,
      preferedWritePortForInit = "lane0",
      syncRead = regFileSync,
      dualPortRam = regFileDualPortRam,
      maskReadDuringWrite = false
    )

    def newExecuteLanePlugin(name : String) = new execute.ExecuteLanePlugin(
      name,
      rfReadAt = 0,
      decodeAt  = 0+regFileSync.toInt,
      executeAt = 0+regFileSync.toInt + 1,
      trapAt    = 0+regFileSync.toInt + 1 + intWritebackAt,
      withBypasses = allowBypassFrom == 0
    )

    plugins += new execute.ExecutePipelinePlugin()

    val lane0 = newExecuteLanePlugin("lane0")
    val early0 = new LaneLayer("early0", lane0, priority = 0)
    plugins += lane0

//    plugins += new RedoPlugin("lane0")
    plugins += new SrcPlugin(early0, executeAt = 0, relaxedRs = relaxedSrc)
    plugins += new IntAluPlugin(early0, formatAt = 0)
    plugins += shifter(early0, formatAt = relaxedShift.toInt)
    plugins += new IntFormatPlugin(lane0)
    plugins += new BranchPlugin(layer=early0, aluAt=0, jumpAt=relaxedBranch.toInt, wbAt=0)
    if(withRvZb) plugins ++= ZbPlugin.make(early0, formatAt=0)
    if(!lsuL1Enable) plugins += new LsuCachelessPlugin(
      layer     = early0,
      withAmo   = withRva,
      withSpeculativeLoadFlush = true,
      addressAt = 0,
      pmaAt     = lsuPmaAt,
      forkAt    = lsuForkAt+0,
      joinAt    = lsuForkAt+1,
      wbAt      = 2, //TODO
      translationStorageParameter = lsuTsp,
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
    if(lsuL1Enable){
      plugins += new LsuPlugin(
        layer = early0,
        withRva = withRva,
        storeRs2At = storeRs2Late.mux(2, 0),
        storeBufferSlots = lsuStoreBufferSlots,
        storeBufferOps = lsuStoreBufferOps,
        softwarePrefetch = lsuSoftwarePrefetch,
        translationStorageParameter = lsuTsp,
        translationPortParameter = withMmu match {
          case false => null
          case true => lsuTpp
        }
      )
      plugins += new LsuL1Plugin(
        lane           = lane0,
        memDataWidth   = lsuMemDataWidth,
        cpuDataWidth   = xlen,
        refillCount    = lsuL1RefillCount,
        writebackCount = lsuL1WritebackCount,
        setCount       = lsuL1Sets,
        wayCount       = lsuL1Ways,
        withBypass     = withLsuBypass,
        withCoherency  = lsuL1Coherency,
        bootMemClear = bootMemClear
      )

      lsuHardwarePrefetch match {
        case "none" =>
        case "nl" => plugins += new lsu.PrefetchNextLinePlugin
        case "rpt" => plugins += new lsu.PrefetchRptPlugin(
          sets = 128,
          bootMemClear = bootMemClear
        )
      }
    }

    if(withMul) {
      plugins += new MulPlugin(early0, keepMulSrc = mulKeepSrc)
    }
    if(withDiv) {
      plugins += new RsUnsignedPlugin("lane0")
      plugins += new DivPlugin(
        layer = early0,
        relaxedInputs = xlen == 64 || relaxedDiv,
        radix = divRadix,
        area  = divArea,
        impl = {
          def pasta(width: Int, radix: Int, area : Boolean) = new DivRadix2(width, lowArea = area)
          def vexii(width: Int, radix: Int, area: Boolean) = new DivRadix(width, radix)
          def default(width: Int, radix: Int, area: Boolean) = vexii(width, radix, area)
          divImpl match {
            case "" => default
            case "bitpasta" => pasta
            case "vexii" => vexii
          }
        }
      )
    }

    plugins += new CsrRamPlugin()
    if(withPerformanceCounters) plugins += new PerformanceCounterPlugin(additionalCounterCount = additionalPerformanceCounters)
    plugins += new CsrAccessPlugin(early0, writeBackKey =  if(lanes == 1) "lane0" else "lane1")
    plugins += new PrivilegedPlugin(privParam, hartId until hartId+hartCount)
    plugins += new TrapPlugin(trapAt = intWritebackAt)
    plugins += new EnvPlugin(early0, executeAt = 0)
    if(embeddedJtagTap || embeddedJtagInstruction) plugins += new EmbeddedRiscvJtag(
      p = DebugTransportModuleParameter(
        addressWidth = 7,
        version = 1,
        idle = 7
      ),
      withTunneling = false,
      withTap = embeddedJtagTap,
      debugCd = embeddedJtagCd,
      noTapCd = embeddedJtagNoTapCd
    )
    val lateAluAt = intWritebackAt
    
    if(withLateAlu) {
      val late0 = new LaneLayer("late0", lane0, priority = -5)
      plugins += new SrcPlugin(late0, executeAt = lateAluAt, relaxedRs = relaxedSrc)
      plugins += new IntAluPlugin(late0, aluAt = lateAluAt, formatAt = lateAluAt)
      plugins += shifter(late0, shiftAt = lateAluAt, formatAt = lateAluAt)
      plugins += new BranchPlugin(late0, aluAt = lateAluAt, jumpAt = lateAluAt/*+relaxedBranch.toInt*/, wbAt = lateAluAt, withJalr = false)
      if(withRvZb) plugins ++= ZbPlugin.make(late0, executeAt = lateAluAt, formatAt = lateAluAt)
    }

    plugins += new WriteBackPlugin(lane0, IntRegFile, writeAt = withLateAlu.mux(lateAluAt, intWritebackAt), allowBypassFrom = allowBypassFrom)

    if(lanes >= 2) {
      val lane1 = newExecuteLanePlugin("lane1")
      val early1 = new LaneLayer("early1", lane1, priority = 10)
      plugins += lane1

      plugins += new SrcPlugin(early1, executeAt = 0, relaxedRs = relaxedSrc)
      plugins += new IntAluPlugin(early1, formatAt = 0)
      plugins += shifter(early1, formatAt = relaxedShift.toInt)
      plugins += new IntFormatPlugin(lane1)
      plugins += new BranchPlugin(early1, aluAt = 0, jumpAt = relaxedBranch.toInt, wbAt = 0)
      if(withRvZb) plugins ++= ZbPlugin.make(early1, formatAt=0)

      if(withLateAlu) {
        val late1 = new LaneLayer("late1", lane1, priority = -3)
        plugins += new SrcPlugin(late1, executeAt = lateAluAt, relaxedRs = relaxedSrc)
        plugins += new IntAluPlugin(late1, aluAt = lateAluAt, formatAt = lateAluAt)
        plugins += shifter(late1, shiftAt = lateAluAt, formatAt = lateAluAt)
        plugins += new BranchPlugin(late1, aluAt = lateAluAt, jumpAt = lateAluAt/*+relaxedBranch.toInt*/, wbAt = lateAluAt, withJalr = false)
        if(withRvZb) plugins ++= ZbPlugin.make(late1, executeAt = lateAluAt, formatAt = lateAluAt)
      }
//      if (withMul) {
//        plugins += new MulPlugin(early1)
//      }
      plugins += new WriteBackPlugin(lane1, IntRegFile, writeAt = withLateAlu.mux(lateAluAt, intWritebackAt), allowBypassFrom = allowBypassFrom)
    }

    plugins.foreach {
      case p: DispatchPlugin => p.trapLayer = early0
      case _ =>
    }

    if (withRvf || withRvd) {
      plugins += new regfile.RegFilePlugin(
        spec = riscv.FloatRegFile,
        physicalDepth = 32,
        preferedWritePortForInit = "lane0",
        syncRead = regFileSync,
        dualPortRam = regFileDualPortRam,
        maskReadDuringWrite = false
      )

//      plugins += new execute.fpu.FpuExecute(early0, 0)
      plugins += new WriteBackPlugin(lane0, FloatRegFile, writeAt = 9, allowBypassFrom = allowBypassFrom.max(2)) //Max 2 to save area on not so important instructions
      plugins += new execute.fpu.FpuFlagsWritebackPlugin(lane0, pipTo = intWritebackAt)
      plugins += new execute.fpu.FpuCsrPlugin(List(lane0), intWritebackAt)
      plugins += new execute.fpu.FpuUnpackerPlugin(early0, ignoreSubnormal = fpuIgnoreSubnormal)
      plugins += new execute.fpu.FpuAddSharedPlugin(lane0)
      plugins += new execute.fpu.FpuAddPlugin(early0)
      plugins += new execute.fpu.FpuMulPlugin(early0, withFma = !skipFma, fmaFullAccuracy = fpuFmaFullAccuracy)
      plugins += new execute.fpu.FpuSqrtPlugin(early0)
      plugins += new execute.fpu.FpuClassPlugin(early0)
      plugins += new execute.fpu.FpuCmpPlugin(early0)
      plugins += new execute.fpu.FpuF2iPlugin(early0)
      plugins += new execute.fpu.FpuMvPlugin(early0, floatWbAt = 2)
      if(withRvd) plugins += new execute.fpu.FpuXxPlugin(early0)
      plugins += new execute.fpu.FpuDivPlugin(early0)
      plugins += new execute.fpu.FpuPackerPlugin(lane0, ignoreSubnormal = fpuIgnoreSubnormal)
      //      plugins += new execute.fpu.FpuEmbedded()
    }

    plugins += new WhiteboxerPlugin()
  }
}

/*
jump at 0 :       dhrystone coremark   embench
1l btb gshare ras => 1.64     3.26      1.04
       + late alu => 1.72     3.54      1.10
2l btb gshare ras => 1.92     3.93      1.34
       + late alu => 2.09     4.39
    + 4b late alu => 2.24     4.55      1.47
    + aligner buf => 2.29     4.70      1.50
    + ali/dis buf => 2.46     4.80      1.59

cached
0.98 1l la rbtb Geometric mean
1.00 1l Geometric mean
1.07 1l la Geometric mean
1.39 2l Geometric mean
1.55 2l la Geometric mean



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



object OptionToPython extends App{
  new scopt.OptionParser[Unit]("lol"){
    new ParamSimple().addOptions(this)
    for(o <- options){
      println(o.name)
    }
  }
}

