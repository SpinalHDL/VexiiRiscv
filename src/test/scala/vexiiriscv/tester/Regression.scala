package vexiiriscv.tester

import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.plugin.Hostable
import spinal.lib.misc.test.{AsyncJob, MultithreadedFunSuite}
import vexiiriscv.memory.MmuPlugin
import vexiiriscv.misc.{EmbeddedRiscvJtag, PrivilegedPlugin}
import vexiiriscv.riscv.Riscv
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.reflect.io.Path.jfile2path
import scala.util.Random


class RegressionSingleConfig(){
  var riscvTest = true
  var riscvArchTest = true
  var buildroot = true
  var freertosCount = 1
  var regular = true
  var benchmark = true
  var jtag = true

  def fromEnv(): this.type = {
    freertosCount = sys.env.getOrElse("VEXIIRISCV_REGRESSION_FREERTOS_COUNT", "1").toInt
    buildroot = sys.env.getOrElse("VEXIIRISCV_REGRESSION_BUILDROOT_ENABLED", "1").toInt.toBoolean
    this
  }

  def disableAll(): Unit = {
    riscvTest = false
    riscvArchTest = false
    buildroot = false
    freertosCount = 0
    regular = false
    benchmark = false
    jtag = false
  }
}

class RegressionSingle(compiled : SimCompiled[VexiiRiscv],
                       dutArgs : Seq[String] = Nil,
                       config : RegressionSingleConfig) {
  val dut = compiled.dut
  val xlen = dut.database(Riscv.XLEN)
  val priv = dut.host.get[PrivilegedPlugin]
  val mmu = dut.host.get[MmuPlugin]

  val rvm = dut.database(Riscv.RVM)
  val rvc = dut.database(Riscv.RVC)
  val rvf = dut.database(Riscv.RVF)
  val rvd = dut.database(Riscv.RVD)
  val rva = dut.database(Riscv.RVA)
  val rvzba = dut.database(Riscv.RVZba)
  val rvzbb = dut.database(Riscv.RVZbb)
  val rvzbc = dut.database(Riscv.RVZbc)
  val rvzbs = dut.database(Riscv.RVZbs)

  var arch = ""
  var archLinux = ""

  if (xlen == 64) {
    arch = "rv64i"
    archLinux = "rv64i"
  } else {
    arch = "rv32i"
    archLinux = "rv32i"
  }
  if (rvm) {
    arch += "m"
    archLinux += "m"
    arch += "a"
    archLinux += "a"
  }
  if (rvf) {
    arch += "f"
    archLinux += "f"
  }
  if (rvd) {
    arch += "d"
    archLinux += "d"
  }
  if(rvc) {
    arch += "c"
    archLinux += "c"
  }
  /*
  if (rvzba) {
    arch += "Zba"
    archLinux += "Zba"
  }
  if (rvzbb) {
    arch += "Zbb"
    archLinux += "Zbb"
  }
  if (rvzbc) {
    arch += "Zbc"
    archLinux += "Zbc"
  }
  if (rvzbs) {
    arch += "Zbs"
    archLinux += "Zbs"
  }*/
  //arch += "Zicsr"


  if(List("im", "imc").exists(arch.endsWith)){
    arch = arch.replace("im", "ima")
  }


  def newTest() = {
    val t = new TestOptions()
    tests += t
    t
  }

  def newArgs() = {
    val t = new TestArgs()
    testArgs += t
    t.noStdin()
    t.ibusReadyFactor(0.5)
    t.dbusReadyFactor(0.5)
    t
  }
  //
//
//  def newTest(args : Seq[String]) = {
//    val t = newTest()
//    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
//      help("help").text("prints this usage text")
//      t.addOptions(this)
//    }.parse(args, Unit).nonEmpty)
//    t
//  }

  val tests = ArrayBuffer[TestOptions]()
  val testArgs = ArrayBuffer[TestArgs]()

  val nsf = new File("ext/NaxSoftware")

  val rejectedTests = mutable.LinkedHashSet("rv32ui-p-simple", "rv32ui-p-fence_i", "rv64ui-p-simple", "rv64ui-p-fence_i", "rv32ua-p-lrsc", "rv64ua-p-lrsc")

  //rvi tests
  val riscvTestsFile = new File(nsf, "riscv-tests")
  val riscvTests = riscvTestsFile.listFiles().sorted
  val rvti = riscvTests.filter{ t => val n = t.getName; n.startsWith(s"rv${xlen}ui-p-") && !n.contains(".") && !rejectedTests.contains(n) }
  val rvtm = riscvTests.filter { t => val n = t.getName; n.startsWith(s"rv${xlen}um-p-") && !n.contains(".") && !rejectedTests.contains(n)  }
  val rvta = riscvTests.filter { t => val n = t.getName; n.startsWith(s"rv${xlen}ua-p-") && !n.contains(".") && !rejectedTests.contains(n) }
  val rvtf = riscvTests.filter { t => val n = t.getName; n.startsWith(s"rv${xlen}uf-p-") && !n.contains(".") && !rejectedTests.contains(n) }
  val rvtd = riscvTests.filter { t => val n = t.getName; n.startsWith(s"rv${xlen}ud-p-") && !n.contains(".") && !rejectedTests.contains(n) }

  val riscvTestsFrom2, riscvTestsFromStart = ArrayBuffer[File]()

  if(config.riscvTest) {
    riscvTestsFrom2 ++= rvti
    if (rvm) riscvTestsFrom2 ++= rvtm
    if (rva) riscvTestsFrom2 ++= rvta
    if (rvf) riscvTestsFromStart ++= rvtf
    if (rvd) riscvTestsFromStart ++= rvtd
  }



  val fpuTestFactor = 0.01 //TODO

  import scala.collection.mutable.ArrayBuffer

  val fpuTestRvf32 = ArrayBuffer(
    (0, "fmv.x.w", "f32"),
    (31, "fmv.s.x", "f32"),
    (101, "fadd.s", "f32"),
    (102, "fsub.s", "f32"),
    (103, "fmul.s", "f32"),
    (104, "fdiv.s", "f32"),
    (105, "fsqrt.s", "f32"),
    (106, "fmadd.s", "f32"),
    (107, "fmsub.s", "f32"),
    (108, "fnmadd.s", "f32"),
    (109, "fnmsub.s", "f32"),
    (110, "fsgnj.s", "f32"),
    (111, "fsgnjn.s", "f32"),
    (112, "fsgnjx.s", "f32"),
    (113, "fmin.s", "f32"),
    (114, "fmax.s", "f32"),
    (115, "fle.s", "f32"),
    (116, "feq.s", "f32"),
    (117, "flt.s", "f32"),
    (118, "fclass.s", "f32"),
    (119, "fcvt.s.wu", "ui32"),
    (120, "fcvt.s.w", "i32"),
    (121, "fcvt.wu.s", "f32"),
    (122, "fcvt.w.s", "f32")
  )

  val fpuTestRvf64 = ArrayBuffer(
    (127, "fcvt.s.lu", "ui64"),
    (128, "fcvt.s.l", "i64"),
    (129, "fcvt.lu.s", "f32"),
    (130, "fcvt.l.s", "f32"),
    (31, "fmv.s.x_64", "f64"),
    (202, "fcvt.s.wu_64", "ui64"),
    (203, "fcvt.s.w_64", "i64")
  )

  val fpuTestRvd32 =  ArrayBuffer(
    (1, "fadd.d", "f64"),
    (2, "fsub.d", "f64"),
    (3, "fmul.d", "f64"),
    (4, "fdiv.d", "f64"),
    (5, "fsqrt.d", "f64"),
    (6, "fmadd.d", "f64"),
    (7, "fmsub.d", "f64"),
    (8, "fnmadd.d", "f64"),
    (9, "fnmsub.d", "f64"),
    (10, "fsgnj.d", "f64"),
    (11, "fsgnjn.d", "f64"),
    (12, "fsgnjx.d", "f64"),
    (13, "fmin.d", "f64"),
    (14, "fmax.d", "f64"),
    (15, "fle.d", "f64"),
    (16, "feq.d", "f64"),
    (17, "flt.d", "f64"),
    (18, "fclass.d", "f64"),
    (19, "fcvt.d.wu", "ui32"),
    (20, "fcvt.d.w", "i32"),
    (21, "fcvt.wu.d", "f64"),
    (22, "fcvt.w.d", "f64"),
    (23, "fcvt.d.s", "f64"),
    (24, "fcvt.s.d", "f64")
  )

  val fpuTestRvd64 = ArrayBuffer(
    (25, "fmv.x.d", "f64"),
    (26, "fmv.d.x", "ui64"),
    (27, "fcvt.d.lu", "ui64"),
    (28, "fcvt.d.l", "i64"),
    (29, "fcvt.lu.d", "f64"),
    (30, "fcvt.l.d", "f64"),
    (200, "fcvt.d.wu_64", "ui64"),
    (201, "fcvt.d.w_64", "i64")
  )

  def fpuTest(name : String, vector : String, testId : Int) = {
    val args = newArgs()
    args.name(s"fpu/${name}_${vector}_${testId}")
    args.loadBin(0x90000000l, s"${nsf.getAbsolutePath}/baremetal/fpu_test/vector/${vector}.bin")
    args.loadElf(s"${nsf.getAbsolutePath}/baremetal/fpu_test/build/${archLinux}/fpu_test.elf")
    val testCount = (0x50000 * fpuTestFactor).toInt
    args.loadU32(0xA0000000l, testCount)
    args.loadU32(0xA0000004l, testId)
    args.failAfter(500000000l*fpuTestFactor*(List("sqrt", "div").exists(name.contains).mux(30,1)) toLong)
  }
  def fpuTest3() = {
    val args = newArgs()
    args.name(s"fpu/fpu_test3")
    args.loadBin(0x90000000l, s"${nsf.getAbsolutePath}/baremetal/fpu_test/vector/f32.bin")
    args.loadElf(s"${nsf.getAbsolutePath}/baremetal/fpu_test3/build/${archLinux}/fpu_test3.elf")
    args.failAfter(500000000)
  }

  for(elf <- riscvTestsFrom2) {
    val args = newArgs()
    args.loadElf(elf)
    args.failAfter(100000)
    args.startSymbol("test_2")
    args.name("riscv-tests/" + elf.getName)
  }

  for (elf <- riscvTestsFromStart) {
    val args = newArgs()
    args.loadElf(elf)
    args.failAfter(1000000)
    args.name("riscv-tests/" + elf.getName)
  }

  if(config.riscvTest) {
    if (rva) {
      val args = newArgs()
      args.loadElf(new File(nsf, s"riscv-tests/rv${xlen}ua-p-lrsc"))
      args.failAfter(1000000)
      args.startSymbol("test_2")
      args.passSymbol("test_5")
      args.name(s"riscv-tests/rv${xlen}ua-p-lrsc")
    }
  }



  def doArchTest(from: String, inName: Seq[String] = Seq()) = {
    val folder = s"riscv-arch-test/rv${xlen}i_m/$from"
    val elfs = new File(nsf, folder)
      .listFiles()
      .filter(_.getName.endsWith(".elf"))
      .filter(file => inName.isEmpty || inName.exists(needle => file.getName.contains(needle)))
    for (elf <- elfs) {
      val args = newArgs()
      args.loadElf(elf)
      args.failAfter(10000000)
      args.name(folder + "/" + elf.getName.replace(".elf", ""))
    }
  }

  if(config.riscvArchTest) {
    doArchTest("I")
    doArchTest("Zifencei")
    doArchTest("privilege")
    if (rvm) doArchTest("M")
    if (rvc) doArchTest("C")
    if (rvzba) doArchTest("B", Seq("add", "slli"))
    if (rvzbb) doArchTest("B", Seq("and", "clz", "cpop", "ctz", "max", "min", "or", "rev", "rol", "ror", "sext", "xnor", "zext"))
    if (rvzbc) doArchTest("B", Seq("mul"))
    if (rvzbs) doArchTest("B", Seq("bclr", "bext", "binv", "bset"))
  }

  val regulars = ArrayBuffer("dhrystone_vexii", "coremark_vexii", "machine_vexii")

  if (rvf) {
    regulars += "fpu_test2"
    fpuTest3()
    for (e <- fpuTestRvf32) {
      fpuTest(e._2, e._3, e._1)
    }
    if (xlen == 64) {
      for (e <- fpuTestRvf64) {
        fpuTest(e._2, e._3, e._1)
      }
    }
  }

  if (rvd) {
    for (e <- fpuTestRvd32) {
      fpuTest(e._2, e._3, e._1)
    }
    if (xlen == 64) {
      for (e <- fpuTestRvd64) {
        fpuTest(e._2, e._3, e._1)
      }
    }
  }




  priv.filter(_.p.withSupervisor).foreach(_ => regulars ++= List("supervisor"))
  if(mmu.nonEmpty) regulars ++= List(s"mmu_sv${if(xlen == 32) 32 else 39}")

  if(config.regular) for(name <- regulars){
    val args = newArgs()
    args.loadElf(new File(nsf, s"baremetal/$name/build/$arch/$name.elf"))
    args.failAfter(300000000)
    args.name(s"regular/$name")
  }

  val benchmarks = ArrayBuffer("dhrystone_vexii", "coremark_vexii")
  if(config.benchmark) for (name <- benchmarks) {
    val args = newArgs()
    args.loadElf(new File(nsf, s"baremetal/$name/build/$arch/$name.elf"))
    args.failAfter(300000000)
    args.ibusReadyFactor(2.0)
    args.dbusReadyFactor(2.0)
    args.name(s"benchmark/$name")
  }


  val freertos = List(
    "integer", "countsem", "EventGroupsDemo", "flop", "QPeek",
    "QueueSet", "recmutex", "semtest", "TaskNotify", "dynamic",
    "GenQTest", "PollQ", "QueueOverwrite", "QueueSetPolling", "test1"
  )
  if(rvm) for(name <- freertos.take(config.freertosCount)){
    val args = newArgs()
    var freertosArch = arch
    if(xlen == 32 && rvf && !rvd) freertosArch = "rv32ima"
    if(xlen == 64 && rvf && !rvd) freertosArch = "rv64ima"
    args.loadElf(new File(nsf,  f"baremetal/freertosDemo/build/${name}/${freertosArch + (freertosArch.endsWith("im").mux("a",""))}/freertosDemo.elf"))
    args.failAfter(300000000)
    args.name(s"freertos/$name")
  }

  if(config.buildroot && rvm && rva && mmu.nonEmpty) priv.filter(_.p.withSupervisor).foreach{ _ =>
    var arch = s"rv${xlen}ima"
    xlen match{
      case 32 => if(rvc) arch += "c"
      case 64 => {
        if(rvf && rvd && rvc) arch += "fdc"
        else if (rvc) arch += "c"
      }
    }
    val path = s"ext/NaxSoftware/buildroot/images/$arch"
    val args = newArgs()
    args.failAfter(10000000000l)
    args.name("buildroot")
    args.loadBin(0x80000000l, s"$path/fw_jump.bin")
    args.loadBin(0x80F80000l, s"$path/linux.dtb")
    args.loadBin(0x80400000l, s"$path/Image")
    args.loadBin(0x81000000l, s"$path/rootfs.cpio")

    args.fsmGetc("buildroot login:")
    args.fsmSleep(100000*10)
    args.fsmPutc("root"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmPutc("cat /proc/cpuinfo"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmPutc("echo 1+2+3*4 | bc"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmPutc("micropython"); args.fsmPutcLr()
    args.fsmGetc(">>> ")
    args.fsmPutc("import math"); args.fsmPutcLr()
    args.fsmGetc(">>> ")
    args.fsmPutc("math.sin(math.pi/4)"); args.fsmPutcLr()
    args.fsmGetc(">>> ")
    args.fsmPutc("from sys import exit"); args.fsmPutcLr()
    args.fsmGetc(">>> ")
    args.fsmPutc("exit()"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmPutc("ls /"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmSuccess()
  }

  if(config.jtag){
    dut.host.get[EmbeddedRiscvJtag].foreach { p =>
      val args = newArgs()
      args.loadElf(new File(nsf, s"baremetal/debugger/build/$arch/debugger.elf"))
      args.failAfter(100000000)
      args.name(s"regular/debugger")
      args.args += "--jtag-remote"
      args.noRvlsCheck()
      args.noProbe()
      args.args ++= List("--spawn-process", s"openocd -f src/main/tcl/openocd/vexiiriscv_sim.tcl -f ${new File(nsf, s"baremetal/debugger/tcl/test.tcl")}")
    }
  }


  implicit val ec = ExecutionContext.global
  val jobs = ArrayBuffer[AsyncJob]()

  val tp = new File(compiled.simConfig.getTestPath(""))
  FileUtils.forceMkdir(tp)
  val argsFile = new BufferedWriter(new FileWriter(new File(tp, "args")))
  argsFile.write(dutArgs.map(v => if (v.contains(" ")) '"' + v + '"' else v).mkString(" "))
  argsFile.close()

  for(args <- testArgs){
    val t = newTest()
    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      t.addOptions(this)
    }.parse(args.args, Unit).nonEmpty)
    
    val testPath = new File(compiled.simConfig.getTestPath(t.testName.get))
    val passFile = new File(testPath, "PASS")
    val failFile = new File(testPath, "FAIL")
//    FileUtils.deleteQuietly(passFile)
    FileUtils.deleteQuietly(failFile)

    val testName = t.testName.get
    if(!passFile.exists()){
      val stdoutHost = Console.out
      val job = new AsyncJob(toStdout = false, logsPath = testPath)({
        FileUtils.forceMkdir(testPath)
        val argsFile = new BufferedWriter(new FileWriter(new File(testPath, "args")))
        argsFile.write(args.args.map(v => if(v.contains(" ")) '"' + v + '"' else v).mkString(" "))
        argsFile.close()

        t.test(compiled)
        val bf = new BufferedWriter(new FileWriter(passFile))
        bf.flush()
        bf.close()
        stdoutHost.println(s"PASS $testName")
      }){
        override def onFail() = {
          val bf = new BufferedWriter(new FileWriter(failFile))
          bf.flush()
          bf.close()
          stdoutHost.println(s"FAIL $testName")
        }
      }
      jobs += job
    }
  }
  jobs.foreach(_.join())
}

object RegressionSingle extends App{
  def test(name : String, plugins : => Seq[Hostable], dutArgs : Seq[String], config : RegressionSingleConfig): Unit = {
    val simConfig = SpinalSimConfig()
//    simConfig.withIVerilog
    simConfig.withFstWave
    simConfig.setTestPath("regression/$COMPILED_tests/$TEST")
    val compiled = SpinalConfig.synchronized(simConfig.compile(VexiiRiscv(plugins).setDefinitionName(s"VexiiRiscv_$name")))
    val regression = new RegressionSingle(compiled, dutArgs, config)
    println("*" * 80)
    val fails = regression.jobs.filter(_.failed)
    if (fails.isEmpty) {
      println("PASS")
      return
    }
    println(s"FAILED ${fails.size}/${regression.jobs.size}")
    for (fail <- fails) {
      println("- " + fail.logsFile.getAbsolutePath)
    }
    Thread.sleep(10)
    throw new Exception()
  }

  def test(ps : ParamSimple, dutArgs : Seq[String], config : RegressionSingleConfig): Unit = {
    test(ps.hashCode().toString, TestBench.paramToPlugins(ps), dutArgs, config)
  }

  def test(args : String) : Unit = test(args.split(" "))
  def test(args : Seq[String]): Unit = {
    val param = new ParamSimple()
    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      param.addOptions(this)
    }.parse(args, Unit).nonEmpty)
    test(param, args, new RegressionSingleConfig().fromEnv())
  }

  try {
    test(args)
  }catch {
    case e : Throwable => System.exit(1)
  }
}


class Regression extends MultithreadedFunSuite(sys.env.getOrElse("VEXIIRISCV_REGRESSION_THREAD_COUNT", "0").toInt){
//  FileUtils.deleteQuietly(new File("regression"))

  val testsAdded = mutable.LinkedHashSet[String]()
  def addTest(param : ParamSimple, args: String): Unit = addTest(param, args.replace("  ", " ").split("\\s+"))
  def addTest(param : ParamSimple, args: Seq[String]): Unit = {
    val paramName = param.getName()
    if(testsAdded.contains(paramName)) return
    testsAdded += paramName
    testMp(paramName) {
      RegressionSingle.test(param, args, new RegressionSingleConfig().fromEnv())
    }
  }


  abstract class Dimensions[T](val name : String){
    def getRandomPosition(state : T, random : Random) : String
  }

  val dimensions = ArrayBuffer[Dimensions[ParamSimple]]()

  def Dim(name : String, poses : Seq[String]) = new Dimensions[ParamSimple](name) {
    override def getRandomPosition(state : ParamSimple, random: Random): String = poses.randomPick(random)
  }
  def addDim(name : String, poses : Seq[String]) = dimensions += Dim(name, poses)

  def addDims(name: String)(poses: Dimensions[ParamSimple]*) = dimensions += new Dimensions[ParamSimple](name) {
    override def getRandomPosition(state : ParamSimple, random: Random): String = poses.randomPick(random).getRandomPosition(state, random)
  }



  addDim("default", List("--with-mul --with-div --performance-counters 4"))
  addDim("lanes", List(1, 2).map(v => s"--lanes $v --decoders $v"))
  addDim("rf", List("--regfile-sync", "--regfile-async"))
  addDim("rfPorts", List("--regfile-infer-ports", "--regfile-dual-ports"))
  addDim("bypass", List(0,0,0,1,2,3,100).map(v => s"--allow-bypass-from $v")) //More weight to fully bypassed configs
  addDim("xlen", List(32, 64).map(v => s"--xlen $v"))
  addDim("prediction", List("", "--with-btb", "--with-btb --with-ras", "--with-btb --with-ras --with-gshare"))
//  addDim("prediction-relaxed", List("", "--relaxed-btb")) //incompatible with single stage fetch pipe

  addDim("priv", List("", "--with-supervisor", "--with-user"))
  addDim("rvm", List("--without-mul --without-div", "--with-mul --with-div"))
  addDim("divParam", List(2, 4).flatMap(radix => List("", "--div-ipc").map(opt => s"$opt --div-radix $radix")))
  addDim("rva", List("", "--with-mul --with-div --with-rva"))
  addDim("rvc", List("", "--with-mul --with-div --with-rvc"))
  addDim("rvzb", List("", "--with-rvZb"))
  addDim("late-alu", List("", "--with-late-alu"))
  addDims("fetch")(
    Dim("", List("--fetch-fork-at 0", "--fetch-fork-at 1")),
    Dim("", for (bytes <- List(1 << 10, 1 << 12, 1 << 14);
                 sets <- List(16, 32, 64);
                 prefetch <- List(true, false);
                 if (bytes / sets >= 64)) yield {
        val ways = bytes / sets / 64
        s"--with-fetch-l1 --fetch-l1-sets=$sets --fetch-l1-ways=$ways ${prefetch.mux(s"--fetch-l1-hardware-prefetch=nl --fetch-l1-refill-count=2","")}"
      }
    )
  )

  addDim("debugger", List("", "--debug-privileged --debug-triggers 4 --debug-triggers-lsu --debug-jtag-tap"))



//  addDim("fl1dwm", List(32, 64, 128, 256).map(w => s"--fetch-l1-mem-data-width-min $w")) //TODO but conflict with aligner plugin without buffer
  addDim("fl1rw", List("", "--fetch-reduced-bank"))
  addDims("lsu")(
    Dim("", List("--lsu-fork-at 0", "--lsu-fork-at 1")),
    Dim("", for (bytes <- List(1 << 10, 1 << 12, 1 << 14);
                 sets <- List(16, 32, 64);
                 if (bytes / sets >= 64);
                 ways = bytes / sets / 64;
                 slots <- List(0,1,2,3);
                 prefetch <- List(true, false);
                 ops <- List(8, 16, 32, 64);
                 if !(slots != 0 ^ ops != 0)) yield {
        s"--with-lsu-l1 --lsu-l1-sets=$sets --lsu-l1-ways=$ways --lsu-l1-store-buffer-slots=$slots --lsu-l1-store-buffer-ops=$ops ${prefetch.mux(s"--lsu-software-prefetch --lsu-hardware-prefetch rpt","")}"
      }
    )
  )

  addDim("coherency", List("", "--with-fetch-l1 --lsu-l1-coherency")) //Want the fetch l1, else it slow down the sim too much
  addDim("lsu bypass", List("", "--with-lsu-bypass"))
  addDim("ishift", List("", "--with-iterative-shift"))
  addDim("alignBuf", List("", "--with-aligner-buffer"))
  addDim("dispBuf", List("", "--with-dispatcher-buffer"))
  addDim("btbParam", List("--btb-sets 512 --btb-hash-width 16", "--btb-sets 128 --btb-hash-width 6"))
  dimensions += new Dimensions[ParamSimple]("fpu") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(!state.lsuL1Enable) return "" //Don't support the FPU yet TODO
      return List("", "--with-rvf", "--with-rvf --with-rvd").randomPick(random)
    }
  }


//  addTest(default)
  // Add a simple test for each dimensions's positions
//  for(dim <- dimensions){
//    for(pos <- dim.getPositions() if pos != "") {
//      addTest(default + " " + pos)
//    }
//  }

  // Generate random parameters
  val random = new Random(42)
  for(i <- 0 until 50){
    val args = ArrayBuffer[String]()
    val p = new ParamSimple()
    val parser = new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      p.addOptions(this)
    }
    for (dim <- dimensions) {
      val arg = dim.getRandomPosition(p, random)
      parser.parse(arg.replace("  ", " ").split("\\s+").filter(_.nonEmpty), Unit) match {
        case Some(_) =>
        case None => throw new Exception("invalid regression test parameters")
      }
      args += arg
    }
    addTest(p, args.mkString(" "))
  }
}

//cd $PWD && find . -name FAIL && find . -name PASS | wc -l && find . -name FAIL | wc -l
//cd $PWD && find . -type d -exec sh -c 'test -f "$0/stdout.log" && ! test -f "$0/PASS"' {} \; -print