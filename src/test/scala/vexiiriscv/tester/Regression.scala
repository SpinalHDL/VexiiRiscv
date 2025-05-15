package vexiiriscv.tester

import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.plugin.Hostable
import spinal.lib.misc.test.{AsyncJob, MultithreadedFunSuite}
import vexiiriscv.memory.{MmuPlugin, PmpPlugin}
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

/**
 * Scala test which will generate a bunch of randomized VexiiRiscv configuration, and run the RegressionSingle on each of them
 * This is the way by which the VexiiRiscv changes are tested on a large scale.
 */
class Regression extends MultithreadedFunSuite(sys.env.getOrElse("VEXIIRISCV_REGRESSION_THREAD_COUNT", "0").toInt){
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

  // A Dimensions model a given axes of VexiiRiscv parametrization. For instance an axe could be "Branch prediction architecture"
  // One one thing that a dimension ned to be able to do, is to provide a set of VexiiRiscv ParamSimple command line arguments
  abstract class Dimensions[T](val name : String){
    def getRandomPosition(state : T, random : Random) : String
  }

  // Let's define some utilities to collect all the different VexiiRiscv parameters Dimensions
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
  addDim("btbSp", List("", "--btb-single-port-ram"))
  addDim("relaxedBranch", List("", "--relaxed-branch"))
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
  addDim("fetchL1AsyncTag", List("", "--fetch-l1-tags-read-async"))

  addDim("debugger", List("", "--debug-privileged --debug-triggers 4 --debug-triggers-lsu --debug-jtag-tap"))
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

  addDim("lsuL1AsyncTag", List("", "--lsu-l1-tags-read-async"))
  addDim("coherency", List("", "--with-fetch-l1 --lsu-l1-coherency")) //Want the fetch l1, else it slow down the sim too much
  addDim("lsu bypass", List("", "--with-lsu-bypass"))
  addDim("ishift", List("", "--with-iterative-shift"))
  addDim("alignBuf", List("", "--with-aligner-buffer"))
  addDim("dispBuf", List("", "--with-dispatcher-buffer"))
  addDim("btbParam", List("--btb-sets 512 --btb-hash-width 16", "--btb-sets 128 --btb-hash-width 6"))
  dimensions += new Dimensions[ParamSimple]("fpu") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(!state.withMul || state.withDiv) return ""
      return List("", "--with-rvf", "--with-rvf --with-rvd").randomPick(random)
    }
  }
  addDim("fpuStressed", List("", "--stressed-fpu"))
  addDim("pmp", List("", "--pmp-size=8"))
  dimensions += new Dimensions[ParamSimple]("btbRelaxed") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(state.alignerPluginFetchAt < 2) return ""
      return List("", "--relaxed-btb").randomPick(random)
    }
  }
  dimensions += new Dimensions[ParamSimple]("physicalWidth") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(state.xlen != 64) return ""
      return s"--physical-width=${List(36,37,38).randomPick(random)}"
    }
  }
  dimensions += new Dimensions[ParamSimple]("fl1dwm") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(!state.withAlignerBuffer) return ""
      return  List(32, 64, 128, 256).map(w => s"--fetch-l1-mem-data-width-min $w").randomPick(random)
    }
  }

  dimensions += new Dimensions[ParamSimple]("fetchBus") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(state.lsuL1Coherency) return "" //As the testbench doesn't implement probe generation from AXI4/Wishbone
      List("", "--fetch-axi4", "--fetch-wishbone").randomPick(random)
    }
  }

  dimensions += new Dimensions[ParamSimple]("lsuBus") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(!state.lsuL1Enable && state.withRva) return ""
      List("", "--lsu-axi4", "--lsu-wishbone").randomPick(random)
    }
  }

  dimensions += new Dimensions[ParamSimple]("lsuL1Bus") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(!state.lsuL1Enable || state.lsuL1Coherency) return ""
      List("", "--lsu-l1-axi4", "--lsu-l1-wishbone").randomPick(random)
    }
  }

  dimensions += new Dimensions[ParamSimple]("cbm") {
    override def getRandomPosition(state : ParamSimple, random: Random): String = {
      if(!state.lsuL1Enable || state.lsuL1Coherency) return ""
      List("", "--with-rvZcbm").randomPick(random)
    }
  }


  // Generate a bunch of random VexiiRiscv configuration and run the tests on them.
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
      parser.parse(arg.replace("  ", " ").split("\\s+").filter(_.nonEmpty), ()) match {
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