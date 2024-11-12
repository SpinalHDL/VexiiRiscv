package vexiiriscv.soc.micro

import spinal.core._
import spinal.lib.system.tag.MemoryConnection
import vexiiriscv.VexiiRiscv
import vexiiriscv.execute.lsu.{LsuCachelessPlugin, LsuCachelessTileLinkPlugin, LsuL1Plugin, LsuL1TileLinkPlugin, LsuPlugin, LsuTileLinkPlugin}
import vexiiriscv.fetch.{FetchCachelessPlugin, FetchCachelessTileLinkPlugin, FetchL1Plugin, FetchL1TileLinkPlugin}
import vexiiriscv.soc.TilelinkVexiiRiscvFiber

import java.io.{File, PrintWriter}
import scala.collection.mutable


object Bsp{
  def apply(target : File, vexii: TilelinkVexiiRiscvFiber): Unit = {

    target.mkdirs()

    val socFile = new File(target, "soc.h")
    val headerWriter = new PrintWriter(socFile)

    headerWriter.println("#pragma once")

    def camelToUpperCase(str : String) = str.split("(?=\\p{Upper})").map(_.toUpperCase).mkString("_")
    val kv = mutable.LinkedHashMap[String, Any]()


    val peripherals = MemoryConnection.getMemoryTransfers(vexii.dBus)
    for(p <- peripherals){
      kv(p.node.getName() + "Addr") = p.where.mapping.lowerBound
    }


    for((name, value) <- kv){
      val patched = camelToUpperCase(name)
      value match {
        case value: Int => headerWriter.println(s"#define ${patched} $value")
        case value: Long => headerWriter.println(f"#define ${patched} 0x$value%x")
        case value: BigInt => headerWriter.println(f"#define ${patched} 0x$value%x")
        case value: FixedFrequency => headerWriter.println(s"#define ${patched} ${value.getValue.toBigDecimal.toBigInt.toString(10)}")
        case value: Boolean => headerWriter.println(s"#define ${patched} ${if (value) 1 else 0}")
        case _ =>
      }
    }

    headerWriter.close()
  }
}

object MicroSocGen extends App{
  val p = new MicroSocParam()

  assert(new scopt.OptionParser[Unit]("MicroSoc") {
    p.addOptions(this)
  }.parse(args, Unit).nonEmpty)
  p.legalize()

  val report = SpinalVerilog(new MicroSoc(p))

  Bsp(new File("."), report.toplevel.system.cpu)
}




//  val h = report.toplevel.main.cpu.logic.core.host
//  val path = PathTracer.impl(h[SrcPlugin].logic.addsub.rs2Patched, h[TrapPlugin].logic.harts(0).trap.pending.state.tval)
//  println(path.report)