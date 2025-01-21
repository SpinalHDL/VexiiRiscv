package spinal.lib.misc

import net.fornwall.jelf.{ElfFile, ElfSection, ElfSectionHeader, ElfSymbol, ElfSymbolTableSection}
import spinal.core._
import spinal.lib.sim.SparseMemory

import java.io.File
import java.nio.file.Files
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Provide a many utilities (based on net.fornwall.jelf) which allows to :
 * - Parse a ELF file
 * - Find the address of a given symbol
 * - Load an ELF program into a simulation DRAM
 */
class Elf(val f : File, addressWidth : Int){
  val fBytes = Files.readAllBytes(f.toPath)
  val elf = ElfFile.from(fBytes)

  def foreachSection(body : ElfSection => Unit): Unit ={
    for(sectionId <- 0 until elf.e_shnum) {
      val section = elf.getSection(sectionId)
      body(section)
    }
  }

  def getData(section : ElfSection): Array[Byte] ={
    val fileAddress = section.header.sh_offset
    val size = section.header.sh_size.toInt
    if(size == 0) return Array.fill(0)(0.toByte)

    val ret = new Array[Byte](size)
    if(section.header.sh_type != ElfSectionHeader.SHT_NOBITS && section.header.sh_type != ElfSectionHeader.SHT_NULL) {
      Array.copy(fBytes, fileAddress.toInt, ret, 0, size)
    }
    ret
  }

  def load(mem : SparseMemory, offset : Long): Unit ={
    foreachSection{section =>
      if((section.header.sh_flags & ElfSectionHeader.FLAG_ALLOC) != 0){
        val data = getData(section)
        val memoryAddress = (section.header.sh_addr - offset) & ((BigInt(1) << addressWidth)-1).toLong
        mem.write(memoryAddress, data)
      }
    }
  }


  def getMemInit[T <: Data](ram: Mem[T],offset: BigInt, allowOverflow: Boolean = false) = {
    val wordSize = ram.wordType.getBitsWidth / 8
    val initContent = Array.fill[BigInt](ram.wordCount)(0)
    foreachSection { section =>
      if ((section.header.sh_flags & ElfSectionHeader.FLAG_ALLOC) != 0) {
        val data = getData(section)
        val memoryAddress = (section.header.sh_addr - offset) & ((BigInt(1) << addressWidth) - 1).toLong
        for((byte, i) <- data.zipWithIndex){
          val addressWithoutOffset = memoryAddress+i
          val addressWord = addressWithoutOffset / wordSize
          if (addressWord < 0 || addressWord >= initContent.size) {
            assert(allowOverflow)
          } else {
            initContent(addressWord.toInt) |= BigInt(byte.toInt & 0xFF) << ((addressWithoutOffset.toInt % wordSize) * 8)
          }
        }
      }
    }
    initContent
  }

  def load(func : (Long, Byte) => Unit) : Unit = {
    foreachSection { section =>
      if ((section.header.sh_flags & ElfSectionHeader.FLAG_ALLOC) != 0) {
        val data = getData(section)
        val memoryAddress = (section.header.sh_addr) & ((BigInt(1) << addressWidth) - 1).toLong
        for((byte, i) <- data.zipWithIndex){
          func(memoryAddress+i, byte)
        }
      }
    }
  }

  def loadArray(array : Array[Byte], offset : Long, allowOverflow: Boolean = false) : Unit = {
    load{(address, data) =>
      if(address > offset && address  < offset + array.size) array(address - offset toInt) = data
      else assert(allowOverflow)
    }
  }

  def init[T <: Data](ram: Mem[T], offset: BigInt, allowOverflow: Boolean = false): Unit = {
    val initContent = getMemInit(ram, offset, allowOverflow)
    ram.initBigInt(initContent)
  }

  def load[T <: Data](ram: Mem[T], offset: BigInt, allowOverflow: Boolean = false): Unit = {
    val initContent = getMemInit(ram, offset, allowOverflow)
    import spinal.core.sim._
    for((e, i) <- initContent.zipWithIndex){
      ram.setBigInt(i, e)
    }
  }


  def getSymbolAddress(name : String): Long ={
    val s = getELFSymbol(name)
    s.st_value
  }

  def getELFSymbol(symbolName: String): ElfSymbol = {
    if (symbolName == null) return null
    // Check dynamic symbol table for symbol name.
    import elf._
    var sh = getDynamicSymbolTableSection
    if (sh != null) {
      val numSymbols = sh.symbols.length
      var i = 0
      while ( {
        i < numSymbols
      }) {
        var symbol = sh.symbols(i)
        if (symbolName == symbol.getName) return symbol
        else if (symbolName == (sh.symbols(numSymbols - 1 - i)).getName) return sh.symbols(numSymbols - 1 - i)

        i += 1
      }
    }
    // Check symbol table for symbol name.
    sh = getSymbolTableSection
    if (sh != null) {
      val numSymbols = sh.symbols.length
      var i = 0
      while ( {
        i < numSymbols
      }) {
        var symbol = sh.symbols(i)
        if (symbolName == symbol.getName) return symbol
        else if (symbolName == (sh.symbols(numSymbols - 1 - i)).getName) return sh.symbols(numSymbols - 1 - i)

        i += 1
      }
    }
    null
  }
}

object ElfTest extends App{
  import net.fornwall.jelf._
  val elf = new Elf(new File("ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf"), 32)

  elf.foreachSection{section =>
    println(f"${section.header.getName} ${section.header.sh_type} ${section.header.sh_flags}")
    if((section.header.sh_flags & ElfSectionHeader.FLAG_ALLOC) != 0){
      val data = elf.getData(section)
      println(section.header.getName)
      println(data)
    }
  }
  val a = elf.getSymbolAddress("_start")
  println(a)
}

/**
 * Dev tool which takes as input two file :
 * - The ELF file of the app being executed, currently linux (/media/data2/proj/vexii/litex/buildroot/buildroot/build/rv32ima/build/linux-custom/vmlinux)
 * - A text file formated with lines which have hexadecimal values. Those values being the PC being executed by the CPU, sampled at random intervals.
 *
 * So, this tool will generate a histogram of which function from the ELF file makes the CPU busy the most time.
 *
 * The input text file being generated by running the Litex's VexiiRiscv SoC, and extracting via jtag the CPU PC location via
 * the src/main/tcl/openocd/trace.tcl script, which uses a custom JTAG tap instruction (0x17) to sample the CPU PC without any disturbance  to the CPU
 */
object ElfMapper extends App{
  import net.fornwall.jelf._
  val elf = new Elf(new File("/media/data2/proj/vexii/litex/buildroot/buildroot/build/rv32ima/build/linux-custom/vmlinux"), 32)
  val mapping = mutable.HashMap[Long, ElfSymbol]()

  def mapSymbol(s : ElfSymbol): Unit = {
    val base = s.st_value
    val size = s.st_size
//    println(f"${base}%08x ${size}%08x ${s.getName}")
    for(i <- 0 until size.toInt){
      mapping((base + i) & 0xFFFFFFFFl) = s
    }
  }
  var sh = elf.elf.getDynamicSymbolTableSection
  if (sh != null) {
    val numSymbols = sh.symbols.length
    var i = 0
    while ( {
      i < numSymbols
    }) {
      var symbol = sh.symbols(i)
      mapSymbol(symbol)
      i += 1
    }
  }
  // Check symbol table for symbol name.
  sh = elf.elf.getSymbolTableSection
  if (sh != null) {
    val numSymbols = sh.symbols.length
    var i = 0
    while ( {
      i < numSymbols
    }) {
      var symbol = sh.symbols(i)
      mapSymbol(symbol)
      i += 1
    }
  }

  val source = Source.fromFile("/media/data2/proj/vexii/VexiiRiscv/src/main/tcl/openocd/trace.out")
  val matched = mutable.LinkedHashMap[ElfSymbol, Ctx]();
  class Ctx(){
    var counter = 0
    var hits = mutable.HashMap[Long, Int]()
  }
  var unknown = 0l
  for (line <- source.getLines()) {
    val splits = line.split(" ")
    val pc = BigInt(splits(0), 16).toLong
    mapping.get(pc) match {
      case Some(s) => {
        val sCtx = matched.getOrElseUpdate(s, new Ctx())
        sCtx.hits(pc) = sCtx.hits.getOrElse(pc, 0) + 1
        sCtx.counter += 1
      }
      case None => unknown += 1 //println(":(")
    }
  }

  val sorted = matched.toArray.sortBy(_._2.counter)
  println("######################")
  for((s, ctx) <- sorted){
    println(s"${s.getName} got ${ctx.counter}")
//    for((pc, cnt) <- ctx.hits.toArray.sortBy(_._2)){
    for((pc, cnt) <- ctx.hits.toArray.sortBy(_._1)){
//      println(f"- ${pc}%08x got ${cnt}")
    }
  }
  println(s"Unkown -> $unknown")
  source.close()
}