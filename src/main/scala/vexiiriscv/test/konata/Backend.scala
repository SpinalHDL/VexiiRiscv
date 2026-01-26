package vexiiriscv.test.konata

import org.apache.commons.io.FileUtils

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.{SortedMap, mutable}
import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.util.HashSet
import spinal.core.sim._

abstract class Command(val at : Long){
  def toString(id : Long) : String
}

class Spawn(at : Long, hartId : Int) extends Command(at){
  override def toString(id : Long): String = s"I\t$id\t0\t$hartId\n"
}

class Comment(at : Long, text : String) extends Command(at){
  override def toString(id: Long): String = s"L\t$id\t0\t$text\n"
}

class Stage(at : Long, name : String) extends Command(at){
  override def toString(id : Long): String = s"S\t$id\t0\t$name\n"
}

class Retire(at : Long) extends Command(at){
  override def toString(id : Long): String = s"R\t$id\t$id\t0\n"
}

class Flush(at : Long) extends Command(at){
  override def toString(id : Long): String = s"R\t$id\t$id\t1\n"
}

class Instruction(){
  var id = -1l
  var ptr = 0
  val elements = ArrayBuffer[Command]()
  def nextAt = elements(ptr).at
  def pop() ={
    val ret = elements(ptr)
    ptr += 1
    ret
  }
  def +=(that : Command) = {
    if(elements.nonEmpty) assert(elements.last.at <= that.at)
    elements += that
  }
  def nonEmpty = ptr != elements.size
  def first = ptr == 0
}

class Thread{
  var cycleLock = -1l
}

/**
 * This konata backend symplify the way a konata file can be generated, by handeling a buffer of in progress instructions.
 *
 * For each hardware thread, the user need to have invoked the newThread function once. Each Thread returned contains a cycleLock timestamp
 * which specifies until which time the pendings instruction for the given thread are full completed can could be flushed out.
 *
 * The user can push instructions into its pendings hashmap, using the instruction creation timestamp as a key.
 * Instructions need to be  have their "elements" lists of Command already populated, Commands having a timestamp and being able to write themself as a konata string.
 *
 * The user can periodicaly call the flush function, this will flush all the instructions until the minimal threads.cycleLock
 */
class Backend(f : File) {
  FileUtils.forceMkdir(f.getParentFile)
  val bf = new BufferedWriter(new FileWriter(f))

  def newThread(): Thread = {
    val t = new Thread
    threads += t
    t
  }

  val threads = ArrayBuffer[Thread]()
  val pendings = mutable.HashMap[Long, mutable.ArrayBuffer[Instruction]]()

  var cycle = 0l
  var idAlloc = 0l

  def insert(i : Instruction) = if(i.nonEmpty) {
    val buf = pendings.getOrElseUpdate(i.nextAt, new mutable.ArrayBuffer[Instruction]())
    buf += i
  }

  bf.write("Kanata\t0004\n")
  bf.write("C=\t0\n")

  def refresh(): Unit = {
    val cycleEnd = if(threads.nonEmpty) threads.map(_.cycleLock).min else cycle
    var skips = 0l

    while(cycle != cycleEnd && pendings.nonEmpty){
      skips += 1
      pendings.get(cycle) match {
        case Some(instrs) => {
          bf.write(s"C\t$skips\n")
          skips = 0
          for(instr <- instrs){
            if(instr.first) {
              instr.id = idAlloc
              idAlloc += 1
            }
            do {
              val e = instr.pop()
              bf.write(e.toString(instr.id))
            } while(instr.nonEmpty && instr.nextAt == cycle)
            insert(instr)
          }
          pendings.remove(cycle)
        }
        case None =>
      }
      cycle += 1
    }
    if (skips != 0) bf.write(s"C\t$skips\n")
  }


  def flush(): Unit = {
    refresh()
    bf.flush()
  }

  def close() : Unit = {
    refresh()
    bf.close()
  }

  def spinalSimFlusher(period: Long): this.type = {
    periodicaly(period){
      flush()
    }
    delayed(1)(onSimEnd(close()))
    this
  }
}
