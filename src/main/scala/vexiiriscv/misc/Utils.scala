package vexiiriscv.misc

import spinal.core._
import spinal.lib._

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AddressToMask{
  def apply(address : UInt, size : UInt, width : Int) : Bits ={
    size.muxListDc((0 to log2Up(width)).map(i => U(i) -> B((1 << (1 << i)) -1, width bits))) |<< address(log2Up(width)-1 downto 0)
  }
}

class Reservation{
  class Entry(val priority : Int) extends Area{
    val win = Bool()
    val take = False

    def takeIt() = take := True
  }
  val model = ArrayBuffer[Entry]()
  def create(priority : Int) : Entry = {
    val e = new  Entry( priority)
    model += e
    e
  }

  def build(){
    for(e <- model){
      e.win := !model.filter(_.priority < e.priority).map(_.take).orR
    }
  }
}

/**
 * MulSpliter is a tool which will cut a multiplication into multiple smaller multiplications
 * Those smaller multiplication results would need to be summed together.
 * MulSpliter Doesn't generate any hardware by itself, but instead provide you with the datamodel
 * of the work to do.
 * Useful for large multiplications which need to be pipelined on multiple cycles when retiming isn't good
 */
object MulSpliter {
  /**
   * A and B being the input operands
   * offsetX, widthX, signedX specifying the operand
   */
  case class Splits(offsetA : Int, offsetB : Int,
                    widthA : Int, widthB : Int,
                    signedA : Boolean, signedB : Boolean, id : Int){
    val offsetC = offsetA+offsetB
    val widthC = if(widthB == 1 && !signedB) widthA else widthA + widthB
    val endC = offsetC+widthC
    def signedC = signedA || signedB

    // Generate the multiplication hardware, and return a UInt.
    // For signed multiplications, the result is sign extended to give a signedWidth bits signal
    // allowing to just sum all the partial multiplication in a single unsigned manner.
    def toMulU(srcA : Bits, srcB : Bits, signedWidth : Int): UInt = {
      val a = srcA(offsetA, widthA bits)
      val b = srcB(offsetB, widthB bits)
      val sw = signedWidth - offsetC
      if(widthB == 1 && !signedB){
        return signedA match {
          case false => U(a).andMask(b.asBool)
          case true  => S(a.andMask(b.asBool)).resize(sw bits).asUInt
        }
      }
      (signedA, signedB) match {
        case (false, false) =>U(a) * U(b)
        case (false, true) => (S(False ## a) * S(b)).resize(sw bits).asUInt
        case (true, false) => (S(a) * S(False ## b)).resize(sw bits).asUInt
        case (true, true) => (S(a) * S(b)).resize(sw).asUInt
      }
    }
  }

  // Generate the datamodel
  def apply(inWidthA : Int, inWidthB : Int,
            splitWidthA : Int, splitWidthB : Int,
            signedA : Boolean, signedB : Boolean) = {
    val outWidth = inWidthA + inWidthB
    val splitsUnordered = for (offsetA <- 0 until inWidthA by splitWidthA;
                               offsetB <- 0 until inWidthB by splitWidthB;
                               widthA = (inWidthA - offsetA) min splitWidthA;
                               widthB = (inWidthB - offsetB) min splitWidthB) yield {
      Splits(offsetA, offsetB,
        widthA, widthB,
        signedA && offsetA + widthA == inWidthA,
        signedB && offsetB + widthB == inWidthB,
        -1
      )
    }
    val splits = splitsUnordered.sortWith(_.endC < _.endC).zipWithIndex.map(e => e._1.copy(id = e._2))
    splits
  }
}

/**
 * Facility to add together a large number of values in a optimized / staged manner
 * Able to cut / concat sources to build multiple adders
 */
object AdderAggregator {
  def Source(s : MulSpliter.Splits, signedWidth : Int) : Source = {
    val width = if(s.signedC) signedWidth - s.offsetC else s.widthC
    Source(s.offsetC, (BigInt(1) << width)-1)
  }
  // Represent a input value
  case class Source(offset: Int, localMax: BigInt) extends OverridedEqualsHashCode{
    var offsetTmp = offset
    val width = log2Up(localMax + 1)
    //    var offsetCost, bitCost = 0

    def offsetNext = offset + width

    override def toString = s"Source($offset, $width bits, $localMax)"
  }

  // Represent a input value in a lane, from which we specify "offset" at which bits should be taken
  case class LaneSource(s : Source){
    val offset = s.offsetTmp

    // Provide the maximal value that "this" can have for a given lane's offset/width
    def valueMax(offset: Int, width: Int) = {
      val widthOverflow = (s.offsetNext) - (offset + width)
      val fixedValue = if (widthOverflow > 0) (BigInt(1) << s.width - widthOverflow) - 1 else s.localMax
      val shiftedTmp = fixedValue >> (this.offset - s.offset)
      val delta = this.offset - offset
      val shifted = if (delta > 0) shiftedTmp << delta else shiftedTmp >> -delta
      shifted
    }
  }

  // Represent an adder input by aggregating multiple laneSources which never overlap
  // The idea is to create as long as possible inputs for the adder
  case class Lane(from: scala.collection.Seq[LaneSource]) {
    def valueMax(offset: Int, width: Int) = from.map(_.valueMax(offset, width)).sum

    // Generate the aggregation hardware
    def craft(offset: Int, width: Int, s2u: scala.collection.Map[Source, UInt]): UInt = {
      val ret = U(0, width bits).allowOverride()
      for(s <- from){
        val l = Math.max(s.offset, offset)
        val h = Math.min(s.s.offsetNext, offset + width) - 1
        ret(h-offset downto l-offset) := s2u(s.s)(h-s.s.offset downto l-s.s.offset)
      }
      ret
    }
  }

  // Represent an adder which sum multiple lanes
  // offset and width specify which portions of the sources signal we are interested into (can be parts of them)
  case class Adder(offset: Int, width: Int, lanes: scala.collection.Seq[Lane]) {
    def toSource() = {
      val source = Source(offset, lanes.map(_.valueMax(offset, width)).sum)
      source
    }

    // Generate the adder hardware using a s2u map to retrieve the sources hardware signals
    def craft(s2u : scala.collection.Map[Source, UInt]): UInt = {
      val sRet = toSource()
      val uLanes = lanes.map(_.craft(offset, width, s2u).resize(sRet.width bits))
      uLanes.reduceBalancedTree(_ + _)
    }
  }

  // Generate the data model to add multiple unsigned sources together
  // widthMax specify how many bits an adder can have
  // lanesMax specify how many inputs an adder can have
  // Note that if the returned adders is only for one layer, meaning you may have to call
  // this function multiple time to reduce more and more, until you get only a single adder.
  def apply(splits: Seq[Source], widthMax: Int, lanesMax: Int, untilOffset : Int = Integer.MAX_VALUE): scala.collection.Seq[Adder] = {
    var srcs = ArrayBuffer[Source]()
    val adders = ArrayBuffer[Adder]()
    srcs ++= splits.sortBy(_.offset)
    srcs.foreach(s => s.offsetTmp = s.offset)

    while (srcs.size != 0) {
      for (i <- srcs.indices.dropRight(1)) assert(srcs(i).offsetTmp <= srcs(i + 1).offsetTmp)
      // Check if the have other srcs in range
      if (srcs.size == 1 || srcs(0).offsetNext <= srcs(1).offsetTmp || srcs(0).offsetTmp >= untilOffset) {
        val a = srcs.head
        adders += Adder(a.offsetTmp, a.offsetNext - a.offsetTmp, List(Lane(List(LaneSource(a)))))
        srcs.remove(0)
      } else {
        val adderOffset = srcs(0).offsetTmp
        val logicOffset = srcs(1).offsetTmp
        val logicOffsetNext = logicOffset + widthMax
        val lanes = ArrayBuffer[Lane]()
        // Build lanes
        while (lanes.size < lanesMax && srcs.nonEmpty && srcs(0).offsetTmp < logicOffsetNext) {
          val from = ArrayBuffer[LaneSource]()
          val lane = Lane(from)
          lanes += lane
          var ptr = adderOffset
          var continue = false
          // Build a lane
          do {
            continue = false
            val iSource = srcs.indexWhere(e => e.offsetTmp >= ptr)
            if (iSource != -1 && srcs(iSource).offsetTmp < logicOffsetNext) {
              continue = true;
              val source = srcs(iSource)
              ptr = source.offsetNext
              from += LaneSource(source)
              source.offsetTmp = logicOffsetNext
              srcs.remove(iSource)
              if (source.offsetTmp < source.offsetNext) {
                // soure need to stay
                val iTarget = srcs.indexWhere(source.offsetTmp < _.offsetTmp) match {
                  case -1 => srcs.size
                  case v => v
                }
                srcs.insert(iTarget, source)
              }
            }
          } while (continue)
        }

        val adderOffsetNext = lanes.map(_.from.map(_.s.offsetNext).max).max min logicOffsetNext
        val adder = Adder(
          offset = adderOffset,
          width = adderOffsetNext - adderOffset,
          lanes = lanes
        )
        adders += adder
      }
    }

    adders
  }

  // Here is an example of usage.
  def main(args: Array[String]): Unit = {
    import spinal.core.sim._
    //    var sources = ArrayBuffer[Source]()
    //    sources += Source(0,   255)
    //    sources += Source(0,   255)
    //
    //    for(i <- 0 until 8){
    //      val adders = AdderAggregator(sources, 4, 4)
    //      println(adders.mkString("\n"))
    //      println("------------")
    //      sources.clear()
    //      sources ++= adders.map(_.toSource())
    //    }

    val aw = 32
    val bw = 32
    val cw = aw + bw
    SimConfig.withFstWave.compile(new Component{
      val doSigned = true
      val a = in Bits (aw bits)
      val b = in Bits (bw bits)
      val aSigned = out(S(a))
      val bSigned = out(S(b))
      val c = out Bits (cw bits)
      val cSigned = out(S(c))

      val splitsSpec = MulSpliter(
        aw, bw,
        1000, 1,
        doSigned, doSigned
      )
      val muls = splitsSpec.map(_.toMulU(a, b, cw))
      val sourceToSignal = mutable.LinkedHashMap[AdderAggregator.Source, UInt]()
      var sourcesSpec = splitsSpec.map(s => AdderAggregator.Source(s, cw)).toList
      for((s, m) <- (sourcesSpec, muls).zipped) sourceToSignal(s) = m

      var stepCounter = 0
      class Step() extends Area {
        val ss = sourcesSpec
        var addersSpec = stepCounter match {
          case 0 => AdderAggregator(sourcesSpec, 16, 2)
          case _ => AdderAggregator(sourcesSpec, 32, 8)
        }
        val adders = addersSpec.map(_.craft(sourceToSignal))
        sourcesSpec = addersSpec.map(_.toSource()).toList
        for ((s, m) <- (sourcesSpec, adders).zipped) sourceToSignal(s) = m
        println(addersSpec.mkString("\n"))
        println("------------")
        stepCounter += 1
      }
      val stepsBuffer = ArrayBuffer[Step]()
      while(sourcesSpec.size != 1) stepsBuffer += new Step()
      val steps = stepsBuffer

      c := sourceToSignal(sourcesSpec(0)).asBits.resized


    }).doSim{ dut =>
      import dut.{a,b,c}
      def check(x : BigInt, y : BigInt): Unit = {
        a #= x
        b #= y
        sleep(1)
        val ar = if(dut.doSigned) dut.aSigned.toBigInt else dut.a.toBigInt
        val br = if(dut.doSigned) dut.bSigned.toBigInt else dut.b.toBigInt
        val ref = ar*br
        val got = if(dut.doSigned) dut.cSigned.toBigInt else dut.c.toBigInt
        assert(got == ref, f"$got%x $ref%x")
      }

      for(i <- 0 until aw; i2 <- 0 until bw){
        check(BigInt(1) << i, BigInt(1) << i2)
      }

      for(i <- 0 until 100000){
        check(a.randomizedBigInt(), b.randomizedBigInt())
      }
    }

  }
}

case class AgedArbiterUp[T <: Data](valid : Bool, payload : T, age : Int, laneAge : UInt, subAge : Int)
class AgedArbiter[T <: Data](ups : scala.collection.Seq[AgedArbiterUp[T]]) extends Area{
  val groups = ups.groupBy(_.age).toSeq.sortBy(_._1).toSeq.reverse
  val ports = for ((_, elements) <- groups) yield {
    val ret = Flow(elements.head.payload)
    val valids = B(for (self <- elements) yield self.valid && elements.filter(_ != self).map(other => !(other.valid && (other.subAge >= self.subAge).mux(other.laneAge < self.laneAge, other.laneAge <= self.laneAge))).andR)
    ret.valid := elements.map(_.valid).orR
    ret.payload := OHMux.or(valids, elements.map(_.payload), true)
    ret
  }
  val oh = B(OHMasking.firstV2(B(ports.map(_.valid))))
  val down = Flow(ups.head.payload)
  down.valid := ups.map(_.valid).orR
  down.payload := OHMux.or(oh, ports.map(_.payload))
}


object FloorplanDisplay extends App{
  import java.awt.{Dimension, Graphics}
  import java.awt.image.BufferedImage
  import javax.swing.{JFrame, JPanel, WindowConstants}

  var width = 500
  var height = 700
  var scale = 1.0
  val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_BGR);

  val frame = new JFrame {
    val fWidth = (width * scale).toInt
    val hWidth = (height * scale).toInt
    setPreferredSize(new Dimension(fWidth, hWidth));

    add(new JPanel {
      this.setPreferredSize(new Dimension(fWidth, hWidth))

      override def paintComponent(g: Graphics): Unit = {
        g.drawImage(image, 0, 0, fWidth, hWidth, null)
      }
    })

    pack();
    setVisible(true);
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
  }

  val f = new File(args(0))
  import scala.io.Source
  var lineId = 6
  for(line <- Source.fromFile(f).getLines().drop(6)) {
    lineId += 1
    val parts = line.split("\t+")
    val name = parts(0)
    val x = parts(1).toInt
    val y = parts(2).toInt
    if(name.endsWith("~FF")) {
      var color = Color.LIGHT_GRAY
      if (name.contains("/vexiis_0_logic_core/")) color = Color.RED
      if (name.contains("/vexiis_1_logic_core/")) color = Color.GREEN
      if (name.contains("/vexiis_2_logic_core/")) color = Color.BLUE
      if (name.contains("/vexiis_3_logic_core/")) color = Color.YELLOW
      if (name.contains("_logic_core/") || true) {
        if(y >= 0 && y < height) image.setRGB(x, y, color.getRGB)
      }
    }
  }

  frame.repaint()

  ImageIO.write(image, "png", new File("floorplan.png"))
}