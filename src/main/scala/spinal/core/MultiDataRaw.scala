///*                                                                           *\
//**        _____ ____  _____   _____    __                                    **
//**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
//**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
//**      ___/ / ____// // /|  / ___ |/ /___                                   **
//**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
//**                                                                           **
//**      This library is free software; you can redistribute it and/or        **
//**    modify it under the terms of the GNU Lesser General Public             **
//**    License as published by the Free Software Foundation; either           **
//**    version 3.0 of the License, or (at your option) any later version.     **
//**                                                                           **
//**      This library is distributed in the hope that it will be useful,      **
//**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
//**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
//**    Lesser General Public License for more details.                        **
//**                                                                           **
//**      You should have received a copy of the GNU Lesser General Public     **
//**    License along with this library.                                       **
//\*                                                                           */
//package spinal.core
//
//import spinal.core.internals.Operator
//import spinal.idslplugin.Location
//
//import scala.collection.mutable.ArrayBuffer
//
///**
//  * Base class for multi data like Vec, Bundle
//  */
//abstract class MultiDataRaw extends Data {
//  def elements: ArrayBuffer[Data]
//  def zipped(that: MultiDataRaw): Seq[(Data, Data)]
//
//  override def flattenLocalName: Seq[String] = {
//    val result = ArrayBuffer[String]()
//    for (e <- elements) {
//      result ++= e.flattenLocalName.map(name => if (name == "") localName else localName + "_" + name)
//    }
//    result // for fun elements.map{case (localName,e) => e.flattenLocalName.map(name => if(name == "") localName else localName + "_" + name)}.reduce(_ ++: _)
//  }
//
//
//  override def addTag[T <: SpinalTag](spinalTag: T): this.type = {
//    super.addTag(spinalTag)
//    elements.foreach(_.addTag(spinalTag))
//    this
//  }
//
//  override def asBits: Bits = {
//    var ret: Bits = null
//    for (e <- elements) {
//      if (ret == null.asInstanceOf[Object]) ret = e.asBits
//      else ret = e.asBits ## ret
//    }
//    if (ret.asInstanceOf[Object] == null) ret = Bits(0 bits)
//    ret
//  }
//
//  override def getBitsWidth: Int = {
//    var accumulateWidth = 0
//    for (e <- elements) {
//      val width = e.getBitsWidth
//      if (width == -1)
//        SpinalError("Can't return bits width")
//      accumulateWidth += width
//    }
//    accumulateWidth
//  }
//
//  override def asInput(): this.type = {
//    super.asInput()
//    elements.foreach(_.asInput())
//    this
//  }
//
//  override def asOutput(): this.type = {
//    super.asOutput()
//    elements.foreach(_.asOutput())
//    this
//  }
//
//  override def asInOut(): this.type = {
//    super.asInOut()
//    elements.foreach(_.asInOut())
//    this
//  }
//
//  override def copyDirectionOfImpl(that : Data): this.type = {
//    super.copyDirectionOfImpl(that)
//    (elements, that.asInstanceOf[MultiDataRaw].elements).zipped.foreach{case (t, s) => t.copyDirectionOfImpl(s)}
//    this
//  }
//
//
//  override def setAsDirectionLess(): this.type = {
//    super.setAsDirectionLess()
//    elements.foreach(_.setAsDirectionLess());
//    this
//  }
//
//  /** Set baseType to reg */
//  override def setAsReg(): this.type = {
//    elements.foreach(_.setAsReg())
//    this
//  }
//
//  /** Set baseType to Combinatorial */
//  override def setAsComb(): this.type = {
//    elements.foreach(_.setAsComb())
//    this
//  }
//
//
//  override def freeze(): MultiDataRaw.this.type = {
//    elements.foreach(_.freeze())
//    this
//  }
//
//  override def unfreeze(): MultiDataRaw.this.type = {
//    elements.foreach(_.unfreeze())
//    this
//  }
//
//  override def flatten: Seq[BaseType] = {
//    elements.map(_.flatten).foldLeft(List[BaseType]())(_ ++ _)
//  }
//
//  override def flattenForeach(body: (BaseType) => Unit): Unit = {
//    elements.foreach(_.flattenForeach(body))
//  }
//
//  override def assignFromBits(bits: Bits): Unit = {
//    var offset = 0
//    for (e <- elements) {
//      val width = e.getBitsWidth
//      e.assignFromBits(bits(offset, width bit))
//      offset = offset + width
//    }
//  }
//
//  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = {
//    var offset = 0
//    var bitsOffset = 0
//
//    for (e <- elements) {
//      val width = e.getBitsWidth
//
//      if (hi >= offset && lo < offset + width) {
//        val high = Math.min(hi-offset,width-1)
//        val low  = Math.max(lo-offset,0)
//        val bitUsage = high - low + 1
//        e.assignFromBits(bits(bitsOffset,bitUsage bit), high,low)
//        bitsOffset += bitUsage
//      }
//
//      offset = offset + width
//    }
//
//  }
//
//  private[core] def isEqualTo(that: Any): Bool = {
//    that match {
//      case that: MultiDataRaw => {
//        val checks = zippedMap(that, _ === _)
//        if(checks.nonEmpty) checks.reduce(_ && _) else True
//      }
//      case _               => SpinalError(s"Function isEquals is not implemented between $this and $that")
//    }
//  }
//
//  private[core] def isNotEqualTo(that: Any): Bool = {
//    that match {
//      case that: MultiDataRaw =>{
//        val checks = zippedMap(that, _ =/= _)
//        if(checks.nonEmpty) checks.reduce(_ || _) else False
//      }
//      case _               => SpinalError(s"Function isNotEquals is not implemented between $this and $that")
//    }
//  }
//
//  private[core] override def autoConnect(that: Data)(implicit loc: Location): Unit = {
//    that match {
//      case that: MultiDataRaw => zippedMap(that, _ autoConnect _)
//      case _               => SpinalError(s"Function autoConnect is not implemented between $this and $that")
//    }
//  }
//
//  def elementsString = this.elements.map(_.toString()).reduce(_ + "\n" + _)
//
//  private[core] def zippedMap[T](that: MultiDataRaw, task: (Data, Data) => T): Seq[T] = {
//    zipped(that).map(e => task(e._1, e._2))
////    if (that.elements.length != this.elements.length) SpinalError(s"Can't zip [$this] with [$that]  because they don't have the same number of elements.\nFirst one has :\n${this.elementsString}\nSeconde one has :\n${that.elementsString}\n")
////    this.elements.map(x => {
////      val (n, e) = x
////      val other = that.find(n)
////      if (other == null) SpinalError(s"Can't zip [$this] with [$that] because the element named '${n}' is missing in the second one")
////      task(e, other)
////    })
//  }
//
//  override def getZero: this.type = {
//    val ret = cloneOf(this)
//    ret.elements.foreach(e => {
//      e := e.getZero
//    })
//    ret.asInstanceOf[this.type]
//  }
//
//  override def flip(): this.type  = {
//    for (e <- elements) {
//      e.flip()
//    }
//
//    dir match {
//      case `in`  => dir = out
//      case `out` => dir = in
//      case _     =>
//    }
//    this
//  }
//
//  override def assignFormalRandom(kind: Operator.Formal.RandomExpKind) = elements.foreach(_.assignFormalRandom(kind))
//}
