package vexiiriscv

import spinal.core._
import spinal.lib.misc.pipeline.Payload

package object decode {
  def DecodeList(e: (Payload[_ <: BaseType], Any)*) = List(e: _*)
  type DecodeListType = Seq[(Payload[_ <: BaseType], Any)]
}
