package vexiiriscv

import spinal.core._
import spinal.lib.misc.pipeline.SignalKey

package object decode {
  def DecodeList(e: (SignalKey[_ <: BaseType], Any)*) = List(e: _*)
  type DecodeListType = Seq[(SignalKey[_ <: BaseType], Any)]
}
