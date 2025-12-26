// SPDX-FileCopyrightText: 2025 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.riscv

import spinal.core._

import scala.collection.mutable
import Rvi.{loadSpec, LoadSpec}

object Rvh extends AreaObject {
  import IntRegFile._

  val HLV_B              = TypeRLQ(M"011000000000-----100-----1110011")
  val HLV_H              = TypeRLQ(M"011001000000-----100-----1110011")
  val HLV_W              = TypeRLQ(M"011010000000-----100-----1110011")
  val HLV_D              = TypeRLQ(M"011011000000-----100-----1110011")
  val HLV_BU             = TypeRLQ(M"011000000001-----100-----1110011")
  val HLV_HU             = TypeRLQ(M"011001000001-----100-----1110011")
  val HLV_WU             = TypeRLQ(M"011010000001-----100-----1110011")

  val HLVX_HU            = TypeRLQ(M"011001000011-----100-----1110011")
  val HLVX_WU            = TypeRLQ(M"011010000011-----100-----1110011")

  val HSV_B              = TypeRSQ(M"0110001----------100000001110011")
  val HSV_H              = TypeRSQ(M"0110011----------100000001110011")
  val HSV_W              = TypeRSQ(M"0110101----------100000001110011")
  val HSV_D              = TypeRSQ(M"0110111----------100000001110011")

  val HFENCE_GVMA        = TypeNone(M"0110001----------000000001110011")
  val HFENCE_VVMA        = TypeNone(M"0010001----------000000001110011")

  loadSpec(HLV_B)  = LoadSpec( 8,  true)
  loadSpec(HLV_H)  = LoadSpec(16,  true)
  loadSpec(HLV_W)  = LoadSpec(32,  true)
  loadSpec(HLV_D)  = LoadSpec(64,  true)
  loadSpec(HLV_BU) = LoadSpec( 8, false)
  loadSpec(HLV_HU) = LoadSpec(16, false)
  loadSpec(HLV_WU) = LoadSpec(32, false)
  loadSpec(HLVX_HU) = LoadSpec(16, false)
  loadSpec(HLVX_WU) = LoadSpec(32, false)
}
