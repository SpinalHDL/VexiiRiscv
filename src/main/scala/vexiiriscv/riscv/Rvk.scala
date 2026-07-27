// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.riscv

import spinal.core._

/**
 * Specifies the RISC-V scalar crypto instructions.
 */
object Rvk {
  import IntRegFile._

  val ZKN_AES            = TypeR(M"--10--1----------000-----0110011")
  val AES32ESI           = TypeR(M"--10001----------000-----0110011")
  val AES32ESMI          = TypeR(M"--10011----------000-----0110011")
  val AES32DSI           = TypeR(M"--10101----------000-----0110011")
  val AES32DSMI          = TypeR(M"--10111----------000-----0110011")

  val AES64ES            = TypeR(M"0011001----------000-----0110011")
  val AES64ESM           = TypeR(M"0011011----------000-----0110011")
  val AES64DS            = TypeR(M"0011101----------000-----0110011")
  val AES64DSM           = TypeR(M"0011111----------000-----0110011")
  val AES64IM            = TypeI(M"001100000000-----001-----0010011")
  val AES64KS1I          = TypeI(M"00110001---------001-----0010011")
  val AES64KS2           = TypeR(M"0111111----------000-----0110011")

  val SHA256SUM0         = TypeI(M"000100000000-----001-----0010011")
  val SHA256SUM1         = TypeI(M"000100000001-----001-----0010011")
  val SHA256SIG0         = TypeI(M"000100000010-----001-----0010011")
  val SHA256SIG1         = TypeI(M"000100000011-----001-----0010011")

  val SHA512SUM0         = TypeI(M"000100000100-----001-----0010011")
  val SHA512SUM1         = TypeI(M"000100000101-----001-----0010011")
  val SHA512SIG0         = TypeI(M"000100000110-----001-----0010011")
  val SHA512SIG1         = TypeI(M"000100000111-----001-----0010011")

  val SHA512SUM0R        = TypeR(M"0101000----------000-----0110011")
  val SHA512SUM1R        = TypeR(M"0101001----------000-----0110011")
  val SHA512SIG0L        = TypeR(M"0101010----------000-----0110011")
  val SHA512SIG1L        = TypeR(M"0101011----------000-----0110011")
  val SHA512SIG0H        = TypeR(M"0101110----------000-----0110011")
  val SHA512SIG1H        = TypeR(M"0101111----------000-----0110011")

  val SM4ED              = TypeR(M"--11000----------000-----0110011")
  val SM4KS              = TypeR(M"--11010----------000-----0110011")

  val SM3P0              = TypeI(M"000100001000-----001-----0010011")
  val SM3P1              = TypeI(M"000100001001-----001-----0010011")
}
