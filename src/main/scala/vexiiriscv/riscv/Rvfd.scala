// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.riscv

import spinal.core._

/**
 * Specifies the RISC-V floating point instructions
 */
object Rvfd {
  import FloatRegFile._

  def FADD_S             = TypeR_RM(M"0000000------------------1010011")
  def FSUB_S             = TypeR_RM(M"0000100------------------1010011")
  def FMUL_S             = TypeR_RM(M"0001000------------------1010011")
  def FDIV_S             = TypeR_RM(M"0001100------------------1010011")
  def FSGNJ_S            = TypeR(M"0010000----------000-----1010011")
  def FSGNJN_S           = TypeR(M"0010000----------001-----1010011")
  def FSGNJX_S           = TypeR(M"0010000----------010-----1010011")
  def FMIN_S             = TypeR(M"0010100----------000-----1010011")
  def FMAX_S             = TypeR(M"0010100----------001-----1010011")
  def FSQRT_S            = TypeR1_RM(M"010110000000-------------1010011")
  def FCVT_S_W           = TypeI2F_RM(M"110100000000-------------1010011")
  def FCVT_S_WU          = TypeI2F_RM(M"110100000001-------------1010011")
  def FCVT_S_L           = TypeI2F_RM(M"110100000010-------------1010011")
  def FCVT_S_LU          = TypeI2F_RM(M"110100000011-------------1010011")
  def FCVT_W_S           = TypeF2I_RM(M"110000000000-------------1010011")
  def FCVT_WU_S          = TypeF2I_RM(M"110000000001-------------1010011")
  def FCVT_L_S           = TypeF2I_RM(M"110000000010-------------1010011")
  def FCVT_LU_S          = TypeF2I_RM(M"110000000011-------------1010011")
  def FCLASS_S           = TypeF2I(M"111000000000-----001-----1010011")
  def FMADD_S            = TypeR3_RM(M"-----00------------------1000011")
  def FMSUB_S            = TypeR3_RM(M"-----00------------------1000111")
  def FNMSUB_S           = TypeR3_RM(M"-----00------------------1001011")
  def FNMADD_S           = TypeR3_RM(M"-----00------------------1001111")

  def FLE_S              = TypeFCI(M"1010000----------000-----1010011")
  def FLT_S              = TypeFCI(M"1010000----------001-----1010011")
  def FEQ_S              = TypeFCI(M"1010000----------010-----1010011")

  def FADD_D             = TypeR_RM(M"0000001------------------1010011")
  def FSUB_D             = TypeR_RM(M"0000101------------------1010011")
  def FMUL_D             = TypeR_RM(M"0001001------------------1010011")
  def FDIV_D             = TypeR_RM(M"0001101------------------1010011")
  def FSGNJ_D            = TypeR(M"0010001----------000-----1010011")
  def FSGNJN_D           = TypeR(M"0010001----------001-----1010011")
  def FSGNJX_D           = TypeR(M"0010001----------010-----1010011")
  def FMIN_D             = TypeR(M"0010101----------000-----1010011")
  def FMAX_D             = TypeR(M"0010101----------001-----1010011")
  def FSQRT_D            = TypeR1_RM(M"010110100000-------------1010011")
  def FMV_X_W            = TypeF2I(M"111000000000-----000-----1010011")
  def FCVT_W_D           = TypeF2I_RM(M"110000100000-------------1010011")
  def FCVT_WU_D          = TypeF2I_RM(M"110000100001-------------1010011")
  def FCVT_L_D           = TypeF2I_RM(M"110000100010-------------1010011")
  def FCVT_LU_D          = TypeF2I_RM(M"110000100011-------------1010011")
  def FMV_X_D            = TypeF2I(M"111000100000-----000-----1010011")
  def FCLASS_D           = TypeF2I(M"111000100000-----001-----1010011")
  def FCVT_D_W           = TypeI2F(M"110100100000-------------1010011")
  def FCVT_D_WU          = TypeI2F(M"110100100001-------------1010011")
  def FCVT_D_L           = TypeI2F_RM(M"110100100010-------------1010011")
  def FCVT_D_LU          = TypeI2F_RM(M"110100100011-------------1010011")
  def FMV_W_X            = TypeI2F(M"111100000000-----000-----1010011")
  def FMV_D_X            = TypeI2F(M"111100100000-----000-----1010011")
  def FMADD_D            = TypeR3_RM(M"-----01------------------1000011")
  def FMSUB_D            = TypeR3_RM(M"-----01------------------1000111")
  def FNMSUB_D           = TypeR3_RM(M"-----01------------------1001011")
  def FNMADD_D           = TypeR3_RM(M"-----01------------------1001111")
  def FLE_D              = TypeFCI(M"1010001----------000-----1010011")
  def FLT_D              = TypeFCI(M"1010001----------001-----1010011")
  def FEQ_D              = TypeFCI(M"1010001----------010-----1010011")
  def FCVT_S_D           = TypeR1_RM(M"010000000001-------------1010011")
  def FCVT_D_S           = TypeR1(M"010000100000-------------1010011")


  def FLW                = TypeILQ(M"-----------------010-----0000111")
  def FLD                = TypeILQ(M"-----------------011-----0000111")
  def FSW                = TypeSSQ(M"-----------------010-----0100111")
  def FSD                = TypeSSQ(M"-----------------011-----0100111")
}
