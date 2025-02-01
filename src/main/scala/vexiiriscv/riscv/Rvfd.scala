// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.riscv

import spinal.core._

/**
 * Specifies the RISC-V floating point instructions
 */
object Rvfd extends AreaObject {
  import FloatRegFile._

  val FADD_S             = TypeR_RM(M"0000000------------------1010011")
  val FSUB_S             = TypeR_RM(M"0000100------------------1010011")
  val FMUL_S             = TypeR_RM(M"0001000------------------1010011")
  val FDIV_S             = TypeR_RM(M"0001100------------------1010011")
  val FSGNJ_S            = TypeR(M"0010000----------000-----1010011")
  val FSGNJN_S           = TypeR(M"0010000----------001-----1010011")
  val FSGNJX_S           = TypeR(M"0010000----------010-----1010011")
  val FMIN_S             = TypeR(M"0010100----------000-----1010011")
  val FMAX_S             = TypeR(M"0010100----------001-----1010011")
  val FSQRT_S            = TypeR1_RM(M"010110000000-------------1010011")
  val FCVT_S_W           = TypeI2F_RM(M"110100000000-------------1010011")
  val FCVT_S_WU          = TypeI2F_RM(M"110100000001-------------1010011")
  val FCVT_S_L           = TypeI2F_RM(M"110100000010-------------1010011")
  val FCVT_S_LU          = TypeI2F_RM(M"110100000011-------------1010011")
  val FCVT_W_S           = TypeF2I_RM(M"110000000000-------------1010011")
  val FCVT_WU_S          = TypeF2I_RM(M"110000000001-------------1010011")
  val FCVT_L_S           = TypeF2I_RM(M"110000000010-------------1010011")
  val FCVT_LU_S          = TypeF2I_RM(M"110000000011-------------1010011")
  val FCLASS_S           = TypeF2I(M"111000000000-----001-----1010011")
  val FMADD_S            = TypeR3_RM(M"-----00------------------1000011")
  val FMSUB_S            = TypeR3_RM(M"-----00------------------1000111")
  val FNMSUB_S           = TypeR3_RM(M"-----00------------------1001011")
  val FNMADD_S           = TypeR3_RM(M"-----00------------------1001111")

  val FLE_S              = TypeFCI(M"1010000----------000-----1010011")
  val FLT_S              = TypeFCI(M"1010000----------001-----1010011")
  val FEQ_S              = TypeFCI(M"1010000----------010-----1010011")

  val FADD_D             = TypeR_RM(M"0000001------------------1010011")
  val FSUB_D             = TypeR_RM(M"0000101------------------1010011")
  val FMUL_D             = TypeR_RM(M"0001001------------------1010011")
  val FDIV_D             = TypeR_RM(M"0001101------------------1010011")
  val FSGNJ_D            = TypeR(M"0010001----------000-----1010011")
  val FSGNJN_D           = TypeR(M"0010001----------001-----1010011")
  val FSGNJX_D           = TypeR(M"0010001----------010-----1010011")
  val FMIN_D             = TypeR(M"0010101----------000-----1010011")
  val FMAX_D             = TypeR(M"0010101----------001-----1010011")
  val FSQRT_D            = TypeR1_RM(M"010110100000-------------1010011")
  val FMV_X_W            = TypeF2I(M"111000000000-----000-----1010011")
  val FCVT_W_D           = TypeF2I_RM(M"110000100000-------------1010011")
  val FCVT_WU_D          = TypeF2I_RM(M"110000100001-------------1010011")
  val FCVT_L_D           = TypeF2I_RM(M"110000100010-------------1010011")
  val FCVT_LU_D          = TypeF2I_RM(M"110000100011-------------1010011")
  val FMV_X_D            = TypeF2I(M"111000100000-----000-----1010011")
  val FCLASS_D           = TypeF2I(M"111000100000-----001-----1010011")
  val FCVT_D_W           = TypeI2F(M"110100100000-------------1010011")
  val FCVT_D_WU          = TypeI2F(M"110100100001-------------1010011")
  val FCVT_D_L           = TypeI2F_RM(M"110100100010-------------1010011")
  val FCVT_D_LU          = TypeI2F_RM(M"110100100011-------------1010011")
  val FMV_W_X            = TypeI2F(M"111100000000-----000-----1010011")
  val FMV_D_X            = TypeI2F(M"111100100000-----000-----1010011")
  val FMADD_D            = TypeR3_RM(M"-----01------------------1000011")
  val FMSUB_D            = TypeR3_RM(M"-----01------------------1000111")
  val FNMSUB_D           = TypeR3_RM(M"-----01------------------1001011")
  val FNMADD_D           = TypeR3_RM(M"-----01------------------1001111")
  val FLE_D              = TypeFCI(M"1010001----------000-----1010011")
  val FLT_D              = TypeFCI(M"1010001----------001-----1010011")
  val FEQ_D              = TypeFCI(M"1010001----------010-----1010011")
  val FCVT_S_D           = TypeR1_RM(M"010000000001-------------1010011")
  val FCVT_D_S           = TypeR1(M"010000100000-------------1010011")


  val FLW                = TypeILQ(M"-----------------010-----0000111")
  val FLD                = TypeILQ(M"-----------------011-----0000111")
  val FSW                = TypeSSQ(M"-----------------010-----0100111")
  val FSD                = TypeSSQ(M"-----------------011-----0100111")
}
