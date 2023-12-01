package vexiiriscv.prediction

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv._
import decode._
import fetch._
import schedule._
import riscv._
import Global._
import spinal.lib.logic.{DecodingSpec, Masked}
import vexiiriscv.prediction.Prediction.ALIGNED_JUMPED

class DecodePredictionPlugin(var decodeAt: Int,
                             var jumpAt: Int) extends FiberPlugin{
  lazy val dpp = host[DecodePipelinePlugin]
  lazy val pcp = host[PcService]
  lazy val rp = host[ReschedulePlugin]
  lazy val dp = host[DecoderService]
  buildBefore(dpp.elaborationLock)
  buildBefore(pcp.elaborationLock)
  setupRetain(rp.elaborationLock)

  val logic = during build new Area{
    val age = dpp.getAge(jumpAt, true)
    val pcPort = pcp.createJumpInterface(age, 0, 0)
    val flushPort = rp.newFlushPort(age, 0, true)

    rp.elaborationLock.release()

    val decodeSpec = new Area{
      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
      val jalKeys = List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key))
      val any = new DecodingSpec(Bool()).setDefault(Masked.zero)
      any.addNeeds(branchKeys ++ jalKeys, Masked.one)
    }


    flushPort.setIdle()
    pcPort.setIdle()

    val slots = for (slotId <- (0 until Decode.LANES).reverse) yield new Area {
      val decodeStage = dpp.ctrl(decodeAt).lane(slotId)
      val jumpStage = dpp.ctrl(jumpAt).lane(slotId)


      val IS_ANY = decodeStage.insert(decodeSpec.any.build(decodeStage(Decode.INSTRUCTION), dp.covers()))

      //TODO Make BTB learn on this
      val onJump = new Area{
        import jumpStage._

        val fixIt = up.isValid && ALIGNED_JUMPED && !IS_ANY
        val fixed = RegInit(False) setWhen(fixIt) clearWhen(up.ready || up.cancel)

        when(fixIt) {
          flushPort.valid := True
          flushPort.self := True // that way we don't have to calculate the next PC
          flushPort.hartId := HART_ID
          flushPort.uopId := Decode.UOP_ID //TODO naaaaa not realy good

          pcPort.valid := True
          pcPort.pc := PC
        }
      }
    }

//    val ras = new Area {
//      val mem = new Area {
//        val stack = Mem.fill(rasDepth)(PC)
//        if (GenerationFlags.simulation) {
//          stack.initBigInt(List.fill(stack.wordCount)(BigInt(0)))
//        }
//      }
//      val ptr = new Area {
//        val push = Reg(UInt(log2Up(rasDepth) bits)) init (0)
//        val pop = Reg(UInt(log2Up(rasDepth) bits)) init (rasDepth - 1)
//        val pushIt, popIt = False
//
//        push := push + U(pushIt) - U(popIt)
//        pop := pop + U(pushIt) - U(popIt)
//      }
//      val read = mem.stack.readAsync(ptr.pop)
//      val write = mem.stack.writePort
//      write.valid := ptr.pushIt
//      write.address := ptr.push
//      write.data.assignDontCare()
//
//      //TODO Restore the RAS ptr on reschedules
////      val reschedule = commit.reschedulingPort(onCommit = false)
////      val healPush = rob.readAsyncSingle(RAS_PUSH_PTR, reschedule.robId)
////      val healPop = healPush - 1
////      when(reschedule.valid) {
////        ptr.push := healPush
////        ptr.pop := healPop
////      }
//    }
//
//    val decodePatch = new Area {
//      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
//      val branchDecoder, jalDecoder, jalrDecoder, anyDecoder = new DecodingSpec(Bool()).setDefault(Masked.zero)
//      branchDecoder.addNeeds(branchKeys, Masked.one)
//      jalDecoder.addNeeds(Masked(Rvi.JAL.key), Masked.one)
//      jalrDecoder.addNeeds(Masked(Rvi.JALR.key), Masked.one)
//      anyDecoder.addNeeds(branchKeys ++ List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key)), Masked.one)
//
//
//
//      var rasPushUsed = False
//      val slots = for (slotId <- 0 until Decode.LANES) yield new Area {
//        def inst = Decode.INSTRUCTION
//
//        val decodeStage = dpp.ctrl(decodeAt).lane(slotId)
//        val pcAddStage = dpp.ctrl(pcAddAt).lane(slotId)
//        val pcPredictionStage = dpp.ctrl(pcPredictionAt).lane(slotId)
//        val applyStage = dpp.ctrl(applyAt).lane(slotId)
//
//        val rd = Decode.rfaKeys.get(RD)
//        val rs1 = Decode.rfaKeys.get(RS1)
//        val decode = new Area {
//          import decodeStage._
//
//          val IS_JAL = insert(jalDecoder.build(inst, dp.covers()))
//          val IS_JALR = insert(jalrDecoder.build(inst, dp.covers()))
//          val IS_BRANCH = insert(branchDecoder.build(inst, dp.covers()))
//          val IS_ANY = insert(anyDecoder.build(inst, dp.covers()))
//
//          val imm = IMM(inst)
//          val OFFSET = insert(inst(2).mux(
//            False -> imm.b_sext,
//            True -> imm.j_sext
//          ).as(PC))
//
//          val rdLink = List(1, 5).map(rd.ARCH === _).orR
//          val rs1Link = List(1, 5).map(rs1.ARCH === _).orR
//          val rdEquRs1 = rd.ARCH === rs1.ARCH
//          val RAS_PUSH = insert((IS_JAL || IS_JALR) && rdLink)
//          val RAS_POP = insert(IS_JALR && (!rdLink && rs1Link || rdLink && rs1Link && !rdEquRs1))
//
//          val LAST_SLICE = insert(PC(Fetch.SLICE_RANGE) + Decode.INSTRUCTION_SLICE_COUNT)
//          val CONDITIONAL_PREDICTION = insert(False) //TODO decodeStage(CONDITIONAL_TAKE_IT, slotId)(LAST_SLICE)
//        }
//
//        val pcAdd = new Area {
//          import pcAddStage._
//
//          val slices = Decode.INSTRUCTION_SLICE_COUNT +^ 1
////          val PC_INC = insert(S(PC + (slices << SLICE_RANGE_LOW)))
////          PC_TARGET_PRE_RAS := S(PC) + OFFSET
//        }
//
//        val pcPrediction = new Area {
//
//          import pcPredictionStage._
//
//          val stage = pcPredictionStage
//
//          BAD_RET_PC := RAS_POP && ras.read =/= ALIGNED_BRANCH_PC_NEXT
//          CAN_IMPROVE := !IS_JALR || RAS_POP
//          BRANCHED_PREDICTION := IS_BRANCH && CONDITIONAL_PREDICTION || IS_JAL || IS_JALR
//        }
//
//        val applyIt = new Area {
//          val stage = applyStage
//
//          import applyStage._
//
//          PC_TARGET := PC_TARGET_PRE_RAS
//          when(IS_JALR) {
//            PC_TARGET := S(ras.read)
//          }
//          PC_PREDICTION := BRANCHED_PREDICTION ? stage(PC_TARGET, slotId) otherwise PC_INC
//
//          val badTaken = IS_ANY ? (BRANCHED_PREDICTION =/= ALIGNED_BRANCH_VALID) otherwise ALIGNED_BRANCH_VALID
//          MISSMATCH_PC := badTaken || BAD_RET_PC // ALIGNED_BRANCH_VALID =/= BRANCHED_PREDICTION // || ALIGNED_BRANCH_VALID && ALIGNED_BRANCH_PC_NEXT =/= U(PC_PREDICTION)
//          //val historyPushed = BRANCH_HISTORY_PUSH_VALID && BRANCH_HISTORY_PUSH_SLICE === LAST_SLICE
//          MISSMATCH_HISTORY := False //historyPushed =/= IS_BRANCH || IS_BRANCH && BRANCH_HISTORY_PUSH_VALUE =/= CONDITIONAL_PREDICTION
//          //MISSMATCH_HISTORY Will improve the branch hit rate, but will also reduce the fetch bandwidth in cases it wasn't realy necessary
//
//          MISSMATCH := MISSMATCH_PC || MISSMATCH_HISTORY
//          NEED_CORRECTION := DECODED_MASK && CAN_IMPROVE && MISSMATCH
//          if (flushOnBranch) MISSMATCH setWhen (IS_BRANCH)
//
//          branchContext.keys.BRANCH_SEL := IS_ANY
//          when(NEED_CORRECTION) {
//            branchContext.keys.BRANCH_EARLY.taken := BRANCHED_PREDICTION
//            branchContext.keys.BRANCH_EARLY.pc := U(PC_TARGET)
//          } otherwise {
//            branchContext.keys.BRANCH_EARLY.taken := ALIGNED_BRANCH_VALID
//            branchContext.keys.BRANCH_EARLY.pc := ALIGNED_BRANCH_PC_NEXT
//          }
//
//          when(DISPATCH_MASK && RAS_PUSH) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
//            when(!rasPushUsed) {
//              ras.write.data := U(PC_INC)
//            }
//            rasPushUsed \= True
//            ras.ptr.pushIt := True
//          }
//          when(DISPATCH_MASK && RAS_POP) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
//            ras.ptr.popIt := True
//          }
//          RAS_PUSH_PTR := ras.ptr.push
//        }
//      }
//
//
//      val applyIt = new Area {
//        val stage = applyStage
//
//        import applyStage._
//
//        val slotIds = (0 until DECODE_COUNT)
//        val hit = slotIds.map(stage(NEED_CORRECTION, _)).orR
//        val selOh = OHMasking.first(slotIds.map(stage(NEED_CORRECTION, _)))
//        val applySideEffects = isFireing
//        val firstCycle = RegInit(True) clearWhen (isValid) setWhen (isReady || isFlushed)
//
//        setup.decodeJump.valid := isValid && hit //IsValid instead of applySideEffects to avoid propagating the ready path
//        setup.decodeJump.pc := U(MuxOH(selOh, (0 until DECODE_COUNT).map(stage(PC_PREDICTION, _))))
//
//        setup.historyPush.flush := setup.decodeJump.valid
//
//        ras.ptr.pushIt clearWhen (!applySideEffects)
//        ras.ptr.popIt clearWhen (!applySideEffects)
//
//
//        flushIt(setup.decodeJump.valid && isReady, root = false)
//
//        //WARNING, overloaded(Frontend.DISPATCH_MASK) may not be reconized by some downstream plugins if you move this futher the decoding stage
//        for (slotId <- 0 until Frontend.DECODE_COUNT) {
//          stage(Frontend.DISPATCH_MASK, slotId) := stage(Frontend.DECODED_MASK, slotId) && !stage(0 until slotId)(NEED_CORRECTION).orR
//        }
//
//        for (slotId <- 0 until Frontend.DECODE_COUNT) {
//          val slot = slots(slotId)
//          implicit val _ = StageableOffset(slotId)
//          setup.historyPush.mask(slotId) := isValid && firstCycle && IS_BRANCH && Frontend.DECODED_MASK && !stage(0 until slotId)(NEED_CORRECTION).orR
//          setup.historyPush.taken(slotId) := BRANCHED_PREDICTION
//        }
//      }
//    }
//
//    val onDispatch = new Area {
//      val stage = frontend.pipeline.dispatch
//
//      import stage._
//
//      rob.write(IS_BRANCH, DISPATCH_COUNT, stage(0 until DISPATCH_COUNT)(IS_BRANCH), ROB.ID, isFireing)
//
//      branchContext.dispatchWrite(
//        IS_BRANCH,
//        CONDITIONAL_TAKE_IT
//      )
//    }

  }
}
