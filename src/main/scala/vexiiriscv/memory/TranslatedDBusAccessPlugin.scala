package vexiiriscv.memory

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.State

class TranslatedDBusAccessPlugin() extends FiberPlugin with TranslatedDBusAccessService {
  val logic = during setup new Area{
    val access = host[DBusAccessService]
    val ats = host.find[AddressTranslationService](_.isShadowMmu)
    val accessLock = retains(access.accessRetainer)
    val withAtsRedo = ats.mayNeedRedo
    val atsPortsLock = retains(ats.portsLock)

    awaitBuild()

    val accessBus = access.newDBusAccess()
    accessLock.release()

    val atsPort = withAtsRedo generate ats.newRefillPort()
    atsPortsLock.release()

    accessRetainer.await()

    if (withAtsRedo) {
      atsPort.cmd.valid               := False
      atsPort.cmd.address             := U(0)
      atsPort.cmd.indirect            := True
      atsPort.cmd.storageEnable       := False
      atsPort.cmd.storageId           := U(0)
      atsPort.cmd.permission.read     := True
      atsPort.cmd.permission.write    := False
      atsPort.cmd.permission.execute  := False
      atsPort.rsp.ready               := False
    }

    val cmd = accessBus.cmd
    val rsp = accessBus.rsp

    cmd.valid     := False
    cmd.address   := U(0)
    cmd.size      := U(0)

    for (tda <- dbusAccesses) {
      tda.rsp.valid := False
      tda.rsp.error := B(0)
      tda.rsp.data.assignDontCare()
    }

    val fsm = for (tda <- dbusAccesses) yield new StateMachine {
      val generateTransPort = withAtsRedo && tda.requestGuest
      val IDLE, CMD, RSP = new State
      val ATS = new State
      val tcmd = tda.cmd
      val trsp = tda.rsp

      val address = Reg(cloneOf(tcmd.address))
      val size = Reg(cloneOf(tcmd.size))

      val cacheRefill = Reg(Bits(access.accessRefillCount bits)) init(0)
      val cacheRefillAny = Reg(Bool()) init(False)

      val cacheRefillSet = cacheRefill.getZero
      val cacheRefillAnySet = False
      cacheRefill    := (cacheRefill | cacheRefillSet) & ~access.accessWake
      cacheRefillAny := (cacheRefillAny | cacheRefillAnySet) & !access.accessWake.orR

      setEntry(IDLE)

      tcmd.ready := False

      IDLE whenIsActive {
        when (tcmd.valid) {
          address := tcmd.address
          size    := tcmd.size

          val guestCtx = WhenBuilder()
          if(generateTransPort) guestCtx.when(tcmd.guest) {
            atsPort.cmd.valid   := True
            atsPort.cmd.address := tcmd.address.resized
            when(atsPort.cmd.ready) {
              tcmd.ready  := True
              goto(ATS)
            }
          }
          guestCtx.otherwise {
            tcmd.ready  := True
            goto(CMD)
          }
        }
      }

      if(generateTransPort) ATS whenIsActive {
        when(atsPort.rsp.valid) {
          atsPort.rsp.ready := True
          /* check permission */
          when (!atsPort.rsp.bypass && atsPort.rsp.pageFault || atsPort.rsp.accessFault) {
            trsp.valid          := True
            trsp.data           := atsPort.rsp.address.asBits.resized
            trsp.error(1)       := atsPort.rsp.pageFault
            trsp.error(0)       := atsPort.rsp.accessFault
            goto(IDLE)
          } otherwise {
            address             := atsPort.rsp.address
            goto(CMD)
          }
        }
      }

      CMD whenIsActive {
        when(cacheRefill === 0 && !cacheRefillAny) {
          cmd.valid     := True
          cmd.address   := address
          cmd.size      := size
          when (cmd.ready) {
            goto(RSP)
          }
        }
      }

      RSP whenIsActive {
        when (rsp.valid) {
          when (rsp.redo) {
            cacheRefillSet    := rsp.waitSlot
            cacheRefillAnySet := rsp.waitAny
            goto(CMD)
          } otherwise {
            trsp.valid        := rsp.valid
            trsp.data         := rsp.data
            trsp.error(0)     := rsp.error
            goto(IDLE)
          }
        }
      }
    }

  }
}
