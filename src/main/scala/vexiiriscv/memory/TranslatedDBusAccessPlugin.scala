package vexiiriscv.memory

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.State

class TranslatedDBusAccessPlugin() extends FiberPlugin with TranslatedDBusAccessService {
  override def accessRefillCount: Int = 0
  override def accessWake: Bits = B(0)

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
      atsPort.cmd.valid         := False
      atsPort.cmd.address       := U(0)
      atsPort.cmd.storageEnable := False
      atsPort.cmd.storageId     := U(0)
      atsPort.rsp.ready         := False
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
      tda.rsp.redo.assignDontCare()
      tda.rsp.waitSlot.assignDontCare()
      tda.rsp.waitAny.assignDontCare()
    }

    val fsm = for (tda <- dbusAccesses) yield new StateMachine {
      val generateTransPort = withAtsRedo && tda.requestGuest
      val CMD, RSP = new State
      val ATS = new State
      val tcmd = tda.cmd
      val trsp = tda.rsp
      val size = generateTransPort generate Reg(cloneOf(tcmd.size))

      setEntry(CMD)

      tcmd.ready := False

      CMD whenIsActive {
        when(tcmd.valid) {
          val guestCtx = WhenBuilder()
          if(generateTransPort) guestCtx.when(tcmd.guest) {
            atsPort.cmd.valid   := True
            atsPort.cmd.address := tcmd.address.resized
            when(atsPort.cmd.ready) {
              tcmd.ready  := True
              size        := tcmd.size
              goto(ATS)
            }
          }
          guestCtx.otherwise {
            cmd.valid     := True
            cmd.address   := tcmd.address
            cmd.size      := tcmd.size
            when (cmd.ready) {
              tcmd.ready  := True
              goto(RSP)
            }
          }
        }
      }

      if(generateTransPort) ATS whenIsActive {
        when(atsPort.rsp.valid) {
          /* check permission */
          val exception = atsPort.rsp.pageFault || atsPort.rsp.accessFault
          val permCheck = False
          when (!atsPort.rsp.bypass && (exception || permCheck)) {
            trsp.valid          := True
            trsp.data           := atsPort.rsp.address.asBits.resized
            trsp.error(1)       := True
            trsp.error(0)       := permCheck
            trsp.redo           := False
            trsp.waitSlot       := B(0)
            trsp.waitAny        := False

            atsPort.rsp.ready   := True
            goto(CMD)
          } otherwise {
            cmd.valid     := True
            cmd.address   := atsPort.rsp.address
            cmd.size      := size
            when (cmd.ready) {
              atsPort.rsp.ready := True
              goto(RSP)
            }
          }
        }
      }

      RSP whenIsActive {
        trsp.valid        := rsp.valid
        trsp.data         := rsp.data
        trsp.error(0)     := rsp.error
        trsp.redo         := rsp.redo
        trsp.waitSlot     := rsp.waitSlot
        trsp.waitAny      := rsp.waitAny

        when (rsp.valid) {
          goto(CMD)
        }
      }
    }

  }
}
