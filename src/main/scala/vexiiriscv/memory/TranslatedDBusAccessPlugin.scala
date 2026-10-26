package vexiiriscv.memory

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.State
import vexiiriscv.misc.PerformanceCounterService

object TranslatedDBusAccessPlugin {
  def storageParameter = MmuStorageParameter(
    levels = List(
      MmuStorageLevel(
        id = 0,
        ways = 2,
        sets = 8
      ),
      MmuStorageLevel(
        id = 1,
        ways = 1,
        sets = 8
      )
    ),
    priority = 2
  )

  def mmuPortParameter = MmuPortParameter(
    readAt = 0,
    hitsAt = 0,
    ctrlAt = 1,
    rspAt = 1
  )
}

class TranslatedDBusAccessPlugin(val withPtwTlb: Boolean = false) extends FiberPlugin with TranslatedDBusAccessService {
  val logic = during setup new Area{
    val dbus = host[DBusAccessService]
    val ats = host.find[AddressTranslationService](_.isShadowMmu)
    val dbusLock = retains(dbus.accessRetainer)
    val withAtsRedo = ats.mayNeedRedo
    val withTlbEnabled = withAtsRedo && withPtwTlb
    val atsStorageLock = withTlbEnabled generate retains(ats.storageLock)
    val atsPortsLock = retains(ats.portsLock)

    awaitBuild()

    val tlb = withTlbEnabled generate new Area {
      val storage = ats.newStorage(TranslatedDBusAccessPlugin.storageParameter, PerformanceCounterService.PTW_TLB_CYCLES)
      atsStorageLock.release()

      val port = ats.newTranslationAccess(
        AddressTranslationPortUsage.IMPLICIT_LOAD,
        TranslatedDBusAccessPlugin.mmuPortParameter,
        storage
      )
    }
    val tlbStorageId = withTlbEnabled.mux(ats.getStorageId(tlb.storage), 0)
    val tlbPort = withTlbEnabled generate tlb.port

    val atsPort = withAtsRedo generate ats.newRefillPort()
    atsPortsLock.release()

    accessRetainer.await()
    if(withTlbEnabled) require(dbusAccesses.count(_.requestGuest) <= 1, "Translated DBus PTW TLB supports one guest-translated client")

    val accessBus = dbusAccesses.nonEmpty generate dbus.newDBusAccess()
    dbusLock.release()

    if (withAtsRedo) {
      atsPort.cmd.valid               := False
      atsPort.cmd.address             := U(0)
      atsPort.cmd.indirect            := True
      atsPort.cmd.forceGuest          := False
      atsPort.cmd.storageEnable       := Bool(withTlbEnabled)
      atsPort.cmd.storageId           := U(tlbStorageId)
      atsPort.cmd.permission.read     := True
      atsPort.cmd.permission.write    := False
      atsPort.cmd.permission.execute  := False
      atsPort.rsp.ready               := False
    }

    if(withTlbEnabled) {
      tlbPort.cmd.valid         := False
      tlbPort.cmd.address       := U(0)
      tlbPort.cmd.load          := True
      tlbPort.cmd.store         := False
      tlbPort.cmd.execute       := False
      tlbPort.cmd.forceGuest    := True
      tlbPort.cmd.forcePhysical := False
      tlbPort.rsp.ready         := False
    }

    val access = dbusAccesses.nonEmpty generate new Area {
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
        val generateTlbPort = generateTransPort && withTlbEnabled
        val IDLE, CMD, RSP = new State
        val ATS = new State
        val TLB_CMD, TLB_RSP, ATS_CMD = generateTlbPort generate new State
        val tcmd = tda.cmd
        val trsp = tda.rsp

        val address = Reg(cloneOf(tcmd.address))
        val size = Reg(cloneOf(tcmd.size))

        val cacheRefill = Reg(Bits(dbus.accessRefillCount bits)) init(0)
        val cacheRefillAny = Reg(Bool()) init(False)

        val cacheRefillSet = cacheRefill.getZero
        val cacheRefillAnySet = False
        cacheRefill    := (cacheRefill | cacheRefillSet) & ~dbus.accessWake
        cacheRefillAny := (cacheRefillAny | cacheRefillAnySet) & !dbus.accessWake.orR

        setEntry(IDLE)

        tcmd.ready := False

        IDLE whenIsActive {
          when (tcmd.valid) {
            address := tcmd.address
            size    := tcmd.size

            val guestCtx = WhenBuilder()
            if(generateTlbPort) guestCtx.when(tcmd.guest) {
              tcmd.ready := True
              goto(TLB_CMD)
            } else if(generateTransPort) guestCtx.when(tcmd.guest) {
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

        if(generateTlbPort) TLB_CMD whenIsActive {
          tlbPort.cmd.valid   := True
          tlbPort.cmd.address := address.resized
          when(tlbPort.cmd.ready) {
            goto(TLB_RSP)
          }
        }

        if(generateTlbPort) TLB_RSP whenIsActive {
          when(tlbPort.rsp.valid) {
            tlbPort.rsp.ready := True
            when(tlbPort.rsp.refill) {
              goto(ATS_CMD)
            } elsewhen(tlbPort.rsp.hazard) {
              goto(TLB_CMD)
            } elsewhen(tlbPort.rsp.pageFault || tlbPort.rsp.accessFault) {
              trsp.valid    := True
              trsp.data     := address.asBits.resized
              trsp.error(1) := tlbPort.rsp.pageFault
              trsp.error(0) := tlbPort.rsp.accessFault
              goto(IDLE)
            } otherwise {
              address := tlbPort.rsp.translated
              goto(CMD)
            }
          }
        }

        if(generateTlbPort) ATS_CMD whenIsActive {
          atsPort.cmd.valid   := True
          atsPort.cmd.address := address.resized
          when(atsPort.cmd.ready) {
            goto(ATS)
          }
        }

        if(generateTransPort) ATS whenIsActive {
          when(atsPort.rsp.valid) {
            atsPort.rsp.ready := True
            /* check permission */
            when (atsPort.rsp.pageFault || atsPort.rsp.accessFault) {
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
}
