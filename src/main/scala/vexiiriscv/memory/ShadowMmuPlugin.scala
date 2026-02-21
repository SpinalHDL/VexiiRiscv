package vexiiriscv.memory

import spinal.core
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv._
import Global._
import spinal.lib.misc.pipeline.{NodeBaseApi, Payload}
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamService}
import vexiiriscv.memory.AddressTranslationPortUsage.{FETCH, LOAD_STORE}
import vexiiriscv.misc.{PerformanceCounterService, PipelineBuilderPlugin, PrivilegedPlugin, TrapReason}
import vexiiriscv.riscv.{CSR, PrivilegeMode}
import vexiiriscv.riscv.Riscv._

import scala.collection.mutable.ArrayBuffer

class ShadowMmuPlugin(var spec : MmuSpec,
                      var physicalWidth : Int,
                      var vmidWidth : Int) extends FiberPlugin with GenericMmuPlugin{
  override def isShadowMmu : Boolean = true

  /* Second stage is always zero-extended */
  def getSignExtension(kind: AddressTranslationPortUsage, rawAddress: UInt) = False

  val api = during build new Area{
    val fetchTranslationEnable = Bool()
    val lsuTranslationEnable = Bool()
  }

  val logic = during setup new Area{
    val priv = host[PrivilegedPlugin]
    val csr = host[CsrAccessPlugin]
    val access = host[TranslatedDBusAccessPlugin]
    val ram = host[CsrRamService]
    val pcs = host.get[PerformanceCounterService]
    val mmu = host[MmuPlugin]

    val csrLock = retains(csr.csrLock, ram.csrLock)
    val accessLock = retains(access.accessRetainer)
    val buildBefore = retains(List(host[PipelineBuilderPlugin].elaborationLock) ++ pcs.map(_.elaborationLock))

    awaitBuild()

    val accessBus = access.newDBusAccess(false)

    accessLock.release()

    def physCap(range : Range) = (range.high min physicalWidth-1) downto range.low

    assert(HART_COUNT.get == 1)
    val hgatp = new Area {
      val (modeOffset, modeWidth, ppnWidth, vmidOffset, vmidWidthMax) = XLEN.get match {
        case 32 => (31, 1, 20, 22, 7) //20 instead of 22 to avoid 34 physical bits
        case 64 => (60, 4, 44, 44, 14)
      }
      assert(vmidWidth <= vmidWidthMax, "vmidWidth is too big")
      val mode = Reg(Bits(modeWidth bits)) init(0)
      val vmid = Reg(Bits(vmidWidth bits)) init(0)
      val ppn =  Reg(UInt(ppnWidth bits))  init(0)
    }

    csr.readWrite(CSR.HGATP, hgatp.vmidOffset -> hgatp.vmid, 0 -> hgatp.ppn)
    csr.read(CSR.HGATP, hgatp.modeOffset -> hgatp.mode)
    csr.allowHostCsr(CSR.HGATP, !priv.logic.harts(0).m.status.tvm || priv.isMachine(0))

    csr.onDecode(CSR.HGATP) {
      csr.bus.decode.doTrap(TrapReason.HFENCE_GMA)
    }

    csr.onWrite(CSR.HGATP, true) {
      val hgatpModeWrite = csr.bus.write.bits(hgatp.modeOffset, hgatp.modeWidth bits)
      when(hgatpModeWrite === 0) {
        hgatp.mode := 0
      } otherwise {
        hgatp.mode := spec.satpMode
      }
    }

    csrLock.release()
    portsLock.await()

    assert(storageSpecs.map(_.p.priority).distinct.size == storageSpecs.size, "MMU storages needs different priorities")
    // Implement the hardware for all the TLB storages
    val tlbGenerateParam = MmuTlbStorageEntryParam(
      checkUser   = false,
      checkGuest  = false
    )
    val storages = for(ss <- storageSpecs) yield new MmuTlbStorage(spec, physicalWidth, tlbGenerateParam, ss)

    assert(HART_COUNT.get == 1)
    val isMachine = priv.isMachine(0)
    val isSupervisor = priv.isSupervisor(0)
    val isUser = priv.isUSer(0)
    val isVirtual = PrivilegeMode.isGuest(priv.getPrivilege(0))
    def mprv = priv.logic.harts(0).m.status.mprv

    api.fetchTranslationEnable := hgatp.mode === spec.satpMode
    api.fetchTranslationEnable clearWhen(!isVirtual)

    api.lsuTranslationEnable := hgatp.mode === spec.satpMode
    when(isMachine) {
      api.lsuTranslationEnable clearWhen (!mprv || priv.logic.harts(0).m.status.mpp === 3 || !priv.logic.harts(0).m.status.mpv)
    } otherwise {
      api.lsuTranslationEnable clearWhen(!isVirtual)
    }

    // Implement the hardware of very MMU ports on their respective pipelines / storages
    val portSpecsSorted = portSpecs.sortBy(_.ss.p.priority).reverse
    val ports = for(ps <- portSpecsSorted) yield new Composite(ps.rsp, "logic", false){
      import ps._

      val storage = storages.find(_.self == ps.ss).get
      val read = for (sl <- storage.sl) yield new Area {
        val readAddress = readStage(ps.req.PRE_ADDRESS)(sl.lineRange)
        for ((way, wayId) <- sl.ways.zipWithIndex) {
          readStage(sl.keys.ENTRIES)(wayId) := way.readAsync(readAddress)
          hitsStage(sl.keys.HITS_PRE_VALID)(wayId) := hitsStage(sl.keys.ENTRIES)(wayId).hit(hitsStage(ps.req.PRE_ADDRESS))
          ctrlStage(sl.keys.HITS)(wayId) := ctrlStage(sl.keys.HITS_PRE_VALID)(wayId) && ctrlStage(sl.keys.ENTRIES)(wayId).valid
        }
      }

      val ctrl = new Area{
        import ctrlStage._

        val hits = Cat(storage.sl.map(s => ctrlStage(s.keys.HITS)))
        val entries = storage.sl.flatMap(s => ctrlStage(s.keys.ENTRIES))
        val hit = hits.orR
        val oh = OHMasking.firstV2(hits)

        def entriesMux[T <: Data](f : MmuTlbStorageEntry => T) : T = OhMux.or(oh, entries.map(f))
        val lineAllowExecute = entriesMux(_.allowExecute)
        val lineAllowRead    = entriesMux(_.allowRead)
        val lineAllowWrite   = entriesMux(_.allowWrite)
        val lineTranslated   = entriesMux(_.physicalAddressFrom(ps.req.PRE_ADDRESS))

        val requireMmuLockup  = CombInit(ps.usage match {
          case LOAD_STORE => api.lsuTranslationEnable || (ps.req.FORCE_GUEST && hgatp.mode === spec.satpMode)
          case FETCH => api.fetchTranslationEnable
        })
        /* Only process request from the guest */
        requireMmuLockup clearWhen(ps.req.FORCE_PHYSICAL)

        import ps.rsp.keys._
        when(requireMmuLockup) {
          val allow_execute = lineAllowExecute
          val allow_read    = lineAllowRead || mmu.logic.status.mxr && lineAllowExecute
          val allow_write   = lineAllowWrite

          HAZARD        := False
          REFILL        := !hit
          TRANSLATED    := lineTranslated
          PAGE_FAULT    := Mux(ps.req.LOAD, !allow_read, False) ||
                           Mux(ps.req.STORE, !allow_write, False) ||
                           Mux(ps.req.EXECUTE, !allow_execute, False)
          ACCESS_FAULT  := False
        } otherwise {
          HAZARD        := False
          REFILL        := False
          TRANSLATED    := ps.req.PRE_ADDRESS.resized
          PAGE_FAULT    := False
          ACCESS_FAULT  := ps.req.PRE_ADDRESS.drop(physicalWidth) =/= 0
        }


        BYPASS_TRANSLATION := !requireMmuLockup
        WAYS_OH       := oh
        (WAYS_PHYSICAL, entries.map(_.physicalAddressFrom(ps.req.PRE_ADDRESS))).zipped.foreach(_ := _)
      }
    }

    // Implement the TLB storage refill FSM
    val refill = new StateMachine{
      val IDLE = new State
      val CMD, RSP, REFILL, DONE = List.fill(spec.levels.size)(new State)

      val busy = !isActive(IDLE)
      val virtual = Reg(UInt(MIXED_WIDTH bits))

      val cacheRefill = Reg(Bits(access.accessRefillCount bits)) init(0)
      val cacheRefillAny = Reg(Bool()) init(False)

      val cacheRefillSet = cacheRefill.getZero
      val cacheRefillAnySet = False
      cacheRefill    := (cacheRefill | cacheRefillSet) & ~access.accessWake
      cacheRefillAny := (cacheRefillAny | cacheRefillAnySet) & !access.accessWake.orR

      setEntry(IDLE)

      val arbiter = StreamArbiterFactory().roundRobin.transactionLock.buildOn(refillPorts.map(_.cmd))
      val portOhReg = Reg(Bits(refillPorts.size bits))
      val storageOhReg = Reg(Bits(storages.size bits))
      val storageEnable = Reg(Bool())

      arbiter.io.output.ready := False
      IDLE whenIsActive {
        when(arbiter.io.output.valid) {
          portOhReg := arbiter.io.chosenOH
          storageOhReg := UIntToOh(arbiter.io.output.storageId)
          storageEnable := arbiter.io.output.storageEnable
          virtual := arbiter.io.output.address
          load.address := (hgatp.ppn @@ spec.levels.last.vpn(arbiter.io.output.address) @@ U(0, log2Up(spec.entryBytes) bits)).resized
          arbiter.io.output.ready := True
          goto(CMD(spec.levels.size - 1))
        }
      }


      val events = pcs map (pcs => new Area {
        val onStorage = for((storage, sel) <- storageSpecs zip storageOhReg.asBools) yield new Area {
          val waiting = pcs.createEventPort(storage.pmuEventId, busy && sel)
        }
      })

      val load = new Area{
        val address = Reg(UInt(PHYSICAL_WIDTH bits))

        def cmd = accessBus.cmd
        val rspUnbuffered = accessBus.rsp
        val rsp = rspUnbuffered.toStream.stage()
        rsp.ready := False
        val readed = rsp.data.subdivideIn(spec.entryBytes*8 bits).read((address >> log2Up(spec.entryBytes)).resized)

        when(rspUnbuffered.valid && rspUnbuffered.redo) {
          cacheRefillSet    := rspUnbuffered.waitSlot
          cacheRefillAnySet := rspUnbuffered.waitAny
        }

        cmd.valid             := False
        cmd.address           := address.resized
        cmd.size              := U(log2Up(spec.entryBytes))

        val flags = readed.resized.as(MmuEntryFlags())
        val leaf = flags.R || flags.X
        val reservedFault = (readed & spec.pteReserved).orR
        val exception = !flags.V || (!flags.R && flags.W) || rsp.error.orR || (!leaf && (flags.D | flags.A | flags.U)) || reservedFault
        val levelToPhysicalAddress = List.fill(spec.levels.size)(UInt(spec.physicalWidth bits))
        val levelException = List.fill(spec.levels.size)(False)
        val nextLevelBase = U(0, PHYSICAL_WIDTH bits)
        for((level, id) <- spec.levels.zipWithIndex) {
          nextLevelBase(physCap(level.physicalRange)) := readed(level.entryRange).asUInt.resized
          levelToPhysicalAddress(id) := 0
          for((e, eId) <- spec.levels.zipWithIndex){
            if(eId < id) {
              levelException(id) setWhen(readed(e.entryRange) =/= 0)
              levelToPhysicalAddress(id)(e.physicalRange) := e.vpn(virtual)
            } else {
              levelToPhysicalAddress(id)(e.physicalRange) := readed(e.entryRange).asUInt
            }
          }
        }
      }

      for (port <- refillPorts; rsp = port.rsp) {
        rsp.valid := False
        rsp.pageFault.assignDontCare()
        rsp.accessFault.assignDontCare()
        rsp.guestFault.assignDontCare()
        rsp.pf.assignDontCare()
        rsp.ae_ptw.assignDontCare()
        rsp.ae_final.assignDontCare()
        rsp.level.assignDontCare()
        rsp.address.assignDontCare()
      }

      refillPorts.map(_.rsp).foreach{o =>
        o.gf  := False
        o.hr  := False
        o.hw  := False
        o.hx  := False

        o.pte.flags := load.flags
        o.pte.ppn := U(load.readed.dropLow(10)).resized
      }

      val fetch = for((level, levelId) <- spec.levels.zipWithIndex) yield new Area{
        val pteFault = load.exception || load.levelException(levelId) || !load.flags.A || !load.flags.U || (levelId == 0).mux(!load.leaf, False)
        val pteReadError = load.rsp.error.orR
        val leafAccessFault = load.levelToPhysicalAddress(levelId).drop(physicalWidth) =/= 0 //levelToPhysicalAddress is used to emit fault when the final translated address it outside the range of the physical addresses
        val pageFault = !pteReadError && pteFault
        val accessFault = pteReadError || !pteFault && leafAccessFault

        CMD(levelId) whenIsActive{
          when(cacheRefill === 0 && cacheRefillAny === False) {
            load.cmd.valid := True
            when(load.cmd.ready) {
              goto(RSP(levelId))
            }
          }
        }

        RSP(levelId) whenIsActive{
          if(levelId == 0) load.exception setWhen(!load.leaf)
          when(load.rsp.valid){
            when(load.rsp.redo){
              load.rsp.ready := True
              goto(CMD(levelId))
            } otherwise {
              levelId match {
                case 0 => {
                  when(!storageEnable || load.exception) {
                    goto(DONE(levelId))
                  } otherwise {
                    goto(REFILL(levelId))
                  }
                }
                case _ => {
                  when(load.exception) {
                    goto(DONE(levelId))
                  } elsewhen(load.leaf) {
                    when(!storageEnable) {
                      goto(DONE(levelId))
                    } otherwise {
                      goto(REFILL(levelId))
                    }
                  } otherwise {
                    val targetLevelId = levelId - 1
                    val targetLevel = spec.levels(targetLevelId)
                    load.address := load.nextLevelBase
                    load.address(log2Up(spec.entryBytes), targetLevel.physicalWidth bits) := targetLevel.vpn(virtual)
                    load.rsp.ready := True
                    goto(CMD(targetLevelId))
                  }
                }
              }
            }
          }
        }

        REFILL(levelId) whenIsActive {
          for((storage, sid) <- storages.zipWithIndex){
            val storageLevelId = storage.self.p.levels.filter(_.id <= levelId).map(_.id).max
            val storageLevel = storage.sl.find(_.slp.id == storageLevelId).get
            val specLevel = storageLevel.level

            val sel = storageOhReg(sid)
            storageLevel.write.mask                 := UIntToOh(storageLevel.allocId).andMask(sel).resized
            storageLevel.write.address              := virtual(storageLevel.lineRange)
            storageLevel.write.data.valid           := True
            storageLevel.write.data.virtualAddress  := virtual(specLevel.virtualOffset + log2Up(storageLevel.slp.sets), widthOf(storageLevel.write.data.virtualAddress) bits)
            storageLevel.write.data.physicalAddress := (load.levelToPhysicalAddress(levelId) >> specLevel.virtualOffset).resized
            storageLevel.write.data.allowRead       := load.flags.R
            storageLevel.write.data.allowWrite      := load.flags.W && load.flags.D
            storageLevel.write.data.allowExecute    := load.flags.X

            storageLevel.allocId.increment()
          }
          goto(DONE(levelId))
        }

        DONE(levelId) whenIsActive{
          refillPorts.onMask(portOhReg){port =>
            port.rsp.valid := True
            load.rsp.ready := port.rsp.ready

            when(port.rsp.ready) {
              goto(IDLE)
            }
          }

          refillPorts.map(_.rsp).foreach { o =>
            val translationFault = pteFault || pteReadError || leafAccessFault
            val translatedAddress = load.levelToPhysicalAddress(levelId)
            translatedAddress(0, level.virtualOffset bits) := virtual.resize(level.virtualOffset)

            /* TODO: guestFault */
            o.pageFault   := pageFault
            o.accessFault := accessFault
            o.pf          := pageFault
            o.ae_ptw      := accessFault && !load.leaf
            o.ae_final    := accessFault && load.leaf //Note so sure
            o.level       := spec.levels.size - 1 - levelId
            /* TODO: error fill */
            o.address     := Mux(translationFault, load.address, translatedAddress.resized)
          }
        }
      }
    }

    //Assume no mmu access are done to the given hart while being invalidated
    val invalidate = new Area{
      val arbiter = StreamArbiterFactory().roundRobin.transactionLock.buildOn(invalidationPorts.map(_.cmd))
      val depthMax = storageSpecs.map(_.p.levels.map(_.sets).max).max
      val counter = Reg(UInt(log2Up(depthMax) bits))
      val busy = RegInit(False)

      arbiter.io.output.ready := False
      when(!busy){
        counter := 0
        when (arbiter.io.output.valid) {
          busy := True
        }
      } otherwise {
        assert(HART_COUNT.get == 1)
        for (storage <- storages;
             sl <- storage.sl) {
          sl.write.mask := (default -> true)
          sl.write.address := counter.resized
          sl.write.data.valid := False
        }
        counter := counter + 1
        when(counter.andR){
          busy := False
          arbiter.io.output.ready := True
        }
      }
    }

    buildBefore.release()
  }
}
