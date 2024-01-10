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
import vexiiriscv.execute.{CsrAccessPlugin, CsrRamService}
import vexiiriscv.memory.AddressTranslationPortUsage.LOAD_STORE
import vexiiriscv.misc.{PipelineBuilderPlugin, PrivilegedPlugin}
import vexiiriscv.riscv.CSR
import vexiiriscv.riscv.Riscv._

import scala.collection.mutable.ArrayBuffer

case class MmuStorageLevel(id : Int,
                           ways : Int,
                           depth : Int){
  assert(isPow2(depth))
}

case class MmuStorageParameter(levels : Seq[MmuStorageLevel],
                               priority : Int)

case class MmuPortParameter(var readAt : Int,
                            var hitsAt : Int,
                            var ctrlAt : Int,
                            var rspAt : Int){

}


case class MmuSpec(levels : Seq[MmuLevel],
                   entryBytes : Int,
                   virtualWidth : Int,
                   physicalWidth : Int,
                   satpMode : Int)

case class MmuLevel(virtualWidth : Int,
                    physicalWidth : Int,
                    virtualOffset : Int,
                    physicalOffset : Int,
                    entryOffset : Int){
  def vpn(address : UInt) : UInt = address(virtualRange)

  val virtualRange  = virtualOffset  + virtualWidth  -1 downto virtualOffset
  val entryRange    = entryOffset    + physicalWidth -1 downto entryOffset
  val physicalRange = physicalOffset + physicalWidth -1 downto physicalOffset
}

object MmuSpec{
  val sv32 = MmuSpec(
    levels     = List(
      MmuLevel(virtualWidth = 10, physicalWidth = 10, virtualOffset =  12, physicalOffset = 12, entryOffset =  10),
      MmuLevel(virtualWidth = 10, physicalWidth = 10, virtualOffset =  22, physicalOffset = 22, entryOffset =  20) //Avoid 34 bits physical address
    ),
    entryBytes = 4,
    virtualWidth   = 32,
    physicalWidth  = 32,
    satpMode   = 1
  )
  val sv39 = MmuSpec(
    levels     = List(
      MmuLevel(virtualWidth = 9, physicalWidth = 9 , virtualOffset =  12, physicalOffset = 12, entryOffset =  10),
      MmuLevel(virtualWidth = 9, physicalWidth = 9 , virtualOffset =  21, physicalOffset = 21, entryOffset =  19),
      MmuLevel(virtualWidth = 9, physicalWidth = 26, virtualOffset =  30, physicalOffset = 30, entryOffset =  28)
    ),
    entryBytes = 8,
    virtualWidth   = 39,
    physicalWidth  = 56,
    satpMode   = 8
  )
}

class MmuPlugin(var spec : MmuSpec,
                var physicalWidth : Int,
                var ioRange : UInt => Bool,
                var fetchRange : UInt => Bool) extends FiberPlugin with AddressTranslationService{


  override def mayNeedRedo: Boolean = true

  case class PortSpec(stages: Seq[NodeBaseApi],
                      preAddress: Payload[UInt],
                      allowRefill : Payload[Bool],
                      usage : AddressTranslationPortUsage,
                      pp: MmuPortParameter,
                      ss : StorageSpec,
                      rsp : AddressTranslationRsp){
    val readStage = stages(pp.readAt)
    val hitsStage = stages(pp.hitsAt)
    val ctrlStage = stages(pp.ctrlAt)
    val rspStage  = stages(pp.ctrlAt)
  }
  val portSpecs = ArrayBuffer[PortSpec]()

  case class StorageSpec(p: MmuStorageParameter) extends Nameable
  val storageSpecs = ArrayBuffer[StorageSpec]()

  override def newStorage(pAny: Any) : Any = {
    val p = pAny.asInstanceOf[MmuStorageParameter]
    storageSpecs.addRet(StorageSpec(p))
  }

  override def getStorageId(s: Any): Int = storageSpecs.indexOf(s)
  override def getStorageIdWidth(): Int = {
    storageLock.await()
    log2Up(storageSpecs.size)
  }

  override def newTranslationPort(stages: Seq[NodeBaseApi],
                                  preAddress: Payload[UInt],
                                  allowRefill : Payload[Bool],
                                  usage : AddressTranslationPortUsage,
                                  portSpec: Any,
                                  storageSpec: Any) = {
    val pp = portSpec.asInstanceOf[MmuPortParameter]
    val ss = storageSpec.asInstanceOf[StorageSpec]
    portSpecs.addRet(
      new PortSpec(
        stages      = stages,
        preAddress  = preAddress,
        allowRefill = allowRefill,
        usage       = usage,
        pp          = pp,
        ss          = ss,
        rsp         = new AddressTranslationRsp(this, 1 /*, stages(pp.rspAt)*/, ss.p.levels.map(_.ways).sum)
      )
    ).rsp
  }

  val logic = during setup new Area{
    val priv = host[PrivilegedPlugin]
    val csr = host[CsrAccessPlugin]
    val access = host[DBusAccessService]
    val ram = host[CsrRamService]
//    val fetch = host[FetchPlugin]


    val csrLock = retains(csr.csrLock, ram.csrLock)
    val accessLock = retains(access.accessRetainer)
    val buildBefore = retains(host[PipelineBuilderPlugin].elaborationLock)


    awaitBuild()

    PHYSICAL_WIDTH.set(physicalWidth)
    VIRTUAL_WIDTH.set(spec.virtualWidth)
    MIXED_WIDTH.set(VIRTUAL_WIDTH.get + (VIRTUAL_WIDTH < XLEN).toInt)
    PC_WIDTH.set(MIXED_WIDTH)
    TVAL_WIDTH.set(MIXED_WIDTH)
    assert(VIRTUAL_WIDTH.get == XLEN.get || XLEN.get > VIRTUAL_WIDTH.get && VIRTUAL_WIDTH.get > physicalWidth)

    val accessBus = access.newDBusAccess()
    val invalidatePort = FlowCmdRsp().setIdleAll()

    accessLock.release()



    val ALLOW_REFILL = Payload(Bool())
    def physCap(range : Range) = (range.high min physicalWidth-1) downto range.low

    case class StorageEntry(levelId : Int, depth : Int) extends Bundle {
      val vw = spec.levels.drop(levelId).map(_.virtualWidth).sum
      val pw = spec.levels.drop(levelId).map(_.physicalWidth).sum-(spec.physicalWidth-physicalWidth)
      val valid = Bool()
      val virtualAddress  = UInt(vw-log2Up(depth) bits)
      val physicalAddress = UInt(pw bits)
      val allowRead, allowWrite, allowExecute, allowUser = Bool

      def hit(address : UInt) = /*valid && */virtualAddress === address(spec.levels(levelId).virtualOffset + log2Up(depth), vw - log2Up(depth) bits)
      def physicalAddressFrom(address : UInt) = physicalAddress @@ address(0, spec.levels(levelId).physicalOffset bits)
    }

    assert(HART_COUNT.get == 1)
    val satp = new Area {
      val (modeOffset, modeWidth, ppnWidth) = XLEN.get match {
        case 32 => (31, 1, 20) //20 instead of 22 to avoid 34 physical bits
        case 64 => (60, 4, 44)
      }
      val mode = Reg(Bits(modeWidth bits)) init(0)
      //val asid = Reg(Bits(9 bits))         init(0)
      val ppn =  Reg(UInt(ppnWidth bits))  init(0)
    }
    val status = new Area{
      val mxr  = RegInit(False)
      val sum  = RegInit(False)
      val mprv = RegInit(False) clearWhen(priv.hart(0).xretAwayFromMachine)
    }

    for(offset <- List(CSR.MSTATUS, CSR.SSTATUS)) csr.readWrite(offset, 19 -> status.mxr, 18 -> status.sum)
    csr.readWrite(CSR.MSTATUS, 17 -> status.mprv)

    csr.readWrite(CSR.SATP, satp.modeOffset -> satp.mode/*, 22 -> satp.asid*/, 0 -> satp.ppn)
    val satpModeWrite = csr.onWriteBits(satp.modeOffset, satp.modeWidth bits)
    csr.writeCancel(CSR.SATP, satpModeWrite =/= 0 && satpModeWrite =/= spec.satpMode)
    //    csr.readWriteRam(CSR.SATP) not suported by writeCancel

    //TODO !!!! MISS SPEC : Changes to the sstatus fields SUM and MXR take effect immediately, without the need to execute an SFENCE.VMA instruction.
    csr.onDecode(CSR.SATP){
      csr.onDecodeFlushPipeline()
      invalidatePort.cmd.valid := True
    }

    csrLock.release()


    elaborationLock.await()

    assert(storageSpecs.map(_.p.priority).distinct.size == storageSpecs.size, "MMU storages needs different priorities")
    val storages = for(ss <- storageSpecs) yield new Composite(ss, "logic", false){
      val sl = for(e <- ss.p.levels) yield new Area{
        val slp = e
        val level = spec.levels(slp.id)
        def newEntry() = StorageEntry(slp.id, slp.depth)
        val ways = List.fill(slp.ways)(Mem.fill(slp.depth)(newEntry()))
        val lineRange = level.virtualRange.low + log2Up(slp.depth) -1 downto level.virtualRange.low

        val write = new Area{
          val mask    = Bits(slp.ways bits)
          val address = UInt(log2Up(slp.depth) bits)
          val data    = newEntry()

          mask := 0
          address.assignDontCare()
          data.assignDontCare()

          for((way, sel) <- (ways, mask.asBools).zipped){
            way.write(address, data, sel)
          }
        }
        val allocId = Counter(slp.ways)

        val keys = new Area {
          setName(s"MMU_L${e.id}")
          val ENTRIES = Payload(Vec.fill(slp.ways)(newEntry()))
          val HITS_PRE_VALID = Payload(Bits(slp.ways bits))
          val HITS = Payload(Bits(slp.ways bits))
        }
      }
    }


    val portSpecsSorted = portSpecs.sortBy(_.ss.p.priority).reverse
    val ports = for(ps <- portSpecsSorted) yield new Composite(ps.rsp, "logic", false){
      import ps._

      val storage = storages.find(_.self == ps.ss).get

//TODO
//      readStage(ALLOW_REFILL) := (if(allowRefill != null) readStage(allowRefill) else True)
//      val allowRefillBypass = for(stageId <- pp.readAt to pp.ctrlAt) yield new Area{
//        val stage = ps.stages(stageId)
//        val reg = RegInit(True)
//        stage.overloaded(ALLOW_REFILL) := stage(ALLOW_REFILL) && !storage.refillOngoing && reg
//        reg := stage.overloaded(ALLOW_REFILL)
//        when(stage.isRemoved || !stage.isStuck){
//          reg := True
//        }
//      }

      val read = for (sl <- storage.sl) yield new Area {
        val readAddress = readStage(ps.preAddress)(sl.lineRange)
        for ((way, wayId) <- sl.ways.zipWithIndex) {
          readStage(sl.keys.ENTRIES)(wayId) := way.readAsync(readAddress)
          hitsStage(sl.keys.HITS_PRE_VALID)(wayId) := hitsStage(sl.keys.ENTRIES)(wayId).hit(hitsStage(ps.preAddress))
          ctrlStage(sl.keys.HITS)(wayId) := ctrlStage(sl.keys.HITS_PRE_VALID)(wayId) && ctrlStage(sl.keys.ENTRIES)(wayId).valid
        }
      }


      val ctrl = new Area{
        import ctrlStage._

        val hits = Cat(storage.sl.map(s => ctrlStage(s.keys.HITS)))
        val entries = storage.sl.flatMap(s => ctrlStage(s.keys.ENTRIES))
        val hit = hits.orR
        val oh = OHMasking.firstV2(hits)

        def entriesMux[T <: Data](f : StorageEntry => T) : T = OhMux.or(oh, entries.map(f))
        val lineAllowExecute = entriesMux(_.allowExecute)
        val lineAllowRead    = entriesMux(_.allowRead)
        val lineAllowWrite   = entriesMux(_.allowWrite)
        val lineAllowUser    = entriesMux(_.allowUser)
        val lineTranslated   = entriesMux(_.physicalAddressFrom(ps.preAddress))

        val requireMmuLockup  = satp.mode === spec.satpMode

        assert(HART_COUNT.get == 1)
        val isMachine = priv.getPrivilege(0) === U"11"
        val isSupervisor = priv.getPrivilege(0) === U"01"
        val isUser = priv.getPrivilege(0) === U"00"
        requireMmuLockup clearWhen(!status.mprv && isMachine)
        when(isMachine) {
          if (ps.usage == LOAD_STORE) {
            requireMmuLockup clearWhen (!status.mprv || isMachine)
          } else {
            requireMmuLockup := False
          }
        }

        import ps.rsp.keys._
        IO := ioRange(TRANSLATED)
        when(requireMmuLockup) {
          REDO          := !hit
          TRANSLATED    := lineTranslated
          ALLOW_EXECUTE := lineAllowExecute && !(lineAllowUser && isSupervisor)
          ALLOW_READ    := lineAllowRead || status.mxr && lineAllowExecute
          ALLOW_WRITE   := lineAllowWrite
          PAGE_FAULT    := lineAllowUser && isSupervisor && !status.sum || !lineAllowUser && isUser
          ACCESS_FAULT  := False
        } otherwise {
          REDO          := False
          TRANSLATED    := ps.preAddress.resized
          ALLOW_EXECUTE := True
          ALLOW_READ    := True
          ALLOW_WRITE   := True
          PAGE_FAULT    := False
          ACCESS_FAULT  := ps.preAddress.drop(physicalWidth) =/= 0
        }

        ALLOW_EXECUTE clearWhen(!fetchRange(TRANSLATED))

        BYPASS_TRANSLATION := !requireMmuLockup
        WAYS_OH       := oh
        (WAYS_PHYSICAL, entries.map(_.physicalAddressFrom(ps.preAddress))).zipped.foreach(_ := _)
      }
    }


    val refill = new StateMachine{
      val IDLE = new State
      val CMD, RSP = List.fill(spec.levels.size)(new State)

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

      arbiter.io.output.ready := False
      IDLE whenIsActive {
        when(arbiter.io.output.valid) {
          portOhReg := arbiter.io.chosenOH
          storageOhReg := OHToUInt(arbiter.io.output.storageId)
          virtual := arbiter.io.output.address
          load.address := (satp.ppn @@ spec.levels.last.vpn(arbiter.io.output.address) @@ U(0, log2Up(spec.entryBytes) bits)).resized
          arbiter.io.output.ready := True
          goto(CMD(spec.levels.size - 1))
        }
      }

      val load = new Area{
        val address = Reg(UInt(PHYSICAL_WIDTH bits))

        def cmd = accessBus.cmd
        val rspUnbuffered = accessBus.rsp
        val rsp = rspUnbuffered.stage()
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
        val exception = !flags.V || (!flags.R && flags.W) || rsp.error || (!leaf && (flags.D | flags.A | flags.U))
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
      }
      val fetch = for((level, levelId) <- spec.levels.zipWithIndex) yield new Area{
        val pageFault = load.exception || load.levelException(levelId) || !load.flags.A
        val accessFault = !pageFault && load.levelToPhysicalAddress(levelId).drop(physicalWidth) =/= 0

        def doneLogic() : Unit = {
          for(storage <- storages){
            val storageLevelId = storage.self.p.levels.filter(_.id <= levelId).map(_.id).max
            val storageLevel = storage.sl.find(_.slp.id == storageLevelId).get
            val specLevel = storageLevel.level

            val sel = (storageOhReg.asBools, ports).zipped.toList.filter(_._2.storage == storage).map(_._1).orR
            storageLevel.write.mask                 := UIntToOh(storageLevel.allocId).andMask(sel)
            storageLevel.write.address              := virtual(storageLevel.lineRange)
            storageLevel.write.data.valid           := True
            storageLevel.write.data.virtualAddress  := virtual(specLevel.virtualOffset + log2Up(storageLevel.slp.depth), widthOf(storageLevel.write.data.virtualAddress) bits)
            storageLevel.write.data.physicalAddress := (load.levelToPhysicalAddress(levelId) >> specLevel.virtualOffset).resized
            storageLevel.write.data.allowRead       := load.flags.R
            storageLevel.write.data.allowWrite      := load.flags.W && load.flags.D
            storageLevel.write.data.allowExecute    := load.flags.X
            storageLevel.write.data.allowUser       := load.flags.U
            storageLevel.allocId.increment()

            when(pageFault || accessFault){
              storageLevel.write.mask := 0
            }

            refillPorts.onMask(portOhReg){port =>
              port.rsp.valid := sel
              port.rsp.pageFault := pageFault
              port.rsp.accessFault := accessFault
            }
          }

          goto(IDLE)
        }

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
              goto(CMD(levelId))
            } otherwise {
              levelId match {
                case 0 => doneLogic
                case _ => {
                  when(load.leaf || load.exception) {
                    doneLogic
                  } otherwise {
                    val targetLevelId = levelId - 1
                    val targetLevel = spec.levels(targetLevelId)
                    load.address := load.nextLevelBase
                    load.address(log2Up(spec.entryBytes), targetLevel.physicalWidth bits) := targetLevel.vpn(virtual)
                    goto(CMD(targetLevelId))
                  }
                }
              }
            }
          }
        }
      }
    }
//TODO
//    val invalidate = new Area{
//      val requested = RegInit(True) setWhen(invalidatePort.cmd.valid)
//      val canStart = True
//      val depthMax = storageSpecs.map(_.p.levels.map(_.depth).max).max
//      val counter = Reg(UInt(log2Up(depthMax)+1 bits)) init(0)
//      val done = counter.msb
//      when(!done){
//        refill.portsRequest := False
//        counter := counter + 1
//        for(storage <- storages;
//            sl <- storage.sl){
//          sl.write.mask := (default -> true)
//          sl.write.address := counter.resized
//          sl.write.data.valid := False
//        }
//      }
//
//      fetch.getStage(0).haltIt(!done || requested)
//
//      when(requested && canStart){
//        counter := 0
//        requested := False
//        refill.portsRequest := False
//      }
//
//      when(refill.busy || getServicesOf[PostCommitBusy].map(_.postCommitBusy).orR){
//        canStart := False
//      }
//
//      invalidatePort.rsp.valid setWhen(done.rise(False))
//    }
//    fetch.release()

//    core.fiber.hardFork(refill.build())

    buildBefore.release()
  }
}


case class MmuEntryFlags() extends Bundle{
  val V, R, W ,X, U, G, A, D = Bool()
}