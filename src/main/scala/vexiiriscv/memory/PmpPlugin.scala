package vexiiriscv.memory

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{NodeBaseApi, Payload}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamService}
import vexiiriscv.misc.{PipelineBuilderPlugin, PrivilegedPlugin, TrapReason}
import vexiiriscv.riscv.{CSR, Riscv}

import scala.collection.mutable.ArrayBuffer

case class PmpStorageParameter(slots : Int)
case class PmpPortParameter(var napotMatchAt : Int,
                            var napotHitsAt : Int,
                            var torCmpAt : Int,
                            var torHitsAt : Int,
                            var hitsAt : Int,
                            var rspAt : Int){

}

//ref :
// https://github.com/riscv-software-src/riscv-isa-sim/blob/2c67071743d4b55719cee22fdb319df2a0756db7/riscv/mmu.cc#L348
// https://github.com/riscv-software-src/riscv-isa-sim/blob/2c67071743d4b55719cee22fdb319df2a0756db7/riscv/csrs.cc#L183
// https://github.com/riscv-software-src/riscv-isa-sim/blob/2c67071743d4b55719cee22fdb319df2a0756db7/riscv/mmu.cc#L348

case class PmpParam(var pmpSize : Int, var granularity : Int)
class PmpPlugin(p : PmpParam) extends FiberPlugin with PmpService{
  import p._

  override def getPmpNum(): Int = p.pmpSize

  assert(isPow2(granularity))
  val granularityWidth = log2Up(granularity)

  case class PortSpec(stages: Seq[NodeBaseApi],
                      physicalAddress: Payload[UInt],
                      read: NodeBaseApi => Bool,
                      write: NodeBaseApi => Bool,
                      execute: NodeBaseApi => Bool,
//                      usage : AddressTranslationPortUsage,
                      pp: PmpPortParameter,
//                      ss : StorageSpec,
                      rsp : PmpRsp){
    val permStage = stages(0)
    val napotMatchStage = stages(pp.napotMatchAt)
    val napotHitsStage = stages(pp.napotHitsAt)
    val torCmpStage = stages(pp.torCmpAt)
    val torHitsStage = stages(pp.torHitsAt)
    val hitsStage = stages(pp.hitsAt)
    val rspStage  = stages(pp.rspAt)
  }
  val portSpecs = ArrayBuffer[PortSpec]()

//  case class StorageSpec(p: PmpStorageParameter) extends Nameable
//  val storageSpecs = ArrayBuffer[StorageSpec]()

//  override def newStorage(pAny: Any) : Any = {
//    val p = pAny.asInstanceOf[PmpStorageParameter]
//    storageSpecs.addRet(StorageSpec(p))
//    null
//  }

//  override def getStorageId(s: Any): Int = storageSpecs.indexOf(s)
//  override def getStorageIdWidth(): Int = {
//    storageLock.await()
//    log2Up(storageSpecs.size)
//  }

  override def createPmpPort(nodes: Seq[NodeBaseApi],
                             physicalAddress: Payload[UInt],
                             read: NodeBaseApi => Bool,
                             write: NodeBaseApi => Bool,
                             execute: NodeBaseApi => Bool,
                             portSpec: Any,
                             storageSpec: Any): PmpRsp = {
    val pp = portSpec.asInstanceOf[PmpPortParameter]
//    val ss = storageSpec.asInstanceOf[StorageSpec]
    portSpecs.addRet(
      new PortSpec(
        stages          = nodes,
        physicalAddress = physicalAddress,
        read            = read,
        write           = write,
        execute         = execute,
        pp              = pp,
//        ss              = ss,
        rsp             = new PmpRsp()
      )
    ).rsp
  }

  val api = during build new Area{
    val fetchTranslationEnable = Bool()
    val lsuTranslationEnable = Bool()
  }

  val logic = during setup new Area{
    val priv = host[PrivilegedPlugin]
    val csr = host[CsrAccessPlugin]
    val ram = host[CsrRamService]

    val csrLock = retains(csr.csrLock, ram.csrLock)
    val buildBefore = retains(List(host[PipelineBuilderPlugin].elaborationLock))

    awaitBuild()

    case class Cfg() extends Bundle{
      val kind = Bits(2 bits)
      val read, write, execute = Bool()
      val locked = Bool()
    }

    val pRange = Global.PHYSICAL_WIDTH-1 downto granularityWidth
    val entries = for(i <- 0 until pmpSize) yield new Area{
      val address = Reg(UInt(Global.PHYSICAL_WIDTH.get + 2 - granularityWidth bits)) init(0)
      val cfg = Reg(Cfg()).initZero()
      val cfgNext = CombInit(cfg)
      when(!cfg.locked) {
        cfg := cfgNext
        cfg.write clearWhen (!cfgNext.read)
      }


      import cfg._

      val isNapot = RegNext(kind(1))  //TODO
      val isTor = RegNext(kind === 1)  //TODO

      val napot = RegNext(Napot(address.asBits).orMask(!kind(0)))
      val mask = napot

      val (cfgId, cfgOffset) = Riscv.XLEN.get match{
        case 32 => (i/4, i%4*8)
        case 64 => (i/8*2, i%8/8)
      }

      csr.writeCancel(CSR.PMPADDR + i, locked)
      csr.readWrite(address, CSR.PMPADDR + i, Math.max(0, granularityWidth - 2))
      csr.read(CSR.PMPCFG + cfgId,  0+cfgOffset -> read, 1+cfgOffset -> write, 2+cfgOffset -> execute, 3+cfgOffset -> kind, 7+cfgOffset -> locked)
      csr.write(CSR.PMPCFG + cfgId,  0+cfgOffset -> cfgNext.read, 1+cfgOffset -> cfgNext.write, 2+cfgOffset -> cfgNext.execute, 3+cfgOffset -> cfgNext.kind, 7+cfgOffset -> cfgNext.locked)
      //TODO use ram
    }

    csr.onDecode(CsrListFilter((0 to 63).map(_ + CSR.PMPADDR) ++ (0 to 15).filter(i => Riscv.XLEN.get == 32 || (i % 2) == 0).map(_ + CSR.PMPCFG))) {
      when(csr.bus.decode.write) {
        csr.bus.decode.doTrap(TrapReason.NEXT)
      }
    }

    assert(Global.HART_COUNT.get == 1)

    csrLock.release()
    portsLock.await()

    val isMachine = priv.isMachine(0)
    val ports = for(ps <- portSpecs) yield new Composite(ps.rsp, "logic", false){
      import ps._
      val torCmpAddress = torCmpStage(ps.physicalAddress) >> granularityWidth
      val TOR_SMALLER = new Array[Payload[Bool]](p.pmpSize)
      val onEntries = for((e, id) <- entries.zipWithIndex) yield new Area{
        val NAPOT_MATCH = napotMatchStage.insert(((e.address << granularityWidth)(pRange) ^ napotMatchStage(ps.physicalAddress)(pRange)).asBits & e.mask.resized)
        val NAPOT_HIT = napotHitsStage.insert(napotHitsStage(NAPOT_MATCH) === 0)
        val TOR_BIGGER = torCmpStage.insert(e.address > torCmpAddress)
        val TOR_HIT = { import torHitsStage._ ; insert(TOR_BIGGER && (id != 0).mux(!TOR_SMALLER(id-1), True))}
        val HIT_ANY = { import hitsStage._ ; insert(e.isNapot && NAPOT_HIT || e.isTor && TOR_HIT) }

        val bypass = False // isMachine && !e.locked
        val normalRwx = (!ps.read(permStage) || e.cfg.read) && (!ps.write(permStage) || e.cfg.write) && (!ps.execute(permStage) || e.cfg.execute)
        val PERM_OK = { import permStage._ ; insert(bypass || normalRwx) } //TODO check rv64 fit in

        TOR_SMALLER(id) = TOR_BIGGER
      }

      val onCtrl = new Area{
        import ps.rspStage._

        val hits = Cat(onEntries.map(e => ps.rspStage(e.HIT_ANY)))
        val oh = OHMasking.firstV2(hits)
        val reader = onEntries.reader(oh)

        rsp.ACCESS_FAULT := !isMachine && !(reader(e => ps.rspStage(e.PERM_OK)))
      }
    }


    buildBefore.release()
  }
}