package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.misc.TrapService
import vexiiriscv.riscv.CSR

case class CachelessBusParam(addressWidth : Int, dataWidth : Int, idCount : Int, cmdPersistence : Boolean){
  val idWidth = log2Up(idCount)
}

case class CachelessCmd(p : CachelessBusParam) extends Bundle{
  val id = UInt(p.idWidth bits)
  val address = UInt(p.addressWidth bits)
}

case class CachelessRsp(p : CachelessBusParam, withId : Boolean = true) extends Bundle{
  val id = withId generate UInt(p.idWidth bits)
  val error = Bool()
  val word  = Bits(p.dataWidth bits)
}

case class CachelessBus(p : CachelessBusParam) extends Bundle with IMasterSlave {
  var cmd = Stream(CachelessCmd(p))
  var rsp = Flow(CachelessRsp(p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

object FetchCachelessPlugin{
  val ID_WIDTH = blocking[Int]
  val ID = blocking[Int]
}

class FetchCachelessPlugin(var wordWidth : Int,
                           var forkAt : Int = 0,
                           var joinAt : Int = 1,
                           var cmdForkPersistence : Boolean = true) extends FiberPlugin{

  val logic = during setup new Area{
    val pp = host[FetchPipelinePlugin]
    val ts = host[TrapService]
    val buildBefore = retains(pp.elaborationLock)
    val trapLock =  ts.trapLock()
    awaitBuild()

    Fetch.WORD_WIDTH.set(wordWidth)

    val trapPort = ts.newTrap(pp.getAge(joinAt), 0)
    trapLock.release()

    val idCount = joinAt - forkAt + 1
    val p = CachelessBusParam(MIXED_WIDTH, Fetch.WORD_WIDTH, idCount, false)
    val bus = master(CachelessBus(p))

    val BUFFER_ID = Payload(UInt(log2Up(idCount) bits))

    val buffer = new Area{
      val reserveId = Counter(idCount)
      val inflight = Vec.fill(idCount)(RegInit(False))
      val words = Mem.fill(idCount)(CachelessRsp(p, false))
      val write = words.writePort()
      val reservedHits = for (ctrlId <- forkAt+1 to joinAt; ctrl = pp.fetch(ctrlId)) yield {
        ctrl.isValid && ctrl(BUFFER_ID) === reserveId
      }
      val full = CombInit(reservedHits.orR || inflight.read(reserveId)) //TODO that's one cycle late, can use sort of ahead value

      when(bus.cmd.fire) {
        inflight(reserveId) := True
      }

      write.valid := False
      write.address := bus.rsp.id
      write.data.assignSomeByName(bus.rsp.payload)
      when(bus.rsp.valid) {
        write.valid := True
        inflight(bus.rsp.id) := False
      }
    }

    val fork = new pp.Fetch(forkAt){
      val fresh = (forkAt == 0).option(host[PcPlugin].forcedSpawn())
      val cmdFork = forkStream(fresh)
      bus.cmd.arbitrationFrom(cmdFork.haltWhen(buffer.full))
      bus.cmd.id := buffer.reserveId
      bus.cmd.address := Fetch.WORD_PC
      bus.cmd.address(Fetch.SLICE_RANGE) := 0

      BUFFER_ID := buffer.reserveId

      when(up.isMoving) {
        buffer.reserveId.increment()
      }
    }

    val join = new pp.Fetch(joinAt){
      val haltIt = buffer.inflight.read(BUFFER_ID)
      val rsp = CombInit(buffer.words.readAsync(BUFFER_ID))
      // Implement bus rsp bypass into the pipeline (without using the buffer)
      // TODO this one can be optional
      when(bus.rsp.valid && bus.rsp.id === BUFFER_ID){
        haltIt := False
        rsp.assignSomeByName(bus.rsp.payload)
      }
      Fetch.WORD := rsp.word
      TRAP := isValid && !haltIt && rsp.error
      trapPort.valid := TRAP
      trapPort.exception := True
      trapPort.tval := Fetch.WORD_PC.asBits
      trapPort.code := CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT
      trapPort.hartId := Global.HART_ID

      haltWhen(haltIt)
    }
    buildBefore.release()
  }
}
