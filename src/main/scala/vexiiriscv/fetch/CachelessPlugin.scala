package vexiiriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.database.Database._
import spinal.lib.misc.pipeline._
import vexiiriscv._
import vexiiriscv.Global._
import vexiiriscv.schedule.FlusherService

case class CachelessBusParam(addressWidth : Int, dataWidth : Int, idCount : Int, cmdPersistence : Boolean){
  val idWidth = log2Up(idCount)
}

case class CachelessCmd(p : CachelessBusParam) extends Bundle{
  val id = UInt(p.idWidth bits)
  val address = UInt(p.addressWidth bits)
}

case class CachelessRsp(p : CachelessBusParam) extends Bundle{
  val id = UInt(p.idWidth bits)
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

object CachelessPlugin{
  val ID_WIDTH = blocking[Int]
  val ID = blocking[Int]
}

class CachelessPlugin(var wordWidth : Int,
                      var forkAt : Int = 0,
                      var joinAt : Int = 1,
                      var cmdForkPersistence : Boolean = true) extends FiberPlugin{
  lazy val pp = host[PipelinePlugin]
  during setup{
    pp.retain()
    Fetch.WORD_WIDTH.set(wordWidth)
  }
  val logic = during build new Area{
    val idCount = joinAt - forkAt //TODO we need more of it
    val p = CachelessBusParam(MIXED_WIDTH, Fetch.WORD_WIDTH, idCount, false)
    val bus = master(CachelessBus(p))
    val x = CombInit(pp.ctrl(2)(Fetch.WORD_PC))

    val forkCtrl = pp.ctrl(forkAt)
    val joinCtrl = pp.ctrl(joinAt)

    val BUFFER_ID = SignalKey(UInt(log2Up(idCount) bits))

    val buffer = new Area{
      val reserveId = Counter(idCount)
      val reserved = Vec.fill(idCount)(RegInit(False))
      val valids = Vec.fill(idCount)(RegInit(True))
      val words = Mem.fill(idCount)(Fetch.WORD)
      val full = CombInit(reserved.read(reserveId) || !valids.read(reserveId)) //TODO that's one cycle late, can use sort of ahead value

      when(bus.cmd.fire) {
        reserveId.increment()
        reserved(reserveId) := True
        valids(reserveId) := False
      }

      when(bus.rsp.valid) {
        valids(bus.rsp.id) := True
        words(bus.rsp.id) := bus.rsp.word
      }

      val flushes = host[FlusherService].getFlushCmds().filter(_.priority >= PcService.Priorities.FETCH_WORD(forkAt, true))
      val flushHarts = (0 until Global.HART_COUNT).map(hartId => flushes.map(f => f.valid && f.hartId === hartId).orR).asBits
      for(ctrlId <- forkAt to joinAt){
        val ctrl = pp.ctrl(ctrlId)
        when(flushHarts(ctrl(BUFFER_ID))){
          reserved(ctrl(BUFFER_ID)) := False
        }
      }
    }

    val fork = new forkCtrl.Area{
      val cmdFork = forkStream()
      bus.cmd.arbitrationFrom(cmdFork.haltWhen(buffer.full))
      bus.cmd.id := buffer.reserveId
      bus.cmd.address := Fetch.WORD_PC

      BUFFER_ID := buffer.reserveId
    }

    val join = new joinCtrl.Area{
      val haltIt = !buffer.valids(BUFFER_ID)
      Fetch.WORD := buffer.words.readAsync(BUFFER_ID)
      // Implement bus rsp bypass into the pipeline (without using the buffer)
      when(bus.rsp.valid && bus.rsp.id === BUFFER_ID){
        haltIt := False
        Fetch.WORD := bus.rsp.word
      }
      haltWhen(haltIt)

      when(isValid && isReady){
        buffer.reserved(BUFFER_ID) := False
      }
    }

    pp.release()
  }
}
