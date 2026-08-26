package vexiiriscv.misc

import spinal.core.{Bool, _}
import spinal.lib._
import spinal.lib.misc.aia._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.execute.CsrAccessPlugin
import vexiiriscv.riscv.{CSR, IndirectCSR}
import vexiiriscv.riscv.Riscv._

import scala.collection.mutable

class ImsicPlugin(val p: PrivilegedParam) extends FiberPlugin {
  val logic = during setup new Area {
    val cap = host[CsrAccessPlugin]
    val indirect = host[IndirectCsrPlugin]
    val buildBefore = retains(cap.csrLock)

    awaitBuild()
    assert(p.withImsic)

    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val api = cap.hart(hartId)
      val indirectHart = indirect.logic.harts(hartId)

      val m = genImsicArea(CSR.MIREG, CSR.MTOPEI, indirectHart.m)
      val s = p.withSupervisor generate genImsicArea(CSR.SIREG, CSR.STOPEI, indirectHart.s)
      val vs = (p.withHypervisor && p.withGuestImsic) generate genGuestImsicArea(CSR.VSIREG, CSR.VSTOPEI, indirectHart.vs)

      def genImsicArea(ireg: Int, topei: Int, indirectApi: IndirectCsrApi) = new Area {
        val fileParameters = ImsicFileParameters(
          hartId    = hartId,
          guestId   = 0,
          sourceNum = p.imsicInterrupts,
          xlen      = XLEN,
          portNum   = 2
        )
        val file = ImsicFileRam(fileParameters)

        val dataWidth = log2Up(XLEN)
        val trigger = slave(Stream(UInt(32 bits)))
        val linkBus = new Area {
          val port = file.ports(1)
          val piped = trigger.m2sPipe()
          val inRange = !piped.payload.drop(file.idWidth).orR
          val data = B(1, XLEN.get bits) |<< piped.payload.resize(dataWidth)
          port.cmd.op       := ImsicOp.WRITE
          port.cmd.doIp     := True
          port.cmd.address  := piped.payload.dropLow(dataWidth).asUInt.resized
          port.cmd.data     := data
          port.cmd.mask     := data
          port.cmd.valid    := piped.valid && inRange
          piped.ready       := !piped.valid || !inRange || port.cmd.ready
        }

        val linkCsr = new Area {
          val port = file.ports(0)

          val iepFilter = indirectApi.csrCondFilter(id => eiepIdCheck(id), ireg)
          val isIp = !indirectApi.iselect(6)
          val address = indirectApi.iselect(5 downto (XLEN.get == 64).toInt).resized

          port.cmd.op := ImsicOp.READ
          port.cmd.doIp := isIp
          port.cmd.address := address
          port.cmd.data := B(0, XLEN.get bits)
          port.cmd.mask := B(0, XLEN.get bits)
          port.cmd.valid := False

          val pending = RegInit(False) setWhen(port.cmd.fire) clearWhen(port.rsp.valid)
          api.read(port.rsp.data.andMask(port.rsp.valid), iepFilter)

          api.onRead(iepFilter, false) {
            port.cmd.op := ImsicOp.READ
            port.cmd.valid := !pending
            when(!port.rsp.valid) {
              cap.bus.read.doHalt()
            }
          }

          api.onWrite(iepFilter, false) {
            val writeMask = B(XLEN.get bits, default -> True)

            val data = Mux(cap.bus.write.mask,
              cap.bus.write.clear ? B(0, XLEN.get bits) | cap.bus.write.maskBit,
              cap.bus.write.bits
            )
            val mask = Mux(cap.bus.write.mask, cap.bus.write.maskBit, writeMask)

            port.cmd.op := ImsicOp.WRITE
            port.cmd.data := data
            port.cmd.mask := mask
            port.cmd.valid := !pending
            when(!port.rsp.valid) {
              cap.bus.write.doHalt()
            }
          }

          val identity = file.identity
          api.read(topei, 0 -> identity, 16 -> identity)
          val claim = new Area {
            val toClaim = RegInit(U(0, file.idWidth bits))
            api.onRead(topei, false){
              toClaim := identity
            }
            api.onWrite(topei, false) {
              port.cmd.op := ImsicOp.WRITE
              port.cmd.doIp := True
              port.cmd.address := toClaim.drop(log2Up(XLEN)).asUInt
              port.cmd.data := B(0, XLEN.get bits)
              port.cmd.mask := B(1, XLEN.get bits) |<< toClaim(dataWidth-1 downto 0)
              port.cmd.valid := !pending
              when(!port.rsp.valid) {
                cap.bus.write.doHalt()
              }
            }
          }
        }

        api.readWrite(file.threshold, indirectApi.csrFilter(IndirectCSR.eithreshold, ireg))

        def deliveryArbiter(external: Option[Bool]): Bool = {
          val deliveryCode = mutable.ArrayBuffer(
            0x1 -> U(1, XLEN bits),
            default -> U(0, XLEN bits)
          )
          val deliveryInterrupt = mutable.ArrayBuffer(
            1 -> file.interrupt,
            default -> False
          )
          var defaultValue = 0
          external match {
            case Some(ext) => {
              defaultValue = 0x40000000
              deliveryCode += 0x40000000 -> U(0x40000000, XLEN bits)
              deliveryInterrupt += 0x40000000 -> ext
            }
            case _ =>
          }
          val eidelivery = RegInit(U(defaultValue, XLEN bits))
          val eideliveryFilter = indirectApi.csrFilter(IndirectCSR.eidelivery, ireg)
          api.read(eidelivery, eideliveryFilter)
          api.onWrite(eideliveryFilter, true) {
            eidelivery := cap.bus.write.bits.muxList(deliveryCode)
          }
          eidelivery.muxList(deliveryInterrupt)
        }
      }

      def genGuestImsicArea(ireg: Int, topei: Int, indirectApi: IndirectCsrApi) = new Area {
        val fileParameters = (1 to p.guestExternalInterruptFiles).map(geid => ImsicFileParameters(
          hartId    = hartId,
          guestId   = geid,
          sourceNum = p.imsicInterrupts,
          xlen      = XLEN,
          portNum   = 2
        ))
        val files = fileParameters.map(ImsicFileRam(_))

        val dataWidth = log2Up(XLEN)
        val triggers = Vec.fill(files.size)(slave(Stream(UInt(32 bits))))
        val linkBus = triggers.zip(files).map{case (trigger, file) => new Area{
          val port = file.ports(1)
          val piped = trigger.m2sPipe()
          val inRange = !piped.payload.drop(file.idWidth).orR
          val data = B(1, XLEN.get bits) |<< piped.payload.resize(dataWidth)
          port.cmd.op       := ImsicOp.WRITE
          port.cmd.doIp     := True
          port.cmd.address  := piped.payload.dropLow(dataWidth).asUInt.resized
          port.cmd.data     := data
          port.cmd.mask     := data
          port.cmd.valid    := piped.valid && inRange
          piped.ready       := !piped.valid || !inRange || port.cmd.ready
        }}

        val mux = RegInit(U(0, log2Up(p.guestExternalInterruptFiles + 1) bits))
        val valid = mux =/= 0
        val currentMux = Mux(valid, mux - 1, U(0)).resized
        val identities = Vec(files.map(_.identity))
        val identity = identities(currentMux)
        val rectifiedIdentity = Mux(valid, identity, U(0))

        val linkCsr = new Area {
          val iepFilter = indirectApi.csrCondFilter(id => eiepIdCheck(id), ireg)
          val isIp = !indirectApi.iselect(6)
          val address = indirectApi.iselect(5 downto (XLEN.get == 64).toInt).resized

          val cmd = cloneOf(files(0).ports(0).cmd)
          val cmdLink = StreamDemux(cmd, mux, p.guestExternalInterruptFiles + 1)
          cmdLink(0).ready := True
          cmd.op := ImsicOp.READ
          cmd.doIp := isIp
          cmd.address := address
          cmd.data := B(0, XLEN.get bits)
          cmd.mask := B(0, XLEN.get bits)
          cmd.valid := False

          val rsps = Vec.fill(p.guestExternalInterruptFiles + 1)(cloneOf(files(0).ports(0).rsp))
          rsps(0).valid := True
          rsps(0).data := B(0)
          val rsp = rsps(mux)

          files.foreach(file => {
            val cmd = cmdLink(file.p.guestId)
            val rsp = rsps(file.p.guestId)
            file.ports(0).cmd << cmd
            rsp << file.ports(0).rsp
          })

          val pending = RegInit(False) setWhen(cmd.fire) clearWhen(rsp.valid)
          api.read(rsp.data.andMask(rsp.valid), iepFilter)
          api.allowCsr(iepFilter, valid)

          api.onRead(iepFilter, false) {
            cmd.op := ImsicOp.READ
            cmd.valid := !pending
            when(!rsp.valid) {
              cap.bus.read.doHalt()
            }
          }

          api.onWrite(iepFilter, false) {
            val writeMask = B(XLEN.get bits, default -> True)

            val data = Mux(cap.bus.write.mask,
              cap.bus.write.clear ? B(0, XLEN.get bits) | cap.bus.write.maskBit,
              cap.bus.write.bits
            )
            val mask = Mux(cap.bus.write.mask, cap.bus.write.maskBit, writeMask)

            cmd.op := ImsicOp.WRITE
            cmd.data := data
            cmd.mask := mask
            cmd.valid := !pending
            when(!rsp.valid) {
              cap.bus.write.doHalt()
            }
          }

          api.read(topei, 0 -> rectifiedIdentity, 16 -> rectifiedIdentity)
          api.allowCsr(topei, valid)
          val claim = new Area {
            val toClaim = RegInit(U(0, files(0).idWidth bits))
            api.onRead(topei, false){
              toClaim := rectifiedIdentity
            }
            api.onWrite(topei, false) {
              cmd.op := ImsicOp.WRITE
              cmd.doIp := True
              cmd.address := toClaim.drop(log2Up(XLEN)).asUInt
              cmd.data := B(0, XLEN.get bits)
              cmd.mask := B(1, XLEN.get bits) |<< toClaim(dataWidth-1 downto 0)
              cmd.valid := !pending
              when(!rsp.valid) {
                cap.bus.write.doHalt()
              }
            }
          }
        }

        val thresholds = Vec(files.map(_.threshold))
        val eithresholdFilter = indirectApi.csrFilter(IndirectCSR.eithreshold, ireg)
        api.allowCsr(eithresholdFilter, valid)
        api.readWrite(thresholds(currentMux), indirectApi.csrFilter(IndirectCSR.eithreshold, ireg, valid))

        /* eidelivery only supports 0/1 */
        val eidelivery = Vec.fill(p.guestExternalInterruptFiles)(RegInit(False))
        val eideliveryFilter = indirectApi.csrFilter(IndirectCSR.eidelivery, ireg)
        api.allowCsr(eideliveryFilter, valid)
        api.read(eidelivery(currentMux), eideliveryFilter)
        api.onWrite(eideliveryFilter, true) {
          eidelivery(currentMux) := cap.bus.write.bits === 0x1
        }

        def deliveryArbiter(): Bool = eidelivery(currentMux) && valid
      }
    }

    def eiepIdCheck(id: UInt) = {
      val mainCheck = (id >> 7) === 1
      val xlenCheck = (XLEN.get == 64).mux(!id(0), True)
      val lineNums = p.imsicInterrupts / XLEN.get
      val idCheck = (p.imsicInterrupts == 2048).mux(True, id(5 downto (XLEN.get == 64).toInt) < lineNums)

      /* eie/eip = 0x80 ~ 0xff */
      mainCheck && xlenCheck && idCheck
    }

    buildBefore.release()
  }
}
