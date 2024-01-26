package vexiiriscv.execute.lsu

trait LsuCachelessBusProvider {
  def getLsuCachelessBus() : LsuCachelessBus
}
