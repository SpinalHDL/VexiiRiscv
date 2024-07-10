package vexiiriscv.execute.lsu

trait LsuCachelessBusProvider {
  def getLsuCachelessBus() : LsuCachelessBus
}

trait CmoService{
  def withSoftwarePrefetch : Boolean
}
