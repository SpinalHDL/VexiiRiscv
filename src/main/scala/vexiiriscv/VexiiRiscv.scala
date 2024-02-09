package vexiiriscv

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin._

object VexiiRiscv{
  def apply(): VexiiRiscv = new VexiiRiscv
  def apply(plugins : Seq[Hostable]): VexiiRiscv = {
    val v = new VexiiRiscv
    v.host.asHostOf(plugins)
    v
  }
}

class VexiiRiscv extends Component{
  val database = new Database
  val host = database on (new PluginHost)
}



