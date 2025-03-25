package vexiiriscv

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin._
import scala.collection._

object VexiiRiscv {
  def apply(): VexiiRiscv = new VexiiRiscv
  def apply(plugins: scala.collection.Seq[Hostable]): VexiiRiscv = {
    val v = new VexiiRiscv
    v.host.asHostOf(plugins)
    v
  }
}

/**
 * This is the VexiiRiscv toplevel, it doesn't define/generate any hardware by itself,
 * it is just a framework to register/execute plugins and bind a database to share global variables
 */
class VexiiRiscv extends Component {
  val database = new Database
  val host     = database on (new PluginHost)
}
