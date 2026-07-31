package vexiiriscv

import scala.language.dynamics
import scala.collection.mutable

case class ExtensionVersion(major: Int, minor: Int)

case class Extension(name: String,
                     var _version: ExtensionVersion = ExtensionVersion(1, 0),
                     var depends: Seq[String] = Seq(),
                     var requires: Seq[String] = Seq(),
                     var implies: Seq[String] = Seq(),
                     var reportIf: Seq[String] => Boolean = _ => true,
                     var ignore: Boolean = false,
                     var dependCheck: Boolean = false,
                     )
{
  def version(major: Int, minor: Int): this.type = {
    _version = ExtensionVersion(major, minor)
    this
  }

  def depend(exts: String*): this.type = {
    depends = exts
    this
  }

  def require(exts: String*): this.type = {
    requires = exts
    this
  }

  def imply(exts: String*): this.type = {
    implies = exts
    this
  }

  def withoutReport: this.type = {
    reportIf = _ => false
    this
  }

  def withReport: this.type = {
    reportIf = _ => true
    this
  }

  def withCondReport(reportCheck: Seq[String] => Boolean): this.type = {
    reportIf = reportCheck
    this
  }

  def needReport(exts: Seq[String]): Boolean = reportIf(exts)

  def ignored: this.type = {
    ignore = true
    this
  }

  def withDependCheck: this.type = {
    dependCheck = true
    this
  }
}

object ExtensionList {
  def E(name: String) = Extension(name)

  val supported = Seq(
    /* Base / one-letter extensions and aliases */
    E("i").version(2, 1),
    E("e").version(2, 0),
    E("m").version(2, 0).imply("zmmul"),
    E("a").version(2, 1).depend("zaamo", "zalrsc").withDependCheck,
    E("f").version(2, 2).depend("zicsr"),
    E("d").version(2, 2).depend("f"),
    E("c").version(2, 0).depend("zca"),
    E("b").depend("zba", "zbb", "zbc", "zbs").withDependCheck,
    E("h").depend("s"),

    /* Special abbreviation extension */
    E("g").depend("i", "m", "a", "f", "d", "zicsr", "zifencei").withDependCheck.ignored,

    /* Privilege-level single-letter support */
    E("s").version(1, 13).depend("u").ignored,
    E("u").version(1, 13).ignored,

    /* Z* extensions */
    E("zicbom"),
    E("zicsr").version(2, 0),
    E("zicntr").version(2, 0),
    E("zifencei").version(2, 0),
    E("zihpm").version(2, 0).depend("zicntr"),
    E("zmmul").withCondReport(!_.contains("m")),
    E("zaamo"),
    E("zalrsc"),
    E("zba"),
    E("zbb"),
    E("zbc").imply("zbkc"),
    E("zbkb"),
    E("zbkc"),
    E("zbkx"),
    E("zbs"),
    E("zca"),
    E("zcf").depend("c", "f"),
    E("zcd").depend("c", "d"),
    E("zknd"),
    E("zkne"),
    E("zknh"),
    E("zksed"),
    E("zksh"),

    /* Ss* extensions */
    E("ssaia").require("s").depend("smaia", "sscsrind"),
    E("sscofpmf").require("s").depend("zihpm"),
    E("sscsrind").require("s").depend("smcsrind"),
    E("sstc").require("s").depend("zicntr"),

    /* Sv* extensions */
    E("svade").require("s"),

    /* Sh* extensions */
    E("shlcofideleg").require("h").depend("sscofpmf"),

    /* Sm* extensions */
    E("smcntrpmf").depend("zicntr"),
    E("smaia").depend("smcsrind"),
    E("smcsrind"),
  )

  val indexed = supported.map(e => e.name -> e).toMap

  def versionOf(name: String): Option[ExtensionVersion] = {
    val key = name.toLowerCase
    indexed.get(key).map(_._version)
  }
}

sealed trait ImpliedSource
case class ByExtension(name: String) extends ImpliedSource
case class ByHint(name: String) extends ImpliedSource

case class ExtensionState(var explicit: Boolean = false,
                          requiredBy: mutable.Set[String] = mutable.Set[String](),
                          impliedBy: mutable.Set[ImpliedSource] = mutable.Set[ImpliedSource]()) {
  def active: Boolean = explicit || requiredBy.nonEmpty || impliedBy.nonEmpty
  def required: Boolean = requiredBy.nonEmpty || impliedBy.exists(_.isInstanceOf[ByExtension])
  def requiredExtensions = requiredBy.toSeq ++ impliedBy.collect { case ByExtension(name) => name }
}

case class ExtensionManager(isa: Set[String] = Set[String]()) extends Dynamic {
  import ExtensionManager._
  private val SingleExtension = "withRv([a-z])".r
  private val MultiExtension = "with([ZS][a-z0-9]+)".r

  def selectDynamic(name: String): Boolean = name match {
    case "withMachine" => true
    case "withSupervisor" => check("s")
    case "withUser" => check("u")
    case "withHypervisor" => check("h")

    case "withMul" => check("zmmul")
    case "withDiv" => check("m")

    case "withIndirectCsr" => check("smcsrind") // sscsrind implies sscsrind
    case "withRdTime" => check("zicntr")
    case "withPerformanceCounters" => check("zicntr")
    case "withPerformanceScountovf" => check("sscofpmf")
    case "withAtomics" => check("zaamo") || check("zalrsc")
    case "withRvZkn" | "withRvZknAes" => check("zkne") || check("zknd")

    case SingleExtension(ext) => check(ext)
    case MultiExtension(ext) => check(ext.toLowerCase)
    case _ => ???
  }

  val states: mutable.HashMap[String, ExtensionState] = {
    val result = mutable.HashMap.empty[String, ExtensionState]
    ExtensionList.indexed.keys.foreach(name => result += name -> ExtensionState())
    result
  }

  add(isa.toSeq: _*)

  def add(exts: String*): Unit = exts.map(_.toLowerCase).foreach(addInternal)

  def remove(exts: String*): Unit = exts.map(_.toLowerCase).foreach(removeInternal)

  def check(exts: String*): Boolean = exts.map(_.toLowerCase).forall { ext => states.get(ext).exists(_.active) }

  def finialize(xlen: Int): Unit = {
    add("i")

    if (check("e")) {
      /*
       * Try remove base I first as user force enable base E,
       * then remove base E if I can not be removed
       */
      remove("i")
      if (check("i")) {
        remove("e")
      }
    }

    /* ISA change for VexiiRiscv implementation */

    /* C extension check */
    if (check("zca")) add("c")
    if (check("c", "d")) add("zcd")
    if (xlen == 32 && check("c", "f")) add("zcf")
    if (check("zbkc")) add("zbc")

    /* AES always enable both encrypt and decrypt */
    if (check("zknd") || check("zkne")) {
      add("zknd", "zkne")
    }

    addDependCheckHints()

    val brokenExtensions = getActiveExts.filter(e => !check(ExtensionList.indexed(e).requires :_*))
    assert(brokenExtensions.size == 0, s"Unmet Extensions ${brokenExtensions}")

    assert(xlen == 32 || !check("zcf"), "Zcf is RV32 only")
  }

  def addInternal(ext: String): Unit = states.get(ext) match {
    case Some(_) => updateState(ext)(_.explicit = true)
    case None => println(s"ISA: ${ext} is not supported")
  }

  def removeInternal(ext: String): Unit = states.get(ext).foreach { state =>
    if (state.active) {
      val required = state.required || isActiveRequired(ext)
      if (required) {
        println(s"ISA: ${ext} can not be removed as it is required or implied")
      } else {
        updateState(ext)(_.explicit = false)
      }
    }
  }

  def activate(ext: String): Unit = {
    val info = ExtensionList.indexed(ext)
    info.depends.foreach { deps => updateState(deps) { state => state.requiredBy += ext }}
    info.implies.foreach { implied => updateState(implied) { state => state.impliedBy += ByExtension(ext) }}
  }

  def deactivate(ext: String): Unit = {
    val info = ExtensionList.indexed(ext)
    info.depends.foreach { deps => updateState(deps) { state => state.requiredBy -= ext }}
    info.implies.foreach { implied => updateState(implied) { state => state.impliedBy -= ByExtension(ext) }}
  }

  private def updateState(ext: String)(updateOp: ExtensionState => Unit): Boolean = {
    val state = states(ext)
    val wasActive = state.active
    updateOp(state)

    if (wasActive != state.active) {
      if (state.active) activate(ext) else deactivate(ext)
    }

    wasActive != state.active
  }

  def addDependCheckHints(): Unit = {
    var changed = true
    while (changed) {
      changed = false
      ExtensionList.supported.filter(_.dependCheck).foreach { extension =>
        val ext = extension.name
        if (check(extension.depends: _*)) {
          if (updateState(ext) { state => state.impliedBy += ByHint(ext) }) changed = true
        }
      }
    }
  }

  def isActiveRequired(ext: String): Boolean = states.exists { case (name, state) => state.active && ExtensionList.indexed(name).requires.contains(ext) }

  def getActiveExts: Set[String] = states.collect { case (name, state) if state.active => name }.toSet

  def getIsaStr(includeIgnored: Boolean = false): String = ExtensionManager.getIsaStr(getActiveExts, includeIgnored)

  def getIsaNameArray(includeIgnored: Boolean = false): Seq[String] = ExtensionManager.getIsaNameArray(getActiveExts, includeIgnored)
}

object ExtensionManager {
  def getIsaStr(exts: scala.collection.Set[String], includeIgnored: Boolean = false): String = {
    val extArray = ExtensionManager.getIsaNameArray(exts, includeIgnored)
    val single = extArray.filter(_.length() == 1).mkString
    val multi = extArray.filter(_.length() > 1).map("_" + _).mkString

    single + multi
  }

  def getIsaNameArray(exts: scala.collection.Set[String], includeIgnored: Boolean = false): Seq[String] = {
    ExtensionList.supported.filter(
      e => exts.contains(e.name) && e.needReport(exts.toSeq) && (!e.ignore || includeIgnored)
    ).map(_.name).sortBy(e => ExtensionList.supported.indexWhere(_.name == e))
  }
}
