import com.typesafe.config._
import sourcecode.File
import sbt.io.Path
import scala.collection.JavaConverters._

object SpinalVersion {
  val currentFile = new java.io.File(sourcecode.File())
  val configFilePath = currentFile.getParent + Path.sep + "version.conf"
  val conf = ConfigFactory.parseFile(new java.io.File(configFilePath)).resolve()

  val compilers = conf.getStringList("compilers").asScala.toList
}
