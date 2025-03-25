val spinalVersion = "dev"
val spinalHdlFromSource = sys.env.getOrElse("SPINALHDL_FROM_SOURCE", "1") == "1"
val spinalHdlPathEnabled = sys.env.contains("SPINALHDL_PATH")
val spinalHdlPath = sys.env.getOrElse("SPINALHDL_PATH", "./ext/SpinalHDL")

def rootGen() = {
  var ret = (project in file(".")).settings(
    scalaVersion := SpinalVersion.compilers.head,
    crossScalaVersions := SpinalVersion.compilers,
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := SpinalVersion.compilers.head,
      crossScalaVersions := SpinalVersion.compilers,
      version := "2.0.0"
    )),
    scalacOptions += s"-Xplugin:${new File((if(spinalHdlPathEnabled) spinalHdlPath else baseDirectory.value.getAbsolutePath + s"/ext/SpinalHDL") + s"/idslplugin/target/scala-${scalaVersion.value.split("\\.").dropRight(1).mkString(".")}/spinalhdl-idsl-plugin_${scalaVersion.value.split("\\.").dropRight(1).mkString(".")}-$spinalVersion.jar")}",
    scalacOptions += s"-Xplugin-require:idsl-plugin",
    scalacOptions += "-language:reflectiveCalls",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17",
      "org.yaml" % "snakeyaml" % "1.8",
      "net.fornwall" % "jelf" % "0.7.0",
      "org.scream3r" % "jssc" % "2.8.0"
    ),
    libraryDependencies ++= (if (spinalHdlFromSource) Nil else Seq(
      "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
      "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion,
      compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
    )),
    Compile / unmanagedSourceDirectories += baseDirectory.value / "ext/rvls/bindings/jni",
    Compile / unmanagedSourceDirectories += baseDirectory.value / "ext/rvls/bindings/spinal",
    name := "VexiiRiscv"
  )
  if(spinalHdlFromSource){
    ret = ret.dependsOn(spinalHdlIdslPlugin, spinalHdlSim, spinalHdlCore, spinalHdlLib)
  }
  ret
}

lazy val root = rootGen()
lazy val spinalHdlIdslPlugin = ProjectRef(file(spinalHdlPath), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file(spinalHdlPath), "sim")
lazy val spinalHdlCore = ProjectRef(file(spinalHdlPath), "core")
lazy val spinalHdlLib = ProjectRef(file(spinalHdlPath), "lib")

fork := true