val spinalVersion = "dev"

lazy val root = (project in file(".")).
settings(
  inThisBuild(List(
    organization := "com.github.spinalhdl",
    scalaVersion := "2.12.18",
    version := "2.0.0"
  )),
  scalacOptions += s"-Xplugin:${new File(baseDirectory.value + s"/../SpinalHDL/idslplugin/target/scala-2.12/spinalhdl-idsl-plugin_2.12-$spinalVersion.jar")}",
  scalacOptions += s"-Xplugin-require:idsl-plugin",
  scalacOptions += "-language:reflectiveCalls",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.17",
    "org.yaml" % "snakeyaml" % "1.8",
    "net.fornwall" % "jelf" % "0.7.0",
    "org.scream3r" % "jssc" % "2.8.0"
  ),
  Compile / unmanagedSourceDirectories += baseDirectory.value / "ext/rvls/bindings/jni",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "ext/rvls/bindings/spinal",
  name := "VexiiRiscv"
).dependsOn(spinalHdlIdslPlugin, spinalHdlSim,spinalHdlCore,spinalHdlLib)

lazy val spinalHdlIdslPlugin = ProjectRef(file("../SpinalHDL"), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file("../SpinalHDL"), "sim")
lazy val spinalHdlCore = ProjectRef(file("../SpinalHDL"), "core")
lazy val spinalHdlLib = ProjectRef(file("../SpinalHDL"), "lib")

fork := true