val spinalVersion = "dev"
val spinalHdlFromSource = sys.env.getOrElse("SPINALHDL_FROM_SOURCE", "1") == "1"

def rootGen() = {
  var ret = (project in file(".")).settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.12.18",
      version := "2.0.0"
    )),
    scalacOptions += s"-Xplugin:${new File(baseDirectory.value + s"/ext/SpinalHDL/idslplugin/target/scala-2.12/spinalhdl-idsl-plugin_2.12-$spinalVersion.jar")}",
    scalacOptions += s"-Xplugin-require:idsl-plugin",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17",
      "org.yaml" % "snakeyaml" % "1.8",
      "net.fornwall" % "jelf" % "0.7.0"
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
    ret = ret.dependsOn(spinalHdlIdslPlugin, spinalHdlSim,spinalHdlCore,spinalHdlLib)
  }
  ret
}

lazy val root = rootGen()
lazy val spinalHdlIdslPlugin = ProjectRef(file("ext/SpinalHDL"), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file("ext/SpinalHDL"), "sim")
lazy val spinalHdlCore = ProjectRef(file("ext/SpinalHDL"), "core")
lazy val spinalHdlLib = ProjectRef(file("ext/SpinalHDL"), "lib")

fork := true
