val spinalVersion = "1.12.0"

lazy val root =  (project in file(".")).settings(
  inThisBuild(List(
    organization := "com.github.spinalhdl",
    scalaVersion := "2.12.18",
    version := "2.0.0"
  )),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.17",
    "org.yaml" % "snakeyaml" % "1.8",
    "net.fornwall" % "jelf" % "0.7.0",
    "org.scream3r" % "jssc" % "2.8.0"
  ),
  libraryDependencies ++= Seq(
    "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
    "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion,
    compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
  ),
  Compile / unmanagedSourceDirectories += baseDirectory.value / "ext/rvls/bindings/jni",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "ext/rvls/bindings/spinal",
  name := "VexiiRiscv"
)

fork := true