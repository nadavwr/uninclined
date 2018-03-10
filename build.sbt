val Scala_2_12 = "2.12.4"
val Scala_2_11 = "2.11.12"

lazy val uninclined = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    moduleName := "uninclined",
    organization := "com.github.nadavwr",
    scalaVersion := Scala_2_11,
    publishArtifact in(Compile, packageDoc) := false,
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    bintrayVcsUrl := Some("git@github.com:nadavwr/uninclined.git"),
    resolvers += Resolver.bintrayRepo("nadavwr", "maven"),
    resolvers += Resolver.defaultLocal,
    libraryDependencies ++= Seq(
      "com.github.nadavwr" %%% "cspice" % "0.3.0",
      "com.github.nadavwr" %%% "math-utils" % "0.5.0")
  )
  .jvmSettings(crossScalaVersions := Seq(Scala_2_11, Scala_2_12))
  .nativeSettings(crossScalaVersions := Seq(Scala_2_11))

lazy val uninclinedJVM = uninclined.jvm
lazy val uninclinedNative = uninclined.native
