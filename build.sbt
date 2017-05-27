import sbtcrossproject.{crossProject, CrossType}

val Scala_2_12 = "2.12.2"
val Scala_2_11 = "2.11.11"

def crossAlias(aliasName: String, commandName: String, projectNames: String*): Command =
  BasicCommands.newAlias(aliasName, projectNames.map { projectName =>
    s""";++$Scala_2_12
       |;${projectName}JVM/$commandName
       |;++$Scala_2_11
       |;${projectName}JVM/$commandName
       |;${projectName}Native/$commandName
     """.stripMargin
  }.mkString)

def forallAlias(aliasName: String, commandName: String, projectNames: String*): Command =
  BasicCommands.newAlias(aliasName, projectNames.map { projectName =>
    s""";${projectName}JVM/$commandName
       |;${projectName}Native/$commandName
    """.stripMargin
  }.mkString)

lazy val commonSettings = Def.settings(
  scalaVersion := "2.11.8",
  organization := "com.github.nadavwr",
  publishArtifact in (Compile, packageDoc) := false,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayVcsUrl := Some("git@github.com:nadavwr/uninclined.git")
)

lazy val unpublished = Def.settings(
  publish := {},
  publishLocal := {},
  publishM2 := {}
)

lazy val uninclined = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    commonSettings,
    moduleName := "uninclined",
    resolvers += Resolver.bintrayRepo("nadavwr", "maven"),
    libraryDependencies ++= Seq(
      "com.github.nadavwr" %%% "spice-scala" % "0.1.0",
      "com.github.nadavwr" %%% "math-utils" % "0.2.0")
  )
lazy val uninclinedJVM = uninclined.jvm
lazy val uninclinedNative = uninclined.native

lazy val uninclinedTest = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(
    commonSettings,
    unpublished,
    resolvers += Resolver.bintrayRepo("nadavwr", "maven"),
    libraryDependencies += "com.github.nadavwr" %%% "makeshift" % "0.1.3"
  )
  .jvmSettings(test := { (run in Compile).toTask("").value })
  .nativeSettings(test := run.toTask("").value)
  .dependsOn(uninclined)

lazy val uninclinedTestJVM = uninclinedTest.jvm
lazy val uninclinedTestNative = uninclinedTest.native

lazy val uninclinedRoot = (project in file("."))
  .aggregate(uninclinedJVM, uninclinedNative, uninclinedTestJVM, uninclinedTestNative)
  .settings(
    commonSettings,
    unpublished,
    commands += crossAlias("publishLocal", "publishLocal", "uninclined"),
    commands += crossAlias("publish", "publish", "uninclined"),
    commands += crossAlias("unpublish", "bintrayUnpublish", "uninclined"),
    commands += crossAlias("test", "test", "uninclinedTest"),
    commands += forallAlias("clean", "clean", "uninclined", "uninclinedTest")
  )

