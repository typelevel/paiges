lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val paigesSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.12.2",
  crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2"),
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.0.1" % Test,
    "org.scalacheck" %%% "scalacheck" % "1.13.5" % Test),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"),
  // HACK: without these lines, the console is basically unusable,
  // since all imports are reported as being unused (and then become
  // fatal errors).
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,

  // release stuff
  releaseCrossBuild := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <scm>
      <url>git@github.com:typelevel/paiges.git</url>
      <connection>scm:git:git@github.com:typelevel/paiges.git</connection>
    </scm>
    <developers>
      <developer>
        <id>johnynek</id>
        <name>Oscar Boykin</name>
        <url>http://github.com/johnynek/</url>
      </developer>
      <developer>
        <id>coltfred</id>
        <name>Colt Frederickson</name>
        <url>http://github.com/coltfred/</url>
      </developer>
      <developer>
        <id>non</id>
        <name>Erik Osheim</name>
        <url>http://github.com/non/</url>
      </developer>
    </developers>
  ))


lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"))

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  requiresDOM := false,
  jsEnv := NodeJSEnv().value,
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(scala.sys.env.get("TRAVIS").isDefined))

lazy val paiges = project
  .in(file("."))
  .settings(name := "root")
  .settings(paigesSettings: _*)
  .settings(noPublish: _*)
  .aggregate(paigesJVM, paigesJS)
  .dependsOn(paigesJVM, paigesJS)

lazy val paigesJVM = project
  .in(file(".paigesJVM"))
  .settings(moduleName := "paiges")
  .settings(paigesSettings)
  .settings(commonJvmSettings)
  .aggregate(coreJVM, catsJVM, benchmark)
  .dependsOn(coreJVM, catsJVM, benchmark)

lazy val paigesJS = project
  .in(file(".paigesJS"))
  .settings(moduleName := "paiges")
  .settings(paigesSettings)
  .settings(commonJsSettings)
  .aggregate(coreJS, catsJS)
  .dependsOn(coreJS, catsJS)
  .enablePlugins(ScalaJSPlugin)

lazy val core = crossProject.crossType(CrossType.Pure)
  .in(file("core"))
  .settings(name := "paiges-core")
  .settings(moduleName := "paiges-core")
  .settings(paigesSettings: _*)
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val cats = crossProject.crossType(CrossType.Pure)
  .in(file("cats"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(name := "paiges-cats")
  .settings(moduleName := "paiges-cats")
  .settings(paigesSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % "0.9.0",
    "org.typelevel" %%% "cats-kernel-laws" % "0.9.0" % Test))
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val catsJVM = cats.jvm
lazy val catsJS = cats.js

lazy val benchmark = project.in(file("benchmark"))
  .dependsOn(coreJVM, catsJVM)
  .settings(name := "paiges-benchmark")
  .settings(paigesSettings: _*)
  .settings(noPublish: _*)
  .enablePlugins(JmhPlugin)

lazy val docs = project.in(file("docs"))
  .dependsOn(coreJVM, catsJVM)
  .settings(name := "paiges-docs")
  .settings(paigesSettings: _*)
  .settings(noPublish: _*)
  .settings(tutSettings: _*)
  .settings(tutScalacOptions := {
    val testOptions = scalacOptions.in(test).value
    val unwantedOptions = Set("-Ywarn-unused-import", "-Xfatal-warnings")
    testOptions.filterNot(unwantedOptions)
  })
