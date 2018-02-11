import ReleaseTransformations._

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val paigesSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.4"),
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
  scalacOptions in (Compile, console) ~= {_.filterNot("-Xlint" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,

  // release stuff
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <url>https://github.com/typelevel/paiges</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
        <comments>A business-friendly OSS license</comments>
      </license>
    </licenses>
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
  ),
  coverageMinimum := 60,
  coverageFailOnMinimum := false) ++ mimaDefaultSettings

def previousArtifact(version: String, proj: String) = {
  // the "-dbuild..." part is for Scala community build friendliness
  val regex = "0\\.([0-9]+)\\.[0-9]+(-SNAPSHOT|-dbuild[a-z0-9]*)?".r
  version match {
    case regex("1", _) => Set("org.typelevel" %% s"paiges-$proj" % "0.1.0")
    case regex("2", _) => Set.empty[ModuleID]
    case _ => throw new RuntimeException(s"Unexpected version: ${version}")
  }
}

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"))

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
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
  .settings(mimaPreviousArtifacts := previousArtifact(version.value, "core"))
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings:_*)
  .jsSettings(coverageEnabled := false)
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
    "org.typelevel" %%% "cats-core" % "1.0.1",
    "org.typelevel" %%% "cats-laws" % "1.0.1" % Test))
  .settings(mimaPreviousArtifacts := previousArtifact(version.value, "cats"))
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings:_*)
  .jsSettings(coverageEnabled := false)
  .jvmSettings(commonJvmSettings:_*)

lazy val catsJVM = cats.jvm
lazy val catsJS = cats.js

lazy val benchmark = project.in(file("benchmark"))
  .dependsOn(coreJVM, catsJVM)
  .settings(name := "paiges-benchmark")
  .settings(paigesSettings: _*)
  .settings(noPublish: _*)
  .settings(coverageEnabled := false)
  .enablePlugins(JmhPlugin)

lazy val docs = project.in(file("docs"))
  .dependsOn(coreJVM, catsJVM)
  .settings(name := "paiges-docs")
  .settings(paigesSettings: _*)
  .settings(noPublish: _*)
  .enablePlugins(TutPlugin)
  .settings(scalacOptions in Tut := {
    val testOptions = scalacOptions.in(test).value
    val unwantedOptions = Set("-Xlint", "-Xfatal-warnings")
    testOptions.filterNot(unwantedOptions)
  })
