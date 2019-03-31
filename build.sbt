import sbtcrossproject.{crossProject, CrossType}

val Scala211 = "2.11.12"
val Scala212 = "2.12.8"
val Scala213 = "2.13.0-M5"

inThisBuild(List(
  organization := "org.typelevel",
  scalaVersion := Scala212,
  crossScalaVersions := Seq(Scala211, Scala212, Scala213),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/typelevel/paiges")),
  pomExtra := (
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
  coverageFailOnMinimum := false
))

crossScalaVersions := Nil
noPublish

// Aggregate for JVM projects, for example run `jvm/test` to run only JVM tests.
lazy val jvm = project.in(file(".jvm"))
  .settings(noPublish)
  .aggregate(coreJVM, catsJVM)

lazy val js = project.in(file(".js"))
  .settings(noPublish)
  .aggregate(coreJS, catsJS)

lazy val native = project.in(file(".native"))
  .settings(noPublish)
  .aggregate(coreNative)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    commonSettings,
    name := "paiges-core",
    moduleName := "paiges-core",
    mimaPreviousArtifacts := previousArtifact(version.value, "core")
  )
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.6-SNAP5" % Test,
      "org.scalacheck" %%% "scalacheck" % "1.14.0" % Test
    )
  )
  .nativeSettings(
    commonNativeSettings,
    scalacOptions in Compile -= "-Xfatal-warnings",
    sources in Test ~= {
      _.filter(f => Set("JsonTest.scala", "PaigesTest.scala").contains(f.getName))
    },
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.0-SNAP10" % Test
    )
  )
lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val cats = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("cats"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    commonSettings,
    name := "paiges-cats",
    moduleName := "paiges-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "1.5.0",
      "org.typelevel" %%% "cats-laws" % "1.5.0" % Test),
    mimaPreviousArtifacts := previousArtifact(version.value, "cats"))
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
lazy val catsJVM = cats.jvm
lazy val catsJS = cats.js

lazy val benchmark = project.in(file("benchmark"))
  .dependsOn(coreJVM, catsJVM)
  .settings(
    noPublish,
    crossScalaVersions := List(Scala212),
    name := "paiges-benchmark",
  )
  .enablePlugins(JmhPlugin)

lazy val docs = project.in(file("docs"))
  .dependsOn(coreJVM, catsJVM)
  .enablePlugins(TutPlugin)
  .settings(
    noPublish,
    crossScalaVersions := List(Scala212),
    name := "paiges-docs",
    scalacOptions in Tut := {
      val testOptions = scalacOptions.in(test).value
      val unwantedOptions = Set("-Xlint", "-Xfatal-warnings")
      testOptions.filterNot(unwantedOptions)
    }
  )

lazy val commonSettings = Seq(
  // The validation steps that we run in CI.
  TaskKey[Unit]("checkCI") := Def.sequential(
    test.in(Test),
    doc.in(Compile),
    mimaReportBinaryIssues
  ).value,
  // scalac options are defined in commonSettings instead of inThisBuild
  // because we customize the settings based on scalaVersion.
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"),
  // HACK: without these lines, the console is basically unusable,
  // since all imports are reported as being unused (and then become
  // fatal errors).
  scalacOptions in (Compile, console) ~= {_.filterNot("-Xlint" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 =>
        Seq(
          "-Xfatal-warnings",
          "-Yno-adapted-args"
        )
      case _ =>
        Nil
    }
  )
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"))

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(scala.sys.env.get("TRAVIS").isDefined),
  coverageEnabled := false
)

lazy val commonNativeSettings = Seq(
  nativeLinkStubs := true,
  scalaVersion := Scala211,
  crossScalaVersions := Seq(Scala211)
)

def previousArtifact(version: String, proj: String) = {
  // the "-dbuild..." part is for Scala community build friendliness
  val regex = "0\\.([0-9]+)\\.[0-9]+(-SNAPSHOT|-dbuild[a-z0-9]*|\\+.*)?".r
  version match {
    case regex("1", _) => Set("org.typelevel" %% s"paiges-$proj" % "0.1.0")
    case regex("2", _) => Set.empty[ModuleID]
    case _ => throw new RuntimeException(s"Unexpected version: ${version}")
  }
}

lazy val noPublish = commonSettings ++ Seq(
  skip in publish := true,
  mimaPreviousArtifacts := Set.empty,
  coverageEnabled := false
)
