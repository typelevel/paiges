import sbtcrossproject.{crossProject, CrossType}

val Scala211 = "2.11.12"
val Scala212 = "2.12.10"
val Scala213 = "2.13.1"

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String) = {
  def extraDirs(suffix: String) =
    List(CrossType.Pure, CrossType.Full)
      .flatMap(_.sharedSrcDir(srcBaseDir, srcName).toList.map(f => file(f.getPath + suffix)))
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y)) if y <= 12 =>
      extraDirs("-2.12-")
    case Some((2, y)) if y >= 13 =>
      extraDirs("-2.13+")
    case _ => Nil
  }
}

inThisBuild(
  List(
    organization := "org.typelevel",
    scalaVersion := Scala213,
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
  )
)

crossScalaVersions := Nil
noPublish

// Aggregate for JVM projects, for example run `jvm/test` to run only JVM tests.
lazy val jvm = project
  .in(file(".jvm"))
  .settings(noPublish)
  .aggregate(coreJVM, catsJVM)

lazy val js = project
  .in(file(".js"))
  .settings(noPublish)
  .aggregate(coreJS, catsJS)

lazy val native = project
  .in(file(".native"))
  .settings(noPublish)
  .settings(
    crossScalaVersions := Seq(Scala211)
  )
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
      "org.scalatest" %%% "scalatest" % "3.1.1" % Test,
      "org.scalatestplus" %%% "scalacheck-1-14" % "3.1.0.1" % Test,
      "org.scalacheck" %%% "scalacheck" % "1.14.3" % Test
    )
  )
  .nativeSettings(
    commonNativeSettings,
    scalacOptions in Compile -= "-Xfatal-warnings",
    sources in Test ~= {
      _.filter(f => Set("JsonTest.scala", "PaigesTest.scala").contains(f.getName))
    },
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.1.1" % Test
    )
  )
lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val cats = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("cats"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    commonSettings,
    name := "paiges-cats",
    moduleName := "paiges-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.0.0",
      "org.typelevel" %%% "cats-laws" % "2.0.0" % Test,
      "org.typelevel" %%% "discipline-scalatest" % "1.0.0" % Test
    ),
    mimaPreviousArtifacts := previousArtifact(version.value, "cats")
  )
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)

lazy val catsJVM = cats.jvm
lazy val catsJS = cats.js

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(coreJVM, catsJVM)
  .settings(
    noPublish,
    crossScalaVersions := List(Scala212),
    name := "paiges-benchmark"
  )
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("docs"))
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
  TaskKey[Unit]("checkCI") := Def
    .sequential(
      test.in(Test),
      doc.in(Compile),
      mimaReportBinaryIssues
    )
    .value,
  // scalac options are defined in commonSettings instead of inThisBuild
  // because we customize the settings based on scalaVersion.
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ),
  // HACK: without these lines, the console is basically unusable,
  // since all imports are reported as being unused (and then become
  // fatal errors).
  scalacOptions in (Compile, console) ~= { _.filterNot("-Xlint" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 =>
        Seq(
          "-Xfatal-warnings",
          "-Yno-adapted-args",
          "-Xfuture"
        )
      case _ =>
        Nil
    }
  ),
  Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
  Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value)
)

lazy val commonJvmSettings = Seq(testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"))

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
  def mod(x: Int, y: Int, z: Int): ModuleID =
    "org.typelevel" %% s"paiges-$proj" % s"$x.$y.$z"

  // the "-dbuild..." part is for Scala community build friendliness
  val regex = "([0-9]+)\\.([0-9]+)\\.([0-9]+)(-SNAPSHOT|-dbuild[a-z0-9]*|\\+.*)?".r
  version match {
    case regex(smajor, sminor, spatch, suffix) =>
      val (major, minor, patch) = (smajor.toInt, sminor.toInt, spatch.toInt)

      // unless we're in a 0.x release, we need to ensure that our
      // minor version is compatible with previous minor versions.
      //
      // for example, 4.1.1 should be compatible with 4.1.0 and also
      // with 4.0.0.
      //
      // ideally we'd want to ensure that 4.1.1 was compatible with
      // the latest 4.0.x release (e.g. 4.0.13) but that would require
      // parsing our full release history; currently the algorithm is
      // only based on the current version.
      val minors = if (major > 0) (0 until minor).toSet else Set.empty
      val patches = (0 until patch).toSet
      val current = if (suffix.startsWith("+")) Set(mod(major, minor, patch)) else Set.empty[ModuleID]
      minors.map(mod(major, _, 0)) | patches.map(mod(major, minor, _)) | current
    case _ =>
      throw new RuntimeException(s"Could not parse Paiges version: $version")
  }
}

lazy val noPublish = commonSettings ++ Seq(
  skip in publish := true,
  mimaPreviousArtifacts := Set.empty,
  coverageEnabled := false
)
