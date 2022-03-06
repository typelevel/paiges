val Scala212 = "2.12.15"
val Scala213 = "2.13.8"
val Scala3Version = "3.1.0"

ThisBuild / tlBaseVersion := "0.4"

ThisBuild / scalaVersion := Scala213
ThisBuild / tlVersionIntroduced := Map("3" -> "0.4.2")
ThisBuild / crossScalaVersions := Seq(Scala213, Scala212, Scala3Version)
ThisBuild / githubWorkflowBuildMatrixExclusions +=
  MatrixExclude(Map("project" -> "rootNative", "scala" -> Scala3Version))

// Setup coverage
ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List("2.13.8"),
    steps = List(WorkflowStep.Checkout) ++ WorkflowStep.SetupJava(
      githubWorkflowJavaVersions.value.toList
    ) ++ githubWorkflowGeneratedCacheSteps.value ++ List(
      WorkflowStep.Sbt(List("coverage", "rootJVM/test", "coverageAggregate")),
      WorkflowStep.Run(List("bash <(curl -s https://codecov.io/bash)"))
    )
  )

ThisBuild / tlCiReleaseBranches := Seq("master")
ThisBuild / tlSitePublishBranch := Some("master")

lazy val root = tlCrossRootProject.aggregate(core, cats)

ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("johnynek", "Oscar Boykin"),
  tlGitHubDev("coltfred", "Colt Frederickson"),
  tlGitHubDev("non", "Erik Osheim")
)

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String) = {
  def extraDirs(suffix: String) =
    List(CrossType.Pure, CrossType.Full)
      .flatMap(_.sharedSrcDir(srcBaseDir, srcName).toList.map(f => file(f.getPath + suffix)))
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y)) if y <= 12 =>
      extraDirs("-2.12-")
    case Some((2, y)) if y >= 13 =>
      extraDirs("-2.13+")
    case Some((3, _)) =>
      extraDirs("-2.13+")
    case _ => Nil
  }
}

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    commonSettings,
    name := "paiges-core",
    moduleName := "paiges-core",
    libraryDependencies ++= Seq(
      "org.scalatestplus" %%% "scalacheck-1-15" % "3.2.10.0" % Test,
      "org.scalatest" %%% "scalatest-funsuite" % "3.2.11" % Test
    ),
    // TODO: 2.13 has warnings for using Stream, but scalacheck Shrink
    tlFatalWarningsInCi := scalaVersion.value.startsWith("2.12.")
  )
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val cats = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("cats"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    commonSettings,
    name := "paiges-cats",
    moduleName := "paiges-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.7.0",
      "org.typelevel" %%% "cats-laws" % "2.7.0" % Test,
      "org.typelevel" %%% "discipline-scalatest" % "2.1.5" % Test
    )
  )
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val catsJVM = cats.jvm
lazy val catsJS = cats.js
lazy val catsNative = cats.native

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(coreJVM, catsJVM)
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "paiges-benchmark"
  )
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("docs"))
  .dependsOn(coreJVM, catsJVM)
  .enablePlugins(TypelevelSitePlugin)
  .settings(
    name := "paiges-docs",
    mdocIn := (LocalRootProject / baseDirectory).value / "docs" / "src" / "main" / "mdoc"
  )

lazy val commonSettings = Seq(
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

lazy val commonJvmSettings = Seq(
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

lazy val commonJsSettings = Seq(
  coverageEnabled := false
)

lazy val commonNativeSettings = Seq(
  crossScalaVersions := (ThisBuild / crossScalaVersions).value.filter(_.startsWith("2.")),
  // Remove when native is published for the default previous versions
  tlVersionIntroduced := List("2.12", "2.13").map(_ -> "0.4.1").toMap,
  coverageEnabled := false
)
