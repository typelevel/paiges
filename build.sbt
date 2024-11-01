val Scala212 = "2.12.20"
val Scala213 = "2.13.14"
val Scala3Version = "3.3.4"

ThisBuild / tlBaseVersion := "0.4"

ThisBuild / startYear := Some(2017)
ThisBuild / scalaVersion := Scala213
ThisBuild / tlVersionIntroduced := Map("3" -> "0.4.2")
ThisBuild / crossScalaVersions := Seq(Scala213, Scala212, Scala3Version)

// Setup coverage
ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List("2.13.12"),
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

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    commonSettings,
    name := "paiges-core",
    moduleName := "paiges-core",
    libraryDependencies ++= Seq(
      "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % Test,
      "org.scalatest" %%% "scalatest-funsuite" % "3.2.19" % Test
    ),
    // TODO: 2.13 has warnings for using Stream, but scalacheck Shrink
    tlFatalWarnings := scalaVersion.value.startsWith("2.12."),
    mimaBinaryIssueFilters ++= {
      if (tlIsScala3.value) {
        import com.typesafe.tools.mima.core._
        Seq(
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.typelevel.paiges.Chunk*"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("org.typelevel.paiges.Chunk#ChunkStream#3#Empty.this")
        )
      } else Nil
    }
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
      "org.typelevel" %%% "cats-core" % "2.12.0",
      "org.typelevel" %%% "cats-laws" % "2.12.0" % Test,
      "org.typelevel" %%% "discipline-scalatest" % "2.3.0" % Test
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
  )
)

lazy val commonJvmSettings = Seq(
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

lazy val commonJsSettings = Seq(
  coverageEnabled := false
)

lazy val commonNativeSettings = Seq(
  // Remove when native is published for the default previous versions
  tlVersionIntroduced := List("2.12", "2.13", "3").map(_ -> "0.5.0").toMap,
  coverageEnabled := false
)
