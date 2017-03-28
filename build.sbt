import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.johnynek",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Paiges",
    scalacOptions := Seq("-feature", "-language:_"),
    libraryDependencies ++=
      List(scalaTest % Test,
        scalaCheck % Test)
  )
