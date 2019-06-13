import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scatto",
    libraryDependencies += scalaTest % Test
  )

initialCommands in console := """import scatto.parser._; import scatto.parser.MonadInstances._; import scatto.parser.MonadPlusInstances._;"""

