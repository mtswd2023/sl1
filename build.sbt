ThisBuild / version := "0.0.1"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "l1",
  )

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0"
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
