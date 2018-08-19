import Dependencies._

ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.12.6"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "console-game",
    libraryDependencies ++= List(jansi, jline3),
  )
