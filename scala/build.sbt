import Dependencies._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "stoeffel"
ThisBuild / organizationName := "stoeffel"
lazy val root = (project in file("."))
  .settings(
    name := "aoc19",
    libraryDependencies ++= Seq(
      attoCore,
      catsCore,
      catsEffect,
      scalaCheck % Test,
      scalaTest % Test
    )
  )
