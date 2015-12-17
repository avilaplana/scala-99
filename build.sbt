name := """scala-99"""

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies ++=
  Seq(
    "com.typesafe.play" % "play-json_2.11" % "2.4.6" % "compile",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"
  )