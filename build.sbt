name := "scala-labs"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
