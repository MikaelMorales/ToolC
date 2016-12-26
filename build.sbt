name := "toolc"

mainClass in (Compile, run) := Some("toolc.Main")

mainClass in (Compile, packageBin) := Some("toolc.Main")

version := "3.0"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"
