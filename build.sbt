name := "register"

version := "1.0"

scalaVersion := "2.13.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"

javacOptions  ++= Seq("-source",  "11")
scalacOptions ++= Seq("-release", "11", "-Werror", "-deprecation", "-unchecked")

trapExit := false
