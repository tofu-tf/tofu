import Publish._, Dependencies._

libraryDependencies ++= Seq(catsCore, catsEffect, simulacrum, macros, logback, logbackGelf, slf4j)

sources in (Compile, doc) := Seq.empty

publishName := "logging"