import Dependencies._
import Publish.publishName
libraryDependencies ++= Seq(catsCore, catsEffect, simulacrum, macros, logback, logbackGelf, slf4j)

publishName := "logging-layout"