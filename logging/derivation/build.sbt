import Dependencies._
import Publish.publishName
libraryDependencies ++= List(magnolia, derevo, macros, scalatest)

publishName := "logging-derivation"