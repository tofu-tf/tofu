import Publish._, Dependencies._

publishName := "env"

libraryDependencies ++= List(catsCore, catsEffect, monix)

libraryDependencies += scalatest

sources in (Compile, doc) := Seq.empty
