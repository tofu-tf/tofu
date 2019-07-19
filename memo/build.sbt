import Publish._, Dependencies._

publishName := "memo"

libraryDependencies ++= Seq(catsCore, catsEffect, simulacrum, monix % Provided, macros)

sources in (Compile, doc) := Seq.empty