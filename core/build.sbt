import Publish._,  Dependencies._

libraryDependencies ++= Seq(simulacrum, macros, catsCore, catsEffect, catsTagless)

publishName := "core"

sources in (Compile, doc) := Seq.empty