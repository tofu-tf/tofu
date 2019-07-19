import Publish._, Dependencies._

publishName := "logging-structured"

libraryDependencies ++= List(catsCore,
                             catsEffect,
                             circeCore,
                             tethys,
                             tethysJackson,
                             macros,
                             slf4j,
                             alleycats,
                             scalatest,
                             derevo,
                             catsTagless)
