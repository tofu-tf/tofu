import sbt._
import Keys._
import org.apache.ivy.core.module.descriptor.ExcludeRule

object Dependencies {
  val minorVersion = SettingKey[Int]("minor scala version")

  object Version {
    val scala212 = "2.12.18"

    val scala213 = "2.13.12"

    val scala3 = "3.3.1"

    val circe = "0.14.5"

    val tethys = "0.28.0"

    val cats = "2.9.0"

    val catsEffect2 = "2.5.5"

    val catsEffect3 = "3.4.9"

    val catsMtl = "1.3.1"

    val catsTagless = "0.15.0"

    val enumeratum = "1.7.3"

    val derevo = "0.13.0"

    val slf4j = "2.0.7"

    val fs2 = "2.5.11"

    val fs2CE3 = "3.7.0"

    val logback = "1.3.11"

    val monix = "3.4.1"

    val scalatest = "3.2.16"

    val magnolia2 = "1.1.6"

    val magnolia3 = "1.3.4"

    val typesafeConfig = "1.4.2"

    val zio = "1.0.18"

    val zio2 = "2.0.15"

    val zioCats = "2.5.1.0"

    val zio2Cats = "23.0.0.4"

    val shapeless = "2.3.10"

    val refined = "0.11.0"

    val doobie = "0.13.4"

    val doobieCE3 = "1.0.0-RC2"

    // Compile time only
    val macroParadise = "2.1.1"

    val simulacrum = "1.0.1"

    val kindProjector = "0.13.2"

    val betterMonadicFor = "0.3.1"

    val collectionCompat = "2.11.0"

    val log4CatsLegacy = "1.7.0"

    val log4Cats = "2.6.0"

    val logstashLogback = "7.4"

    val groovy = "3.0.18"

    val http4s = "0.22.15"

    val glass = "0.2.2"
  }

  val noCatsCore        =
    Vector(ExclusionRule("org.typelevel", "cats-core_2.13"), ExclusionRule("org.typelevel", "cats-core_2.12"))
  val noCatsEffect      =
    Vector(
      ExclusionRule("org.typelevel", "cats-effect_2.13"),
      ExclusionRule("org.typelevel", "cats-effect_2.12")
    )
  val catsCore          = "org.typelevel"                %% "cats-core"                % Version.cats
  val catsFree          = "org.typelevel"                %% "cats-free"                % Version.cats
  val catsMtl           = "org.typelevel"                %% "cats-mtl"                 % Version.catsMtl
  val alleycats         = "org.typelevel"                %% "alleycats-core"           % Version.cats
  val catsEffect2       = "org.typelevel"                %% "cats-effect"              % Version.catsEffect2
  val catsEffect3       = "org.typelevel"                %% "cats-effect"              % Version.catsEffect3
  val monix             = "io.monix"                     %% "monix"                    % Version.monix excludeAll (noCatsEffect: _*)
  val logback           = "ch.qos.logback"                % "logback-classic"          % Version.logback
  val slf4j             = "org.slf4j"                     % "slf4j-api"                % Version.slf4j     % Provided
  val circeCore         = "io.circe"                     %% "circe-core"               % Version.circe excludeAll (noCatsCore: _*)
  val circeJava8        = "io.circe"                     %% "circe-java8"              % Version.circe excludeAll (noCatsCore: _*)
  val circeDerivation   = "io.circe"                     %% "circe-derivation"         % Version.circe excludeAll (noCatsCore: _*)
  val magnolia2         = "com.softwaremill.magnolia1_2" %% "magnolia"                 % Version.magnolia2
  val magnolia3         = "com.softwaremill.magnolia1_3" %% "magnolia"                 % Version.magnolia3
  val derevo            = "tf.tofu"                      %% "derevo-core"              % Version.derevo
  val derevoCirce       = "tf.tofu"                      %% "derevo-circe"             % Version.derevo
  val derevoTagless     = "tf.tofu"                      %% "derevo-cats-tagless"      % Version.derevo
  val enumeratum        = "com.beachape"                 %% "enumeratum"               % Version.enumeratum
  val fs2               = "co.fs2"                       %% "fs2-core"                 % Version.fs2
  val fs2CE3            = "co.fs2"                       %% "fs2-core"                 % Version.fs2CE3
  val tethys            = "com.tethys-json"              %% "tethys-core"              % Version.tethys
  val tethysJackson     = "com.tethys-json"              %% "tethys-jackson213"        % Version.tethys
  val catsTaglessMacros = "org.typelevel"                %% "cats-tagless-macros"      % Version.catsTagless
  val catsTaglessCore   = "org.typelevel"                %% "cats-tagless-core"        % Version.catsTagless
  val typesafeConfig    = "com.typesafe"                  % "config"                   % Version.typesafeConfig
  val zio               = "dev.zio"                      %% "zio"                      % Version.zio
  val zio2              = "dev.zio"                      %% "zio"                      % Version.zio2
  val zio2Test          = "dev.zio"                      %% "zio-test"                 % Version.zio2      % Test
  val zio2TestSbt       = "dev.zio"                      %% "zio-test-sbt"             % Version.zio2      % Test
  val zioCats           = "dev.zio"                      %% "zio-interop-cats"         % Version.zioCats
  val zio2Cats          = "dev.zio"                      %% "zio-interop-cats"         % Version.zio2Cats
  val scalatest         = "org.scalatest"                %% "scalatest"                % Version.scalatest % Test
  val shapeless         = "com.chuusai"                  %% "shapeless"                % Version.shapeless
  val refined           = "eu.timepit"                   %% "refined"                  % Version.refined
  val doobieCore        = "org.tpolecat"                 %% "doobie-core"              % Version.doobie
  val doobieH2          = "org.tpolecat"                 %% "doobie-h2"                % Version.doobie
  val doobieCoreCE3     = "org.tpolecat"                 %% "doobie-core"              % Version.doobieCE3
  val doobieH2CE3       = "org.tpolecat"                 %% "doobie-h2"                % Version.doobieCE3
  val collectionCompat  = "org.scala-lang.modules"       %% "scala-collection-compat"  % Version.collectionCompat
  val log4CatsLegacy    = "org.typelevel"                %% "log4cats-core"            % Version.log4CatsLegacy
  val log4Cats          = "org.typelevel"                %% "log4cats-core"            % Version.log4Cats
  val logstashLogback   = "net.logstash.logback"          % "logstash-logback-encoder" % Version.logstashLogback
  val groovy            = "org.codehaus.groovy"           % "groovy"                   % Version.groovy
  val http4s            =
    Seq("org.http4s" %% "http4s-dsl", "org.http4s" %% "http4s-circe", "org.http4s" %% "http4s-blaze-server").map(
      _ % Version.http4s
    )
  val glassCore         = "tf.tofu"                      %% "glass-core"               % Version.glass
  val glassMacro        = "tf.tofu"                      %% "glass-macro"              % Version.glass
  // Compile-time only
  val macroParadise     = "org.scalamacros"               % "paradise"                 % Version.macroParadise cross CrossVersion.patch
  val kindProjector     = "org.typelevel"                %% "kind-projector"           % Version.kindProjector cross CrossVersion.patch
  val simulacrum        = "org.typelevel"                %% "simulacrum"               % Version.simulacrum
  val betterMonadicFor  = "com.olegpy"                   %% "better-monadic-for"       % Version.betterMonadicFor
}
