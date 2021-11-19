import sbt._
import Keys._
import org.apache.ivy.core.module.descriptor.ExcludeRule

object Dependencies {
  val minorVersion = SettingKey[Int]("minor scala version")

  object Version {
    val scala212 = "2.12.15"

    val scala213 = "2.13.6"

    val circe = "0.14.1"

    val tethys = "0.25.0"

    val cats = "2.6.1"

    val catsEffect2 = "2.5.4"

    val catsEffect3 = "3.1.1"

    val catsMtl = "1.2.1"

    val catsTagless = "0.14.0"

    val monocle = "2.1.0"

    val enumeratum = "1.7.0"

    val derevo = "0.12.8"

    val slf4j = "1.7.32"

    val fs2 = "2.5.10"

    val logback = "1.2.6"

    val monix = "3.4.0"

    val scalatest = "3.2.10"

    val magnolia = "0.17.0"

    val typesafeConfig = "1.4.1"

    val zio = "1.0.12"

    val zioCats = "2.5.1.0"

    val shapeless = "2.3.7"

    val refined = "0.9.27"

    val doobie = "0.13.4"

    // Compile time only
    val macroParadise = "2.1.1"

    val simulacrum = "1.0.1"

    val kindProjector = "0.13.2"

    val betterMonadicFor = "0.3.1"

    val collectionCompat = "2.5.0"

    val log4Cats = "1.3.1"

    val groovy = "3.0.9"

    val http4s = "0.22.7"
  }

  val noCatsCore       =
    Vector(ExclusionRule("org.typelevel", "cats-core_2.13"), ExclusionRule("org.typelevel", "cats-core_2.12"))
  val noCatsEffect     =
    Vector(
      ExclusionRule("org.typelevel", "cats-effect_2.13"),
      ExclusionRule("org.typelevel", "cats-effect_2.12")
    )
  val catsCore         = "org.typelevel"              %% "cats-core"               % Version.cats
  val catsFree         = "org.typelevel"              %% "cats-free"               % Version.cats
  val catsMtl          = "org.typelevel"              %% "cats-mtl"                % Version.catsMtl
  val monocle          = "com.github.julien-truffaut" %% "monocle-core"            % Version.monocle excludeAll (noCatsCore: _*)
  val alleycats        = "org.typelevel"              %% "alleycats-core"          % Version.cats
  val catsEffect2      = "org.typelevel"              %% "cats-effect"             % Version.catsEffect2
  val catsEffect3      = "org.typelevel"              %% "cats-effect"             % Version.catsEffect3
  val monix            = "io.monix"                   %% "monix"                   % Version.monix excludeAll (noCatsEffect: _*)
  val logback          = "ch.qos.logback"              % "logback-classic"         % Version.logback
  val slf4j            = "org.slf4j"                   % "slf4j-api"               % Version.slf4j     % Provided
  val circeCore        = "io.circe"                   %% "circe-core"              % Version.circe excludeAll (noCatsCore: _*)
  val circeJava8       = "io.circe"                   %% "circe-java8"             % Version.circe excludeAll (noCatsCore: _*)
  val circeDerivation  = "io.circe"                   %% "circe-derivation"        % Version.circe excludeAll (noCatsCore: _*)
  val magnolia         = "com.propensive"             %% "magnolia"                % Version.magnolia
  val derevo           = "tf.tofu"                    %% "derevo-core"             % Version.derevo
  val derevoCirce      = "tf.tofu"                    %% "derevo-circe"            % Version.derevo
  val derevoTagless    = "tf.tofu"                    %% "derevo-cats-tagless"     % Version.derevo
  val enumeratum       = "com.beachape"               %% "enumeratum"              % Version.enumeratum
  val fs2              = "co.fs2"                     %% "fs2-io"                  % Version.fs2
  val tethys           = "com.tethys-json"            %% "tethys-core"             % Version.tethys
  val tethysJackson    = "com.tethys-json"            %% "tethys-jackson"          % Version.tethys
  val catsTagless      = "org.typelevel"              %% "cats-tagless-macros"     % Version.catsTagless
  val typesafeConfig   = "com.typesafe"                % "config"                  % Version.typesafeConfig
  val zio              = "dev.zio"                    %% "zio"                     % Version.zio
  val zioCats          = "dev.zio"                    %% "zio-interop-cats"        % Version.zioCats
  val scalatest        = "org.scalatest"              %% "scalatest"               % Version.scalatest % Test
  val shapeless        = "com.chuusai"                %% "shapeless"               % Version.shapeless
  val refined          = "eu.timepit"                 %% "refined"                 % Version.refined
  val doobieCore       = "org.tpolecat"               %% "doobie-core"             % Version.doobie
  val doobieH2         = "org.tpolecat"               %% "doobie-h2"               % Version.doobie
  val collectionCompat = "org.scala-lang.modules"     %% "scala-collection-compat" % Version.collectionCompat
  val log4Cats         = "org.typelevel"              %% "log4cats-core"           % Version.log4Cats
  val groovy           = "org.codehaus.groovy"         % "groovy"                  % Version.groovy
  val http4s           =
    Seq("org.http4s" %% "http4s-dsl", "org.http4s" %% "http4s-circe", "org.http4s" %% "http4s-blaze-server").map(
      _ % Version.http4s
    )
  // Compile-time only
  val macroParadise    = "org.scalamacros"             % "paradise"                % Version.macroParadise cross CrossVersion.patch
  val kindProjector    = "org.typelevel"              %% "kind-projector"          % Version.kindProjector cross CrossVersion.patch
  val simulacrum       = "org.typelevel"              %% "simulacrum"              % Version.simulacrum
  val betterMonadicFor = "com.olegpy"                 %% "better-monadic-for"      % Version.betterMonadicFor
}
