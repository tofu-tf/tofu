import sbt._
import Keys._
import org.apache.ivy.core.module.descriptor.ExcludeRule

object Dependencies {
  val minorVersion = SettingKey[Int]("minor scala version")

  object Version {
    val scala212 = "2.12.13"

    val scala213 = "2.13.5"

    val circe = "0.13.0"

    val tethys = "0.23.0"

    val cats = "2.6.0"

    val catsEffect = "2.5.0"

    val catsMtl = "1.2.0"

    val catsTagless = "0.14.0"

    val monocle = "2.1.0"

    val enumeratum = "1.6.1"

    val derevo = "0.12.5"

    val slf4j = "1.7.30"

    val fs2 = "2.5.5"

    val logback = "1.2.3"

    val monix = "3.3.0"

    val scalatest = "3.2.8"

    val magnolia = "0.17.0"

    val typesafeConfig = "1.4.1"

    val zio = "1.0.7"

    val zioCats = "2.4.1.0"

    val shapeless = "2.3.5"

    val refined = "0.9.24"

    val doobie = "0.13.1"

    // Compile time only
    val macroParadise = "2.1.1"

    val simulacrum = "1.0.1"

    val kindProjector = "0.12.0"

    val betterMonadicFor = "0.3.1"

    val collectionCompat = "2.4.3"

    val log4Cats = "1.3.0"
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
  val catsEffect       = "org.typelevel"              %% "cats-effect"             % Version.catsEffect
  val monix            = "io.monix"                   %% "monix"                   % Version.monix excludeAll (noCatsEffect: _*)
  val logback          = "ch.qos.logback"              % "logback-classic"         % Version.logback
  val slf4j            = "org.slf4j"                   % "slf4j-api"               % Version.slf4j     % Provided
  val circeCore        = "io.circe"                   %% "circe-core"              % Version.circe excludeAll (noCatsCore: _*)
  val circeJava8       = "io.circe"                   %% "circe-java8"             % Version.circe excludeAll (noCatsCore: _*)
  val circeDerivation  = "io.circe"                   %% "circe-derivation"        % Version.circe excludeAll (noCatsCore: _*)
  val magnolia         = "com.propensive"             %% "magnolia"                % Version.magnolia
  val derevo           = "tf.tofu"                    %% "derevo-core"             % Version.derevo
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
  val collectionCompat = "org.scala-lang.modules"     %% "scala-collection-compat" % Version.collectionCompat
  val log4Cats         = "org.typelevel"              %% "log4cats-core"           % Version.log4Cats

  // Compile-time only
  val macroParadise    = "org.scalamacros" % "paradise"           % Version.macroParadise cross CrossVersion.patch
  val kindProjector    = "org.typelevel"  %% "kind-projector"     % Version.kindProjector cross CrossVersion.patch
  val simulacrum       = "org.typelevel"  %% "simulacrum"         % Version.simulacrum
  val betterMonadicFor = "com.olegpy"     %% "better-monadic-for" % Version.betterMonadicFor
}
