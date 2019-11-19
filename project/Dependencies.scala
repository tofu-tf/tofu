import sbt._
import Keys._

object Dependencies {
  val minorVersion = SettingKey[Int]("minor scala version")

  object Version {
    val circe = "0.12.3"

    val tethys = "0.10.0"

    val cats = "2.0.0"

    val catsEffect = "2.0.0"

    val catsTagless = "0.10"

    val monocle = "2.0.0"

    val enumeratum = "1.5.13"

    val derevo = "0.10.5"

    val slf4j = "1.7.29"

    val fs2 = "1.0.5"

    val logback = "1.2.3"

    val simulacrum = "1.0.0"

    val monix = "3.1.0"

    val scalatest = "3.0.8"

    val magnolia = "0.12.0"

    val typesafeConfig = "1.4.0"

    val silencer = "1.4.4"

    val zio = "1.0.0-RC14"
  }

  val catsCore        = "org.typelevel"              %% "cats-core"           % Version.cats
  val catsFree        = "org.typelevel"              %% "cats-free"           % Version.cats
  val monocle         = "com.github.julien-truffaut" %% "monocle-core"        % Version.monocle
  val alleycats       = "org.typelevel"              %% "alleycats-core"      % Version.cats
  val catsEffect      = "org.typelevel"              %% "cats-effect"         % Version.catsEffect
  val monix           = "io.monix"                   %% "monix"               % Version.monix
  val simulacrum      = "org.typelevel"              %% "simulacrum"          % Version.simulacrum
  val logback         = "ch.qos.logback"             % "logback-classic"      % Version.logback
  val slf4j           = "org.slf4j"                  % "slf4j-simple"         % Version.slf4j % Provided
  val circeCore       = "io.circe"                   %% "circe-core"          % Version.circe
  val circeJava8      = "io.circe"                   %% "circe-java8"         % Version.circe
  val circeDerivation = "io.circe"                   %% "circe-derivation"    % Version.circe
  val magnolia        = "com.propensive"             %% "magnolia"            % Version.magnolia
  val derevo          = "org.manatki"                %% "derevo-core"         % Version.derevo
  val derevoTagless   = "org.manatki"                %% "derevo-cats-tagless" % Version.derevo
  val enumeratum      = "com.beachape"               %% "enumeratum"          % Version.enumeratum
  val fs2             = "co.fs2"                     %% "fs2-io"              % Version.fs2
  val tethys          = "com.tethys-json"            %% "tethys-core"         % Version.tethys
  val tethysJackson   = "com.tethys-json"            %% "tethys-jackson"      % Version.tethys
  val catsTagless     = "org.typelevel"              %% "cats-tagless-macros" % Version.catsTagless
  val typesafeConfig  = "com.typesafe"               % "config"               % Version.typesafeConfig
  val zio             = "dev.zio"                    %% "zio"                 % Version.zio
  val scalatest       = "org.scalatest"              %% "scalatest"           % Version.scalatest % Test
}
