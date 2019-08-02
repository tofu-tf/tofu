import sbt._

object Dependencies {
  val minorVersion = SettingKey[Int]("minor scala version")

  val circeVersion       = "0.12.0-M3"
  val tethysVersion      = "0.10.0"
  val catsVersion        = "2.0.0-M4"
  val catsEffectVersion  = "2.0.0-M4"
  val catsTaglessVersion = "0.8"
  val monocleVersion     = "1.5.1-cats"
  val enumeratumVersion  = "1.5.13"
  val derevoVersion      = "0.9.1"
  val slf4jVersion       = "1.7.26"
  val fs2Version         = "1.0.5"

  val catsCore        = "org.typelevel"              %% "cats-core"        % catsVersion
  val catsFree        = "org.typelevel"              %% "cats-free"        % catsVersion
  val monocle         = "com.github.julien-truffaut" %% "monocle-core"     % monocleVersion
  val alleycats       = "org.typelevel"              %% "alleycats-core"   % catsVersion
  val catsEffect      = "org.typelevel"              %% "cats-effect"      % catsEffectVersion
  val monix           = "io.monix"                   %% "monix"            % "3.0.0-RC2"
  val simulacrum      = "com.github.mpilquist"       %% "simulacrum"       % "0.19.0"
  val logback         = "ch.qos.logback"             % "logback-classic"   % "1.2.3"
  val slf4j           = "org.slf4j"                  % "slf4j-simple"      % slf4jVersion
  val circeCore       = "io.circe"                   %% "circe-core"       % circeVersion
  val circeJava8      = "io.circe"                   %% "circe-java8"      % circeVersion
  val circeDerivation = "io.circe"                   %% "circe-derivation" % "0.10.0-M1"
  val scalatest       = "org.scalatest"              %% "scalatest"        % "3.0.8" % Test
  val scalamock       = "org.scalamock"              %% "scalamock"        % "4.1.0" % Test

  val derevo        = "org.manatki"   %% "derevo-core"         % derevoVersion
  val derevoTagless = "org.manatki"   %% "derevo-cats-tagless" % derevoVersion
  val enumeratum    = "com.beachape"  %% "enumeratum"          % enumeratumVersion
  val catsTagless   = "org.typelevel" %% "cats-tagless-macros" % catsTaglessVersion

  val fs2 = "co.fs2" %% "fs2-io" % fs2Version

  val tethys        = "com.tethys-json" %% "tethys-core"    % tethysVersion
  val tethysJackson = "com.tethys-json" %% "tethys-jackson" % tethysVersion

  val oracleJbdc = "com.oracle"           % "ojdbc6"      % "11.2.0.4.0"
  val couchbase  = "com.couchbase.client" % "java-client" % "2.5.8"

  val macros = Keys.libraryDependencies ++= {
    minorVersion.value match {
      case 13      => List()
      case 11 | 12 => List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
    }
  }

  val magnolia = Keys.libraryDependencies += "com.propensive" %% "magnolia" % {
    minorVersion.value match {
      case 12 | 13 => "0.11.0"
      case 11      => "0.10.0"
    }
  }
}
