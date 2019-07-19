import sbt._

object Dependencies {
  val circeVersion       = "0.11.1"
  val tethysVersion      = "0.9.0.1"
  val catsVersion        = "1.6.0"
  val catsEffectVersion  = "1.2.0"
  val catsTaglessVersion = "0.8"
  val monocleVersion     = "1.5.1-cats"
  val enumeratumVersion  = "1.5.13"
  val derevoVersion      = "0.8.0"
  val slf4jVersion       = "1.7.26"

  val catsCore        = "org.typelevel"              %% "cats-core"           % catsVersion
  val monocle         = "com.github.julien-truffaut" %% "monocle-core"        % monocleVersion
  val alleycats       = "org.typelevel"              %% "alleycats-core"      % catsVersion
  val catsEffect      = "org.typelevel"              %% "cats-effect"         % catsEffectVersion
  val monix           = "io.monix"                   %% "monix"               % "3.0.0-RC2"
  val simulacrum      = "com.github.mpilquist"       %% "simulacrum"          % "0.16.0"
  val logback         = "ch.qos.logback"             % "logback-classic"      % "1.2.3"
  val logbackGelf     = "me.moocar"                  % "logback-gelf"         % "0.3"
  val slf4j           = "org.slf4j"                  % "slf4j-simple"         % slf4jVersion
  val circeCore       = "io.circe"                   %% "circe-core"          % circeVersion
  val circeJava8      = "io.circe"                   %% "circe-java8"         % circeVersion
  val circeDerivation = "io.circe"                   %% "circe-derivation"    % "0.10.0-M1"
  val scalatest       = "org.scalatest"              %% "scalatest"           % "3.0.7" % Test
  val scalamock       = "org.scalamock"              %% "scalamock"           % "4.1.0" % Test
  val magnolia        = "com.propensive"             %% "magnolia"            % "0.10.0"
  val derevo          = "org.manatki"                %% "derevo-core"         % derevoVersion
  val derevoTagless   = "org.manatki"                %% "derevo-cats-tagless" % derevoVersion
  val enumeratum      = "com.beachape"               %% "enumeratum"          % enumeratumVersion
  val catsTagless     = "org.typelevel"              %% "cats-tagless-macros" % catsTaglessVersion

  val tethys        = "com.tethys-json" %% "tethys-core"    % tethysVersion
  val tethysJackson = "com.tethys-json" %% "tethys-jackson" % tethysVersion

  val oracleJbdc = "com.oracle"           % "ojdbc6"      % "11.2.0.4.0"
  val couchbase  = "com.couchbase.client" % "java-client" % "2.5.8"

  val macros = compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
}
