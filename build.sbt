import Publish._, Dependencies._
import com.typesafe.sbt.SbtGit.git

name := "tofu"

scalaVersion := "2.12.8"

val libVersion = "0.1"

lazy val setModuleName = moduleName := { s"tofu-${(publishName or name).value}" }
lazy val experimental  = scalacOptions ++= { if (scalaVersion.value < "2.12") List("-Xexperimental") else Nil }

lazy val defaultSettings = List(setModuleName, experimental, defaultScalacOptions, scalac212Options) ++ publishSettings

moduleName := "tofu"

lazy val core = project dependsOn (opticsCore) settings defaultSettings
lazy val memo = project dependsOn (core) settings defaultSettings

lazy val loggingStr    = project.in(file("logging/structured")) settings defaultSettings dependsOn (core, data)
lazy val loggingDer    = project.in(file("logging/derivation")) settings defaultSettings dependsOn loggingStr
lazy val loggingLayout = project.in(file("logging/layout")) settings defaultSettings dependsOn loggingStr
lazy val logging = project dependsOn
  (loggingStr, loggingDer, loggingLayout) aggregate
  (loggingStr, loggingDer, loggingLayout) settings defaultSettings

lazy val env        = project dependsOn (core, memo) settings defaultSettings
lazy val observable = project settings defaultSettings
lazy val parallel   = project settings defaultSettings
lazy val concurrent = project dependsOn (core, parallel) settings defaultSettings

lazy val coreModules   = Seq(core, memo, env, parallel, concurrent, opticsCore, data)
lazy val commonModules = Seq(observable, opticsInterop, logging, enums)

lazy val opticsCore    = project.in(file("optics/core")) settings defaultSettings
lazy val opticsInterop = project.in(file("optics/interop")) dependsOn (opticsCore) settings defaultSettings

lazy val enums = project settings defaultSettings dependsOn loggingStr
lazy val data  = project settings defaultSettings dependsOn (core, opticsCore)

lazy val fputils = project
  .in(file("."))
  .aggregate((coreModules ++ commonModules).map(x => x: ProjectReference): _*)
  .dependsOn(coreModules.map(x => x: ClasspathDep[ProjectReference]): _*)

libraryDependencies in ThisBuild += compilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3")
libraryDependencies in ThisBuild += compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0")

libraryDependencies += scalatest

sources in (Compile, doc) := Seq.empty

lazy val defaultScalacOptions = scalacOptions ++= List(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfuture", // Turn on future language features.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification", // Enable partial unification in type constructor inference
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
)

lazy val scalac212Options = scalacOptions ++= {
  val version = scalaVersion.value
  if (version >= "2.12")
    List(
      "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
      "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
      "-Ywarn-unused:locals", // Warn if a local definition is unused.
      "-Ywarn-unused:params", // Warn if a value parameter is unused.
      "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates", // Warn if a private member is unused.
      "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
      "-Ywarn-extra-implicit" // Warn when more than one implicit parameter section is defined.
    )
  else Nil
}

lazy val publishSettings = List(
  organization in ThisBuild := "ru.tinkoff",
  publishVersion := libVersion,
  publishMavenStyle in ThisBuild := true,
  description := "Opinionated Set of tool for functional programming in scala",
  crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.8"),
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),
  credentials in ThisBuild += Credentials(Path.userHome / ".sbt" / ".ossrh-credentials"),
  version in ThisBuild := {
    val branch = git.gitCurrentBranch.value
    if (branch == "master") publishVersion.value
    else s"${publishVersion.value}-$branch-SNAPSHOT"
  },
  sources in (Compile, doc) := Seq.empty,
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/TinkoffCreditSystems/tofu"),
      "git@github.com:TinkoffCreditSystems/tofu.git"
    )),
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/TinkoffCreditSystems/tofu")),
  developers := List(
    Developer("odomontois", "Oleg Nizhnik", "odomontois@gmail.com", url("https://github.com/odomontois"))
  )
)
