import Publish._, Dependencies._
import com.typesafe.sbt.SbtGit.git

scalaVersion := "2.12.8"

val libVersion = "0.2.0"

lazy val setMinorVersion = minorVersion := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) => v.toInt
    case _            => 0
  }
}

lazy val setModuleName = moduleName := { s"tofu-${(publishName or name).value}" }
lazy val experimental  = scalacOptions ++= { if (scalaVersion.value < "2.12") List("-Xexperimental") else Nil }

lazy val defaultSettings = List(
  setMinorVersion,
  setModuleName,
  experimental,
  defaultScalacOptions,
  scala11Options,
  libraryDependencies += compilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
  libraryDependencies += compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0"),
) ++ publishSettings ++ scala213Options

lazy val compile213 = crossScalaVersions += "2.13.0"

moduleName := "tofu"

lazy val core = project dependsOn (opticsCore) settings (
  defaultSettings,
  compile213,
  publishName := "core",
  libraryDependencies ++= Seq(simulacrum, catsCore, catsEffect, catsTagless),
  macros
)

lazy val memo = project
  .dependsOn(core, concurrent)
  .settings(
    defaultSettings,
    compile213,
    libraryDependencies ++= Seq(catsCore, catsEffect, simulacrum),
    macros
  )

lazy val loggingStr = project
  .in(file("logging/structured"))
  .settings(
    publishName := "logging-structured",
    defaultSettings,
    compile213,
    libraryDependencies ++= List(
      catsCore,
      catsEffect,
      circeCore,
      tethys,
      tethysJackson,
      slf4j,
      alleycats,
      scalatest,
      derevo,
      catsTagless,
    ),
    macros,
  )
  .dependsOn(core, data)

lazy val loggingDer = project
  .in(file("logging/derivation"))
  .dependsOn(loggingStr)
  .settings(
    defaultSettings,
    compile213,
    libraryDependencies ++= List(derevo, scalatest),
    magnolia,
    macros,
    publishName := "logging-derivation"
  )

lazy val loggingLayout = project
  .in(file("logging/layout"))
  .settings(
    defaultSettings,
    compile213,
    libraryDependencies ++= List(catsCore, catsEffect, simulacrum, logback, slf4j),
    macros,
    publishName := "logging-layout"
  )
  .dependsOn(loggingStr)

lazy val logging = project
  .dependsOn(loggingStr, loggingDer, loggingLayout)
  .aggregate(loggingStr, loggingDer, loggingLayout)
  .settings(defaultSettings, compile213)

lazy val env = project
  .dependsOn(core, memo)
  .settings(defaultSettings, libraryDependencies ++= List(catsCore, catsEffect, monix, scalatest))

lazy val observable = project.settings(
  defaultSettings,
  libraryDependencies += monix,
  libraryDependencies += scalatest,
)
lazy val parallel =
  project.settings(defaultSettings, compile213, libraryDependencies ++= List(simulacrum, catsCore), macros)

lazy val concurrent =
  project dependsOn (core, parallel) settings (
    defaultSettings,
    compile213,
    libraryDependencies ++= List(catsEffect, catsTagless, simulacrum, scalatest),
    macros,
)

lazy val coreModules   = List(core, memo, env, parallel, concurrent, opticsCore, data)
lazy val commonModules = List(observable, opticsInterop, opticsMacro, logging, enums)

lazy val opticsCore = project
  .in(file("optics/core"))
  .settings(
    defaultSettings,
    compile213,
    libraryDependencies ++= Seq(catsCore, alleycats),
    publishName := "optics-core"
  )

lazy val opticsInterop = project
  .in(file("optics/interop"))
  .dependsOn(opticsCore)
  .settings(defaultSettings, libraryDependencies += monocle, publishName := "optics-interop")

lazy val opticsMacro = project
  .in(file("optics/macro"))
  .dependsOn(opticsCore)
  .settings(defaultSettings, compile213, macros, publishName := "optics-macro")

lazy val enums = project
  .dependsOn(loggingStr)
  .settings(
    defaultSettings,
    compile213,
    libraryDependencies ++= List(enumeratum)
  )

lazy val data =
  project.settings(defaultSettings, compile213, libraryDependencies += catsFree).dependsOn(core, opticsCore)

lazy val tofu = project
  .in(file("."))
  .settings(defaultSettings)
  .aggregate((coreModules ++ commonModules).map(x => x: ProjectReference): _*)
  .dependsOn(coreModules.map(x => x: ClasspathDep[ProjectReference]): _*)

libraryDependencies += scalatest

lazy val scala213Options = List(
  scalacOptions ++= {
    minorVersion.value match {
      case 13 => List("-Ymacro-annotations")
      case 11 | 12 =>
        List(
          "-Yno-adapted-args",                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
          "-Ypartial-unification",            // Enable partial unification in type constructor inference
          "-Ywarn-inaccessible",              // Warn about inaccessible types in method signatures.
          "-Ywarn-infer-any",                 // Warn when a type argument is inferred to be `Any`.
          "-Ywarn-nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Ywarn-nullary-unit",              // Warn when nullary methods return Unit.
          "-Ywarn-numeric-widen",             // Warn when numerics are widened.
          "-Ywarn-value-discard",             // Warn when non-Unit expression results are unused.
          "-Xlint:unsound-match",             // Pattern match may not be typesafe.
          "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
          "-Xfuture",                         // Turn on future language features.
        )
    }
  },
)

lazy val defaultScalacOptions = scalacOptions ++= List(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8",                         // Specify character encoding used by source files.
  "-explaintypes",                 // Explain type errors in more detail.
  "-feature",                      // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds",         // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
  "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver.
  "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
  "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",       // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",        // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
)

lazy val scala11Options = scalacOptions ++= {
  minorVersion.value match {
    case 12 | 13 =>
      List(
        "-Xlint:constant",         // Evaluation of a constant arithmetic expression results in an error.
        "-Ywarn-unused:imports",   // Warn if an import selector is not referenced.
        "-Ywarn-unused:locals",    // Warn if a local definition is unused.
        "-Ywarn-unused:params",    // Warn if a value parameter is unused.
        "-Ywarn-unused:patvars",   // Warn if a variable bound in a pattern is unused.
        "-Ywarn-unused:privates",  // Warn if a private member is unused.
        "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
        "-Ywarn-extra-implicit"    // Warn when more than one implicit parameter section is defined.
      )
    case 11 => List()
  }
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
    )
  ),
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/TinkoffCreditSystems/tofu")),
  developers := List(
    Developer("odomontois", "Oleg Nizhnik", "odomontois@gmail.com", url("https://github.com/odomontois"))
  )
)
