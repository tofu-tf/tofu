import Publish._, Dependencies._
import com.typesafe.sbt.SbtGit.git

val libVersion = "0.5.2"

lazy val setMinorVersion = minorVersion := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) => v.toInt
    case _            => 0
  }
}

lazy val setModuleName = moduleName := { s"tofu-${(publishName or name).value}" }
lazy val experimental  = scalacOptions ++= { if (scalaVersion.value < "2.12") List("-Xexperimental") else Nil }

val macros = Keys.libraryDependencies ++= {
  minorVersion.value match {
    case 13 => List(scalaOrganization.value % "scala-reflect" % scalaVersion.value)
    case 12 =>
      List(
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
        scalaOrganization.value % "scala-reflect" % scalaVersion.value
      )
  }
}

lazy val defaultSettings = List(
  scalaVersion := "2.13.1",
  setMinorVersion,
  setModuleName,
  experimental,
  defaultScalacOptions,
  libraryDependencies += compilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.0" cross CrossVersion.patch),
  libraryDependencies += compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
  libraryDependencies += scalatest,
  libraryDependencies ++=
    List(
      compilerPlugin("com.github.ghik" % "silencer-plugin" % Version.silencer cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % Version.silencer % Provided cross CrossVersion.full
    )
) ++ publishSettings ++ scala213Options

moduleName := "tofu"

lazy val core = project dependsOn (opticsCore) settings (
  defaultSettings,
  publishName := "core",
  libraryDependencies ++= Seq(simulacrum, catsCore, catsEffect, catsTagless),
  macros
)

lazy val memo = project
  .dependsOn(core, concurrent)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, catsEffect, simulacrum),
    macros
  )

lazy val loggingStr = project
  .in(file("logging/structured"))
  .settings(
    publishName := "logging-structured",
    defaultSettings,
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
    libraryDependencies ++= List(derevo, magnolia),
    macros,
    publishName := "logging-derivation"
  )

lazy val loggingLayout = project
  .in(file("logging/layout"))
  .settings(
    defaultSettings,
    libraryDependencies ++= List(catsCore, catsEffect, simulacrum, logback, slf4j),
    macros,
    publishName := "logging-layout"
  )
  .dependsOn(loggingStr)

lazy val logging = project
  .dependsOn(loggingStr, loggingDer, loggingLayout)
  .aggregate(loggingStr, loggingDer, loggingLayout)
  .settings(defaultSettings)

lazy val env = project
  .dependsOn(core, memo)
  .settings(defaultSettings, libraryDependencies ++= List(catsCore, catsEffect, monix))

lazy val observable = project.settings(
  defaultSettings,
  libraryDependencies += monix,
  libraryDependencies += scalatest,
)

lazy val concurrent =
  project dependsOn (core) settings (
    defaultSettings,
    libraryDependencies ++= List(catsEffect, catsTagless, simulacrum),
    macros,
)

lazy val config = project dependsOn (core, data, opticsCore, concurrent) settings (
  defaultSettings,
  libraryDependencies ++= List(typesafeConfig, magnolia, derevo),
  macros,
)



lazy val opticsCore = project
  .in(file("optics/core"))
  .settings(
    defaultSettings,
    libraryDependencies ++= List(catsCore, alleycats),
    publishName := "optics-core",
  )

lazy val opticsInterop = project
  .in(file("optics/interop"))
  .dependsOn(opticsCore)
  .settings(defaultSettings, libraryDependencies += monocle, publishName := "optics-interop")

lazy val opticsMacro = project
  .in(file("optics/macro"))
  .dependsOn(opticsCore)
  .settings(
    defaultSettings,
    scalacOptions --= List(
      "-Ywarn-unused:params",
      "-Ywarn-unused:patvars",
    ),
    macros,
    publishName := "optics-macro"
  )

lazy val enums = project
  .dependsOn(loggingStr)
  .settings(
    defaultSettings,
    libraryDependencies ++= List(enumeratum)
  )

lazy val data =
  project
    .settings(defaultSettings, libraryDependencies ++= List(catsFree))
    .dependsOn(core, opticsCore)

lazy val derivation =
  project
    .settings(
      defaultSettings,
      libraryDependencies ++= List(magnolia, derevo, catsTagless),
      macros,
      publishName := "derivation",
    )
    .dependsOn(data)

lazy val zioCore =
  project.in(file("zio/core")).settings(defaultSettings, libraryDependencies += zio).dependsOn(core)

lazy val zioInterop = project
  .in(file("zio"))
  .settings(defaultSettings)
  .dependsOn(zioCore)
  .aggregate(zioCore)


lazy val coreModules = List(core, memo, env, concurrent, opticsCore, data)

lazy val commonModules = List(observable, opticsInterop, opticsMacro, logging, enums, config, derivation, zioInterop)

lazy val allModuleRefs = (coreModules ++ commonModules).map(x => x: ProjectReference)
lazy val allModuleDeps = (coreModules ++ commonModules).map(x => x: ClasspathDep[ProjectReference])

lazy val docs = project // new documentation project
  .in(file("tofu-docs"))
  .settings(
    defaultSettings,
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(allModuleRefs: _*),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(unidoc in Compile).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value,
  )
  .dependsOn(allModuleDeps:_*)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

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
      case 12 =>
        List(
          "-Yno-adapted-args",                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
          "-Ypartial-unification",            // Enable partial unification lype constructor inference
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
  "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
  "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",          // Warn if a local definition is unused.
  "-Ywarn-unused:params",          // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",        // Warn if a private member is unused.
  "-Ywarn-unused:implicits",       // Warn if an implicit parameter is unused.
  "-Ywarn-extra-implicit",         // Warn when more than one implicit parameter section is defined.
)

lazy val publishSettings = List(
  organization := "ru.tinkoff",
  publishVersion := libVersion,
  publishMavenStyle := true,
  description := "Opinionated Set of tool for functional programming in scala",
  crossScalaVersions := List("2.12.10", "2.13.1"),
  publishTo := {
    if (isSnapshot.value) {
      Some(Opts.resolver.sonatypeSnapshots)
    } else sonatypePublishToBundle.value
  },
  credentials += Credentials(Path.userHome / ".sbt" / ".ossrh-credentials"),
  version := {
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
    Developer("odomontois", "Oleg Nizhnik", "odomontois@gmail.com", url("https://github.com/odomontois")),
    Developer("danslapman", "Daniil Smirnov", "danslapman@gmail.com", url("https://github.com/danslapman")),
    Developer("Phill101", "Nikita Filimonov", "holypics6@gmail.com", url("https://github.com/Phill101"))
  )
)
