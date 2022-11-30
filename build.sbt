import Publish._, Dependencies._
import com.typesafe.sbt.SbtGit.git
import sbt.ModuleID

lazy val setMinorVersion = minorVersion := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) => v.toInt
    case _            => 0
  }
}

lazy val defaultSettings = Seq(
  scalaVersion       := Version.scala213,
  crossScalaVersions := Vector(Version.scala212, Version.scala213),
  setMinorVersion,
  defaultScalacOptions,
  scalacWarningConfig,
  Compile / doc / scalacOptions -= "-Xfatal-warnings",
  libraryDependencies ++= Seq(
    compilerPlugin(kindProjector),
    compilerPlugin(betterMonadicFor),
    scalatest,
    collectionCompat,
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
  )
) ++ macros

lazy val higherKindCore = project
  .in(file("modules/higherKindCore"))
  .settings(
    defaultSettings,
    name := "tofu-core-higher-kind",
    libraryDependencies ++= Seq(catsCore, catsFree, catsTagless),
  )

lazy val kernel = project
  .in(file("modules/kernel"))
  .settings(defaultSettings)
  .dependsOn(higherKindCore)
  .settings(
    defaultSettings,
    name := "tofu-kernel",
    libraryDependencies += glassCore
  )

lazy val kernelCE2Interop = project
  .in(file("modules/kernelCE2Interop"))
  .dependsOn(kernel)
  .settings(
    defaultSettings,
    name := "tofu-kernel-ce2-interop",
    libraryDependencies += catsEffect2
  )

lazy val core = project
  .in(file("modules/core"))
  .dependsOn(kernel, kernelCE2Interop)
  .settings(
    defaultSettings,
    name := "tofu-core-ce2",
  )

lazy val kernelCE3Interop = project
  .in(file("modules/kernelCE3Interop"))
  .dependsOn(kernel)
  .settings(
    defaultSettings,
    name := "tofu-kernel-ce3-interop",
    libraryDependencies += catsEffect3
  )

lazy val core3 = project
  .in(file("modules/core3"))
  .dependsOn(kernel, kernelCE3Interop)
  .settings(
    defaultSettings,
    name := "tofu-core-ce3",
  )

lazy val kernelCatsMtlInterop = project
  .in(file("modules/kernel/interop/cats-mtl"))
  .settings(
    defaultSettings,
    name := "tofu-kernel-cats-mtl",
    libraryDependencies += catsMtl
  )
  .dependsOn(kernel)

lazy val memo = project
  .in(file("modules/memo"))
  .dependsOn(core, concurrent)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, catsEffect2),
    name := "tofu-memo"
  )

lazy val loggingStr = project
  .in(file("modules/logging/structured"))
  .settings(
    name := "tofu-logging-structured",
    defaultSettings,
    libraryDependencies ++= Seq(
      catsCore,
      circeCore,
      tethys,
      tethysJackson,
      slf4j,
      alleycats,
      scalatest,
      derevo,
      catsTagless
    ),
  )
  .dependsOn(kernel)

lazy val loggingDer = project
  .in(file("modules/logging/derivation"))
  .dependsOn(loggingStr)
  .dependsOn(derivation % "compile->test")
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(derevo, magnolia, slf4j, glassMacro % Test),
    name := "tofu-logging-derivation"
  )

lazy val loggingLayout = project
  .in(file("modules/logging/layout"))
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, logback, slf4j),
    name := "tofu-logging-layout"
  )
  .dependsOn(loggingStr)

lazy val loggingShapeless = project
  .in(file("modules/logging/interop/shapeless"))
  .settings(
    defaultSettings,
    name := "tofu-logging-shapeless",
    libraryDependencies += shapeless
  )
  .dependsOn(loggingStr)

lazy val loggingRefined = project
  .in(file("modules/logging/interop/refined"))
  .settings(
    defaultSettings,
    name := "tofu-logging-refined",
    libraryDependencies += refined
  )
  .dependsOn(loggingStr)

lazy val loggingLog4Cats = project
  .in(file("modules/logging/interop/log4cats"))
  .settings(
    defaultSettings,
    name := "tofu-logging-log4cats",
    libraryDependencies += log4Cats
  )
  .dependsOn(loggingStr)

lazy val loggingLogstashLogback = project
  .in(file("modules/logging/interop/logstash-logback"))
  .settings(
    defaultSettings,
    name := "tofu-logging-logstash-logback",
    libraryDependencies ++= Seq(logback, logstashLogback)
  )
  .dependsOn(loggingStr)
  .dependsOn(loggingDer % Test)

lazy val logging = project
  .in(file("modules/logging"))
  .dependsOn(loggingStr, loggingDer, loggingLayout, loggingShapeless, loggingRefined, loggingLog4Cats)
  .aggregate(
    loggingStr,
    loggingDer,
    loggingLayout,
    loggingShapeless,
    loggingRefined,
    loggingLog4Cats,
    loggingLogstashLogback
  )
  .settings(
    defaultSettings,
    name := "tofu-logging"
  )

lazy val env = project
  .in(file("modules/env"))
  .dependsOn(core, memo)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, catsEffect2, monix),
    name := "tofu-env"
  )

lazy val observable = project
  .in(file("modules/observable"))
  .settings(
    defaultSettings,
    libraryDependencies ++= Vector(monix, catsEffect2),
    libraryDependencies += scalatest,
    name := "tofu-observable",
  )

lazy val concurrent =
  project
    .in(file("modules/concurrent"))
    .dependsOn(core, derivation % "compile->test")
    .settings(
      defaultSettings,
      libraryDependencies ++= Seq(catsEffect2, catsTagless),
      libraryDependencies ++= Seq(simulacrum, derevoTagless, glassMacro).map(_ % Test),
      name := "tofu-concurrent",
    )

lazy val config = project
  .in(file("modules/config"))
  .dependsOn(core, concurrent)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(typesafeConfig, magnolia, derevo, glassCore),
    name := "tofu-config",
  )

lazy val enums = project
  .in(file("modules/enums"))
  .dependsOn(loggingStr)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(enumeratum),
    name := "tofu-enums",
  )

lazy val derivation =
  project
    .in(file("modules/derivation"))
    .settings(
      defaultSettings,
      libraryDependencies ++= Seq(magnolia, derevo, catsTagless),
      name := "tofu-derivation",
    )
    .dependsOn(kernel)

lazy val zioCore =
  project
    .in(file("modules/zio/core"))
    .settings(defaultSettings, libraryDependencies ++= List(zio, zioCats), name := "tofu-zio-core")
    .dependsOn(core, concurrent)

lazy val zioLogging =
  project
    .in(file("modules/zio/logging"))
    .settings(
      defaultSettings,
      libraryDependencies ++= List(zio, zioCats, slf4j, logback % Test),
      name := "tofu-zio-logging"
    )
    .dependsOn(loggingStr, loggingDer % "test", zioCore % Test)

lazy val zio2Logging =
  project
    .in(file("modules/zio2/logging"))
    .settings(
      defaultSettings,
      name := "tofu-zio2-logging",
      libraryDependencies ++= List(zio2, slf4j, logback % Test, zio2Test, zio2TestSbt)
    )
    .dependsOn(loggingStr, loggingDer % "test")

lazy val examplesZIO2 =
  project
    .in(file("examples-zio2"))
    .settings(
      defaultSettings,
      name := "tofu-examples-zio2",
      exampleSettings
    )
    .dependsOn(zio2Logging, loggingDer, loggingLayout)

lazy val zioInterop = project
  .in(file("modules/zio"))
  .settings(
    name := "tofu-zio-interop",
    defaultSettings
  )
  .dependsOn(zioCore, zioLogging)
  .aggregate(zioCore, zioLogging)

lazy val fs2Interop = project
  .in(file("modules/fs2"))
  .settings(
    name                             := "tofu-fs2-interop",
    libraryDependencies += fs2,
    libraryDependencies += glassMacro % Test,
    defaultSettings
  )
  .dependsOn(concurrent, streams)

lazy val fs2CE3Interop = project
  .in(file("modules/fs2-ce3"))
  .settings(
    name                             := "tofu-fs2-ce3-interop",
    libraryDependencies += fs2CE3,
    libraryDependencies += glassMacro % Test,
    defaultSettings
  )
  .dependsOn(core3, streams, derivation % "compile->test")

lazy val doobie = project
  .in(file("modules/doobie/core"))
  .settings(
    libraryDependencies ++= List(doobieCore, derevo, monix % Test),
    defaultSettings,
    name := "tofu-doobie",
  )
  .dependsOn(core, derivation, env % Test, zioInterop % Test)

lazy val doobieLogging = project
  .in(file("modules/doobie/logging"))
  .settings(
    defaultSettings,
    name := "tofu-doobie-logging",
  )
  .dependsOn(doobie, loggingStr)

lazy val doobieCE3 = project
  .in(file("modules/doobie/core-ce3"))
  .settings(
    libraryDependencies ++= List(doobieCoreCE3, derevo),
    defaultSettings,
    name := "tofu-doobie-ce3",
  )
  .dependsOn(core3, derivation)

lazy val doobieLoggingCE3 = project
  .in(file("modules/doobie/logging-ce3"))
  .settings(
    defaultSettings,
    name := "tofu-doobie-logging-ce3",
  )
  .dependsOn(doobieCE3, loggingStr)

lazy val examples = project
  .in(file("examples"))
  .settings(
    libraryDependencies ++= List(doobieCore, doobieH2, derevo, monix, groovy, derevoCirce),
    libraryDependencies ++= http4s,
    defaultSettings,
    name := "tofu-examples",
    exampleSettings,
  )
  .dependsOn(mainModuleDeps: _*)

lazy val examplesCE3 = project
  .in(file("examples-ce3"))
  .settings(
    libraryDependencies ++= List(doobieCoreCE3, doobieH2CE3, derevo, groovy),
    defaultSettings,
    name := "tofu-examples-ce3",
    exampleSettings,
  )
  .dependsOn(ce3MainModuleDeps: _*)

lazy val streams = project
  .in(file("modules/streams"))
  .settings(
    libraryDependencies ++= List(fs2 % Test),
    defaultSettings,
    name := "tofu-streams",
  )
  .dependsOn(kernel)

lazy val coreModules =
  Vector(
    higherKindCore,
    kernel,
    kernelCE2Interop,
    core,
    memo,
    derivation,
    env,
    concurrent,
    streams,
    kernelCatsMtlInterop
  )

lazy val ce3CoreModules = Vector(
  kernelCE3Interop,
  core3,
)

lazy val commonModules =
  Vector(observable, logging, enums, config, zioInterop, zio2Logging, fs2Interop, doobie, doobieLogging)

lazy val ce3CommonModules =
  Vector(loggingStr, loggingDer, loggingLayout, doobieCE3, doobieLoggingCE3, fs2CE3Interop)

lazy val allModuleRefs  = (coreModules ++ commonModules).map(x => x: ProjectReference)
lazy val mainModuleDeps = (coreModules ++ commonModules).map(x => x: ClasspathDep[ProjectReference])

lazy val ce3AllModuleRefs  = (ce3CoreModules ++ ce3CommonModules).map(x => x: ProjectReference)
lazy val ce3MainModuleDeps = (ce3CoreModules ++ ce3CommonModules).map(x => x: ClasspathDep[ProjectReference])

lazy val docs = project // new documentation project
  .in(file("tofu-docs"))
  .settings(
    noPublishSettings,
    addCompilerPlugin(simulacrum),
    macros,
    // ScalaUnidoc / doc / scalacOptions += "-Ymacro-expand:none",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(allModuleRefs: _*),
    ScalaUnidoc / unidoc / target              := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite                       := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages                   := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(mainModuleDeps: _*)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val tofu = project
  .in(file("."))
  .settings(
    defaultSettings,
    name := "tofu"
  )
  .aggregate(
    (coreModules ++ commonModules ++ ce3CoreModules ++ ce3CommonModules :+ docs :+ examples :+ examplesCE3 :+ examplesZIO2)
      .map(x => x: ProjectReference): _*
  )
  .dependsOn(coreModules.map(x => x: ClasspathDep[ProjectReference]): _*)

lazy val defaultScalacOptions = scalacOptions := {
  val tpolecatOptions = scalacOptions.value

  val dropLints = Set(
    "-Ywarn-dead-code",
    "-Wdead-code" // ignore dead code paths where `Nothing` is involved
  )

  val opts = tpolecatOptions.filterNot(dropLints)

  // drop `-Xfatal-warnings` on dev and 2.12 CI
  if (!sys.env.get("CI").contains("true") || (minorVersion.value == 12))
    opts.filterNot(Set("-Xfatal-warnings"))
  else
    opts
}

lazy val scalacWarningConfig = scalacOptions += {
  // // ignore unused imports that cannot be removed due to cross-compilation
  // val suppressUnusedImports = Seq(
  //   "scala/tofu/config/typesafe.scala"
  // ).map { src =>
  //   s"src=${scala.util.matching.Regex.quote(src)}&cat=unused-imports:s"
  // }.mkString(",")

  // print warning category for fine-grained suppressing, e.g. @nowarn("cat=unused-params")
  val contextDeprecationInfo = "cat=deprecation&msg=^(.*((Has)|(With)|(Logging)).*)$:silent"
  val verboseWarnings        = "any:wv"

  s"-Wconf:$contextDeprecationInfo,$verboseWarnings"
}

ThisBuild / libraryDependencySchemes += "io.circe" %% "circe-core" % "early-semver"

lazy val macros = Seq(
  scalacOptions ++= { if (minorVersion.value == 13) Seq("-Ymacro-annotations") else Seq() },
  libraryDependencies ++= { if (minorVersion.value == 12) Seq(compilerPlugin(macroParadise)) else Seq() }
)

lazy val noPublishSettings =
  defaultSettings ++ Seq(publish := {}, publishArtifact := false, publishTo := None, publish / skip := true)

lazy val exampleSettings = 
  noPublishSettings ++ Set(evictionErrorLevel := Level.Info)

addCommandAlias("fmt", "all tofu/scalafmtSbt tofu/scalafmtAll")
addCommandAlias("checkfmt", "all tofu/scalafmtSbtCheck tofu/scalafmtCheckAll")

addCommandAlias("preparePR", "scalafmtAll ;scalafmtSbt ;reload; githubWorkflowGenerate; clean; Test / compile")
