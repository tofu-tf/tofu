import Publish._, Dependencies._
import sbt.ModuleID
import org.typelevel.scalacoptions.{ScalacOption, ScalaVersion, ScalacOptions}
import scala.Ordering.Implicits._
import sbt.internal._

lazy val scala2Versions = List(Version.scala212, Version.scala213)
lazy val scala3Versions = List(Version.scala212, Version.scala213, Version.scala3)

lazy val defaultSettings = Seq(
  scalaVersion := Version.scala213,
  scalacWarningConfig,
  Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement,
  Compile / doc / tpolecatExcludeOptions ++= fatalWarningsOptions,
  // crossScalaVersions := scala2Versions,
  libraryDependencies ++= {
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) =>
        Seq(
          compilerPlugin(betterMonadicFor),
          compilerPlugin(kindProjector),
          scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
        )
      case _            => Seq()
    }) ++ Seq(scalatest, collectionCompat)
  }
) ++ macros ++ defaultScalacOptions

val modules = file("modules")

lazy val higherKindCore = projectMatrix
  .in(modules / "kernel" / "higherKind")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-core-higher-kind",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) =>
          Seq(catsCore, catsFree, catsTaglessMacros)
        case _            =>
          Seq(catsCore, catsFree, catsTaglessCore)
      }
    },
  )
  .jvmPlatform(scala3Versions)

lazy val kernel = projectMatrix
  .in(modules / "kernel")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-kernel",
    libraryDependencies += glassCore
  )
  .jvmPlatform(scala3Versions)
  .dependsOn(higherKindCore)

lazy val coreCE2 = projectMatrix
  .in(modules / "core" / "ce2")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-core-ce2",
    libraryDependencies += catsEffect2
  )
  .jvmPlatform(scala3Versions)
  .dependsOn(kernel)

lazy val concurrentCE2 =
  projectMatrix
    .in(modules / "core" / "concurrent-ce2")
    .settings(
      defaultSettings,
      libraryDependencies ++= Seq(catsEffect2, catsTaglessMacros),
      libraryDependencies ++= Seq(simulacrum, derevoTagless, glassMacro).map(_ % Test),
      name := "tofu-concurrent-ce2",
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(coreCE2, derivation % "compile->test")

lazy val coreCE3 = projectMatrix
  .in(modules / "core" / "ce3")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-core-ce3",
    libraryDependencies += catsEffect3
  )
  .jvmPlatform(scala3Versions)
  .dependsOn(kernel)

lazy val kernelCatsMtlInterop = projectMatrix
  .in(modules / "kernel" / "interop" / "cats-mtl")
  .settings(
    defaultSettings,
    name := "tofu-kernel-cats-mtl",
    libraryDependencies += catsMtl
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(kernel)

lazy val loggingStr = projectMatrix
  .in(modules / "logging" / "structured")
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
      catsTaglessMacros
    ),
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(kernel)

lazy val loggingDer = projectMatrix
  .in(modules / "logging" / "derivation")
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(derevo, magnolia, slf4j, glassMacro % Test),
    name := "tofu-logging-derivation"
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr, derivation % "compile->test")

lazy val loggingLayout = projectMatrix
  .in(modules / "logging" / "layout")
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, logback, slf4j),
    name := "tofu-logging-layout"
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr)

val loggingInterop = modules / "logging" / "interop"

lazy val loggingShapeless = projectMatrix
  .in(loggingInterop / "shapeless")
  .settings(
    defaultSettings,
    name := "tofu-logging-shapeless",
    libraryDependencies += shapeless
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr)

lazy val loggingRefined = projectMatrix
  .in(loggingInterop / "refined")
  .settings(
    defaultSettings,
    name := "tofu-logging-refined",
    libraryDependencies += refined
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr)

lazy val loggingLog4CatsLegacy = projectMatrix
  .in(loggingInterop / "log4cats-legacy")
  .settings(
    defaultSettings,
    name := "tofu-logging-log4cats-legacy",
    libraryDependencies += log4CatsLegacy
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr)

lazy val loggingLog4Cats = projectMatrix
  .in(loggingInterop / "log4cats")
  .settings(
    defaultSettings,
    name := "tofu-logging-log4cats",
    libraryDependencies += log4Cats
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr)

lazy val loggingLogstashLogback = projectMatrix
  .in(loggingInterop / "logstash-logback")
  .settings(
    defaultSettings,
    name := "tofu-logging-logstash-logback",
    libraryDependencies ++= Seq(logback, logstashLogback)
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr, loggingDer % Test)

lazy val loggingEnumeratum = projectMatrix
  .in(loggingInterop / "enums")
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(enumeratum),
    name := "tofu-logging-enumeratum",
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr)

// lazy val allLoggingModuleRefs =
//   loggingStr.projectRefs ++
//     loggingDer.projectRefs ++
//     loggingLayout.projectRefs ++
//     loggingShapeless.projectRefs ++
//     loggingRefined.projectRefs ++
//     loggingLog4Cats.projectRefs ++
//     loggingLog4CatsLegacy.projectRefs ++
//     loggingLogstashLogback.projectRefs

lazy val logging = projectMatrix
  .in(modules / "logging")
  .aggregate(
    loggingStr,
    loggingDer,
    loggingLayout,
    loggingShapeless,
    loggingRefined,
    loggingLog4Cats,
    loggingLog4CatsLegacy,
    loggingLogstashLogback
  )
  .settings(
    // defaultSettings,
    name := "tofu-logging"
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr, loggingDer, loggingLayout, loggingShapeless, loggingRefined, loggingLog4Cats)

val util = modules / "util"

lazy val env = projectMatrix
  .in(util / "env")
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, catsEffect2, monix),
    name := "tofu-env"
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(coreCE2, memo)

lazy val observable = projectMatrix
  .in(util / "observable")
  .settings(
    defaultSettings,
    libraryDependencies ++= Vector(monix, catsEffect2),
    libraryDependencies += scalatest,
    name := "tofu-observable",
  )
  .jvmPlatform(scala2Versions)

lazy val config = projectMatrix
  .in(util / "config")
  .dependsOn(coreCE2, concurrentCE2)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(typesafeConfig, magnolia, derevo, glassCore),
    name := "tofu-config",
  )
  .jvmPlatform(scala2Versions)

lazy val memo = projectMatrix
  .in(util / "memo")
  .dependsOn(coreCE2, concurrentCE2)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, catsEffect2),
    name := "tofu-memo"
  )
  .jvmPlatform(scala2Versions)

lazy val derivation =
  projectMatrix
    .in(modules / "derivation")
    .settings(
      defaultSettings,
      libraryDependencies ++= Seq(magnolia, derevo, catsTaglessMacros),
      name := "tofu-derivation",
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(kernel)

val zioInterop = modules / "interop" / "zio1"

lazy val zio1Core =
  projectMatrix
    .in(zioInterop / "core")
    .settings(defaultSettings, libraryDependencies ++= List(zio, zioCats), name := "tofu-zio-core")
    .jvmPlatform(scala2Versions)
    .dependsOn(coreCE2, concurrentCE2)

lazy val zio2Core =
  projectMatrix
    .in(modules / "interop" / "zio2" / "core")
    .settings(
      defaultSettings,
      scala3MigratedModuleOptions,
      libraryDependencies ++= List(zio2, zio2Cats),
      name := "tofu-zio2-core"
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(coreCE3)

lazy val zio1Logging =
  projectMatrix
    .in(zioInterop / "logging")
    .settings(
      defaultSettings,
      libraryDependencies ++= List(zio, zioCats, slf4j, logback % Test),
      name := "tofu-zio-logging"
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(loggingStr, loggingDer % "test", zio1Core % Test)

lazy val zio2Logging =
  projectMatrix
    .in(modules / "interop" / "zio2" / "logging")
    .settings(
      defaultSettings,
      name := "tofu-zio2-logging",
      libraryDependencies ++= List(zio2, slf4j, logback % Test, zio2Test, zio2TestSbt),
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(loggingStr, loggingDer % "test")

val interop = modules / "interop"

lazy val zio1Interop = projectMatrix
  .in(file("modules/zio"))
  .settings(
    name := "tofu-zio-interop",
    defaultSettings
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(zio1Core, zio1Logging)
  .aggregate(zio1Core, zio1Logging)

lazy val fs2CE2Interop = projectMatrix
  .in(interop / "fs2" / "ce2")
  .settings(
    name                             := "tofu-fs2-interop",
    libraryDependencies += fs2,
    libraryDependencies += glassMacro % Test,
    defaultSettings
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(concurrentCE2, streams)

lazy val fs2CE3Interop = projectMatrix
  .in(interop / "fs2" / "ce3")
  .settings(
    name                             := "tofu-fs2-ce3-interop",
    libraryDependencies += fs2CE3,
    libraryDependencies += glassMacro % Test,
    defaultSettings
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(coreCE3, streams, derivation % "compile->test")

lazy val doobie = projectMatrix
  .in(modules / "doobie" / "core-ce2")
  .settings(
    libraryDependencies ++= List(doobieCore, derevo, monix % Test),
    defaultSettings,
    name := "tofu-doobie",
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(coreCE2, derivation, env % Test, zio1Interop % Test)

lazy val doobieLogging = projectMatrix
  .in(modules / "doobie" / "logging-ce2")
  .settings(
    defaultSettings,
    name := "tofu-doobie-logging",
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(doobie, loggingStr)

lazy val doobieCE3 = projectMatrix
  .in(modules / "doobie" / "core-ce3")
  .settings(
    libraryDependencies ++= List(doobieCoreCE3, derevo),
    defaultSettings,
    name := "tofu-doobie-ce3",
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(coreCE3, derivation)

lazy val doobieLoggingCE3 = projectMatrix
  .in(file("modules/doobie/logging-ce3"))
  .settings(
    defaultSettings,
    name := "tofu-doobie-logging-ce3",
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(doobieCE3, loggingStr)

lazy val streams = projectMatrix
  .in(file("modules/streams"))
  .settings(
    libraryDependencies ++= List(fs2 % Test),
    defaultSettings,
    name := "tofu-streams",
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(kernel)

val examples = file("examples")

lazy val examplesCE2 = projectMatrix
  .in(examples / "ce2")
  .settings(
    libraryDependencies ++= List(doobieCore, doobieH2, derevo, monix, groovy, derevoCirce),
    libraryDependencies ++= http4s,
    defaultSettings,
    name := "tofu-examples-ce2",
    exampleSettings,
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(mainModuleDeps: _*)

lazy val examplesCE3 = projectMatrix
  .in(examples / "ce3")
  .settings(
    libraryDependencies ++= List(doobieCoreCE3, doobieH2CE3, derevo, groovy),
    defaultSettings,
    name := "tofu-examples-ce3",
    exampleSettings,
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(ce3MainModuleDeps: _*)

lazy val examplesZIO2 =
  projectMatrix
    .in(examples / "zio2")
    .settings(
      defaultSettings,
      name := "tofu-examples-zio2",
      exampleSettings
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(zio2Logging, loggingDer, loggingLayout)

lazy val coreModules =
  Vector(
    higherKindCore,
    kernel,
    coreCE2,
    memo,
    derivation,
    env,
    concurrentCE2,
    streams,
    kernelCatsMtlInterop
  )

lazy val ce3CoreModules = Vector(coreCE3)

lazy val commonModules =
  Vector(observable, logging, loggingEnumeratum, config, zio1Interop, fs2CE2Interop, doobie, doobieLogging)

lazy val ce3CommonModules =
  Vector(loggingStr, loggingDer, loggingLayout, doobieCE3, doobieLoggingCE3, fs2CE3Interop)

lazy val allModuleRefs  = (coreModules ++ commonModules).flatMap(_.projectRefs)
lazy val mainModuleDeps = (coreModules ++ commonModules).map(x => x: MatrixClasspathDep[ProjectMatrixReference])

//lazy val ce3AllModuleRefs  = (ce3CoreModules ++ ce3CommonModules).map(x => x: ProjectReference)
lazy val ce3MainModuleDeps =
  (ce3CoreModules ++ ce3CommonModules).map(x => x: MatrixClasspathDep[ProjectMatrixReference])

lazy val zio2Modules = Vector(zio2Logging, zio2Core)

lazy val docs = projectMatrix // new documentation project
  .in(file("tofu-docs"))
  .settings(
    noPublishSettings,
    macros,
    tpolecatExcludeOptions ++= fatalWarningsOptions,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(allModuleRefs: _*),
    ScalaUnidoc / unidoc / target              := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite                       := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages                   := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(mainModuleDeps: _*)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val allProjectsRef =
  (coreModules ++ commonModules ++ ce3CoreModules ++ ce3CommonModules ++ zio2Modules :+ docs :+ examplesCE2 :+ examplesCE3 :+ examplesZIO2)
    .flatMap(_.projectRefs)

lazy val tofu = project
  .in(file("."))
  .settings(
    defaultSettings,
    name := "tofu"
  )
  .aggregate(allProjectsRef: _*)
//.dependsOn(coreModules.map(x => x: MatrixClasspathDep[ProjectMatrixReference]): _*)

lazy val defaultScalacOptions =
  Seq(
    tpolecatExcludeOptions ++= Set(ScalacOptions.warnDeadCode, ScalacOptions.privateWarnDeadCode),
    tpolecatExcludeOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12))                            => fatalWarningsOptions
        case _ if !sys.env.get("CI").contains("true") => fatalWarningsOptions
        case _                                        => Set.empty
      }
    }
  )

lazy val scala3MigratedModuleOptions =
  Seq(
    scalaVersion := Version.scala3,
    tpolecatScalacOptions ++= Set(
      ScalacOption("-Ykind-projector:underscores", _ >= ScalaVersion.V3_0_0),
      ScalacOption("-P:kind-projector:underscore-placeholders", _ < ScalaVersion.V3_0_0),
      ScalacOptions.source3,
      ScalacOption("-Xmigration", _ < ScalaVersion.V3_0_0)
    ),
    // crossScalaVersions := Vector(Version.scala212, Version.scala213, Version.scala3)
  )

lazy val scalacWarningConfig = tpolecatScalacOptions ++= {
  // // ignore unused imports that cannot be removed due to cross-compilation
  // val suppressUnusedImports = Seq(
  //   "scala/tofu/config/typesafe.scala"
  // ).map { src =>
  //   s"src=${scala.util.matching.Regex.quote(src)}&cat=unused-imports:s"
  // }.mkString(",")

  // print warning category for fine-grained suppressing, e.g. @nowarn("cat=unused-params")
  val contextDeprecationInfo    = "cat=deprecation&msg=^(.*((Has)|(With)|(Logging)|(Mut)).*)$:silent"
  val concurrentDeprecationInfo = "cat=deprecation&msg=^(.*((Mut)|(MVar)).*)$:silent"
  val deprecationInfo           = s"$contextDeprecationInfo,$concurrentDeprecationInfo"

  val verboseWarnings         = "any:wv"
  val scala3MigrationWarnings = "cat=scala3-migration:silent"

  Set(
    ScalacOption(s"-Wconf:$deprecationInfo", _ >= ScalaVersion.V3_0_0),
    ScalacOption(
      s"-Wconf:$deprecationInfo,$scala3MigrationWarnings,$verboseWarnings",
      _.isBetween(ScalaVersion.V2_13_0, ScalaVersion.V3_0_0)
    ),
    ScalacOption(s"-Wconf:$deprecationInfo,$verboseWarnings", _ < ScalaVersion.V2_13_0)
  )
}

lazy val fatalWarningsOptions =
  Set(ScalacOptions.fatalWarnings, ScalacOptions.warnError)

ThisBuild / libraryDependencySchemes += "io.circe" %% "circe-core" % "early-semver"

lazy val macros = Seq(
  tpolecatScalacOptions += ScalacOption(
    "-Ymacro-annotations",
    _.isBetween(ScalaVersion.V2_13_0, ScalaVersion.V3_0_0)
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq(compilerPlugin(macroParadise))
      case _             => Seq.empty
    }
  }
)

lazy val noPublishSettings =
  defaultSettings ++ Seq(publish := {}, publishArtifact := false, publishTo := None, publish / skip := true)

lazy val exampleSettings =
  noPublishSettings ++ Set(evictionErrorLevel := Level.Info)

addCommandAlias("fmt", "all tofu/scalafmtSbt tofu/scalafmtAll")
addCommandAlias("checkfmt", "all tofu/scalafmtSbtCheck tofu/scalafmtCheckAll")

addCommandAlias("preparePR", "scalafmtAll ;scalafmtSbt ;reload; clean; Test / compile")
