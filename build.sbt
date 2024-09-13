import Publish._, Dependencies._
import sbt.ModuleID
import org.typelevel.scalacoptions.{ScalacOption, ScalaVersion, ScalacOptions}
import scala.Ordering.Implicits._
import sbt.internal._
import sbt.Reference.display
import complete.DefaultParsers._
import sbt.librarymanagement.CrossVersion.{binaryScalaVersion, partialVersion}

lazy val scala2Versions     = List(Version.scala212, Version.scala213)
lazy val scala2And3Versions = scala2Versions ++ List(Version.scala3)

val scopesDescription = s"Scala version can be: ${scala2And3Versions.mkString}"
val testScoped        = inputKey[Unit](s"Run tests in the given scope. Usage: testScoped [scala version]. $scopesDescription")

def filterByScalaVersion(scalaVersionFilter: String) = {
  val scalaVersion =
    scala2And3Versions
      .filter(_.startsWith(scalaVersionFilter))
      .headOption
      .getOrElse(
        sys.error(
          s"invalid scala version value: $scalaVersionFilter, please use one of: ${scala2And3Versions.mkString}"
        )
      )

  ScopeFilter(
    inProjects(
      allModules
        .flatMap(_.filterProjects(Seq(VirtualAxis.scalaABIVersion(scalaVersion), VirtualAxis.jvm)))
        .map(x => x: ProjectReference): _*
    )
  )
}

lazy val defaultSettings = Seq(
  scalacWarningConfig,
  Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement,
  Compile / doc / tpolecatExcludeOptions ++= fatalWarningsOptions,
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
    libraryDependencies ++= Seq(catsCore, catsFree, catsTaglessCore),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) =>
          Seq(catsTaglessMacros)
        case _            =>
          Seq.empty
      }
    },
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)

lazy val kernel = projectMatrix
  .in(modules / "kernel")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-kernel",
    libraryDependencies += glassCore
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)
  .dependsOn(higherKindCore)

lazy val coreCE2 = projectMatrix
  .in(modules / "core" / "ce2")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-core-ce2",
    libraryDependencies += catsEffect2
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)
  .dependsOn(kernel)

lazy val concurrentCE2 = projectMatrix
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
  .jvmPlatform(scalaVersions = scala2And3Versions)
  .dependsOn(kernel)

lazy val kernelCatsMtlInterop = projectMatrix
  .in(modules / "kernel" / "interop" / "cats-mtl")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-kernel-cats-mtl",
    libraryDependencies += catsMtl
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)
  .dependsOn(kernel)

lazy val loggingStr = projectMatrix
  .in(modules / "logging" / "structured")
  .settings(
    name := "tofu-logging-structured",
    defaultSettings,
    scala3MigratedModuleOptions,
    libraryDependencies ++= Seq(
      catsCore,
      circeCore,
      tethys,
      tethysJackson,
      slf4j,
      alleycats,
      catsTaglessCore,
      scalatest
    )
  )
  .jvmPlatform(scala2And3Versions)
  .dependsOn(kernel)

lazy val loggingDer = projectMatrix
  .in(modules / "logging" / "derivation")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(derevo, magnolia2, slf4j, glassMacro % Test)
        case Some((3, _)) => Seq(magnolia3, slf4j)
        case _            => Seq.empty
      }
    },
    name := "tofu-logging-derivation"
  )
  .jvmPlatform(scala2And3Versions)
  .dependsOn(loggingStr, loggingDerivationAnnotations)

lazy val loggingDerivationAnnotations = projectMatrix
  .in(modules / "logging" / "derivation-annotations")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-logging-derivation-annotations"
  )
  .jvmPlatform(scala2And3Versions)

lazy val loggingLayout = projectMatrix
  .in(modules / "logging" / "layout")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    libraryDependencies ++= Seq(catsCore, logback, slf4j),
    name := "tofu-logging-layout"
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)
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
    scala3MigratedModuleOptions,
    name := "tofu-logging-refined",
    libraryDependencies += refined
  )
  .jvmPlatform(scala2And3Versions)
  .dependsOn(loggingStr)

lazy val loggingLog4CatsLegacy = projectMatrix
  .in(loggingInterop / "log4cats-legacy")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-logging-log4cats-legacy",
    libraryDependencies += log4CatsLegacy
  )
  .jvmPlatform(scala2And3Versions)
  .dependsOn(loggingStr)

lazy val loggingLog4Cats = projectMatrix
  .in(loggingInterop / "log4cats")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-logging-log4cats",
    libraryDependencies += log4Cats,
    libraryDependencies += slf4j // added to fix compiler crash - `cannot resolve reference to type org.slf4j.type.Marker`
  )
  .jvmPlatform(scala2And3Versions)
  .dependsOn(loggingStr)

lazy val loggingLogstashLogback = projectMatrix
  .in(loggingInterop / "logstash-logback")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-logging-logstash-logback",
    libraryDependencies ++= Seq(logback, logstashLogback)
  )
  .jvmPlatform(scala2And3Versions)
  .dependsOn(loggingStr, loggingDer % Test)

lazy val loggingEnumeratum = projectMatrix
  .in(loggingInterop / "enumeratum")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    libraryDependencies ++= Seq(enumeratum),
    name := "tofu-logging-enumeratum",
  )
  .jvmPlatform(scala2And3Versions)
  .dependsOn(loggingStr)

lazy val logging = {
  def projectRefFromMatrix(pm: ProjectMatrix, scalaVersion: String): ProjectReference = {
    val projects = pm.filterProjects(Seq(VirtualAxis.jvm, VirtualAxis.scalaABIVersion(scalaVersion)))
    projects.headOption match {
      case Some(p) => p
      case None    => throw new Exception(s"Can't found $scalaVersion axis for ${pm}")
    }
  }

  val modulesSettings =
    List(
      // ($project, $haScala3, $dependsOn)
      (loggingStr, true, true),
      (loggingDer, true, true),
      (loggingDerivationAnnotations, true, true),
      (loggingLayout, true, true),
      (loggingShapeless, false, true),
      (loggingRefined, true, true),
      (loggingLog4Cats, true, true),
      (loggingLogstashLogback, true, true),
    )

  val initial = ProjectMatrix("logging", modules / "logging")
  scala2And3Versions.foldLeft(initial) { case (prMatrix, scalaVersionValue) =>
    val scalaAxis = VirtualAxis.scalaABIVersion(scalaVersionValue)
    prMatrix.customRow(
      autoScalaLibrary = true,
      axisValues = Seq(VirtualAxis.jvm, scalaAxis),
      process = (pr: Project) => {
        val filterScala                    =
          if (scalaVersionValue.startsWith("3")) identity[Boolean](_)
          else (_: Boolean) => true
        val (aggModules, dependsOnModules) =
          modulesSettings.foldLeft((List.empty[ProjectReference], List.empty[ClasspathDependency])) {
            case ((agg, dependsOn), (prM, hasScala3, isInDependsOn)) if filterScala(hasScala3) =>
              val ref           = projectRefFromMatrix(prM, scalaVersionValue)
              val nextAgg       = ref :: agg
              val nextDependsOn = if (isInDependsOn) ClasspathDependency(ref, None) :: dependsOn else dependsOn
              (nextAgg, nextDependsOn)
            case (acc, _)                                                                      => acc
          }

        pr.aggregate(aggModules: _*)
          .settings(
            defaultSettings,
            scalaVersion := scalaVersionValue,
            name         := "tofu-logging"
          )
          .dependsOn(dependsOnModules: _*)
      }
    )
  }
}

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
    scala3MigratedModuleOptions,
    libraryDependencies ++= Vector(monix, catsEffect2),
    libraryDependencies += scalatest,
    name := "tofu-observable",
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)

lazy val config = projectMatrix
  .in(util / "config")
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(typesafeConfig, magnolia2, derevo, glassCore),
    name := "tofu-config",
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(coreCE2, concurrentCE2)

lazy val memo = projectMatrix
  .in(util / "memo")
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, catsEffect2),
    name := "tofu-memo"
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(coreCE2, concurrentCE2)

lazy val derivation = projectMatrix
  .in(modules / "derivation")
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(magnolia2, derevo, catsTaglessMacros),
    name := "tofu-derivation",
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(kernel)

val zioInterop = modules / "interop" / "zio1"

lazy val zio1Core = projectMatrix
  .in(zioInterop / "core")
  .settings(defaultSettings, libraryDependencies ++= List(zio, zioCats), name := "tofu-zio-core")
  .jvmPlatform(scala2Versions)
  .dependsOn(coreCE2, concurrentCE2)

lazy val zio2Core = projectMatrix
  .in(modules / "interop" / "zio2" / "core")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    libraryDependencies ++= List(zio2, zio2Cats, zio2Test, zio2TestSbt),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    name := "tofu-zio2-core"
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)
  .dependsOn(coreCE3)

lazy val zio1Logging = projectMatrix
  .in(zioInterop / "logging")
  .settings(
    defaultSettings,
    libraryDependencies ++= List(zio, zioCats, slf4j, logback % Test),
    name := "tofu-zio-logging"
  )
  .jvmPlatform(scala2Versions)
  .dependsOn(loggingStr, loggingDer % Test, zio1Core % Test)

lazy val zio2Logging = projectMatrix
  .in(modules / "interop" / "zio2" / "logging")
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    name := "tofu-zio2-logging",
    libraryDependencies ++= List(zio2, slf4j, logback % Test, zio2Test, zio2TestSbt),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)
  .dependsOn(loggingStr, loggingDer % Test, zio2Core % Test)

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
    name := "tofu-fs2-ce3-interop",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(fs2CE3, glassMacro % Test)
        case Some((3, _)) => Seq(fs2CE3)
        case _            => Seq.empty
      }
    },
    defaultSettings,
    scala3MigratedModuleOptions
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)
  .dependsOn(coreCE3, streams)

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
    scala3MigratedModuleOptions,
    name := "tofu-streams",
  )
  .jvmPlatform(scalaVersions = scala2And3Versions)
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

lazy val examplesZIO2 = projectMatrix
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

lazy val mainModules = (coreModules ++ commonModules)

lazy val mainModuleScala213Refs =
  mainModules
    .flatMap(_.filterProjects(Seq(VirtualAxis.scalaABIVersion(Version.scala213), VirtualAxis.jvm)))
    .map(x => x: ProjectReference)

lazy val mainModuleDeps = mainModules.map(x => x: MatrixClasspathDep[ProjectMatrixReference])

lazy val ce3MainModuleDeps =
  (ce3CoreModules ++ ce3CommonModules).map(x => x: MatrixClasspathDep[ProjectMatrixReference])

lazy val zio2Modules = Vector(zio2Logging, zio2Core)

lazy val allModules =
  coreModules ++ commonModules ++ ce3CoreModules ++ ce3CommonModules ++ zio2Modules :+ docs :+ examplesCE2 :+ examplesCE3 :+ examplesZIO2

lazy val docs = projectMatrix // new documentation project
  .in(file("tofu-docs"))
  .settings(
    defaultSettings,
    scala3MigratedModuleOptions,
    noPublishSettings,
    macros,
    tpolecatScalacOptions += ScalacOption(s"-Wconf:cat=other-pure-statement:silent", _ >= ScalaVersion.V2_13_0),
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(mainModuleScala213Refs: _*),
    ScalaUnidoc / unidoc / target              := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite                       := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages                   := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .jvmPlatform(Seq(Version.scala213))
  .dependsOn(mainModuleDeps: _*)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val tofu = project
  .in(file("."))
  .settings(
    defaultSettings,
    name       := "tofu",
    testScoped := Def.inputTaskDyn {
      val args = spaceDelimited("<arg>").parsed
      Def.taskDyn((Test / test).all(filterByScalaVersion(args.head)))
    }.evaluated
  )
  .aggregate(allModules.flatMap(_.projectRefs): _*)

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
    tpolecatExcludeOptions ++= Set(ScalacOptions.privateKindProjector),
    tpolecatScalacOptions ++= Set(
      ScalacOption("-Ykind-projector:underscores", _ >= ScalaVersion.V3_0_0),
      ScalacOption("-P:kind-projector:underscore-placeholders", _ < ScalaVersion.V3_0_0),
      ScalacOptions.source3
    )
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
