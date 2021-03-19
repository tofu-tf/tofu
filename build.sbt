import Publish._, Dependencies._
import com.typesafe.sbt.SbtGit.git

val libVersion = "0.7.1"

lazy val setMinorVersion = minorVersion := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) => v.toInt
    case _            => 0
  }
}

lazy val setModuleName = moduleName := { s"tofu-${(publishName or name).value}" }
lazy val experimental  = scalacOptions ++= { if (scalaVersion.value < "2.12") Seq("-Xexperimental") else Nil }

lazy val macros = Seq(
  scalacOptions ++= { if (minorVersion.value == 13) Seq("-Ymacro-annotations") else Seq() },
  libraryDependencies ++= { if (minorVersion.value == 12) Seq(compilerPlugin(macroParadise)) else Seq() }
)

lazy val defaultSettings = Seq(
  scalaVersion := "2.13.4",
  setMinorVersion,
  setModuleName,
  experimental,
  defaultScalacOptions,
  scalacWarningConfig,
  libraryDependencies ++= Seq(
    compilerPlugin(kindProjector),
    compilerPlugin(betterMonadicFor),
    scalatest
  )
) ++ macros ++ publishSettings ++ simulacrumOptions

moduleName := "tofu"

lazy val higherKindCore = project settings (
  defaultSettings,
  publishName := "core-higher-kind",
  libraryDependencies ++= Seq(catsCore, catsFree, catsTagless)
)

lazy val core = project dependsOn (opticsCore, higherKindCore) settings (
  defaultSettings,
  publishName := "core",
  libraryDependencies ++= Seq(catsCore, catsEffect, catsTagless)
)

lazy val memo = project
  .dependsOn(core, concurrent)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, catsEffect)
  )

lazy val loggingStr = project
  .in(file("logging/structured"))
  .settings(
    publishName := "logging-structured",
    defaultSettings,
    libraryDependencies ++= Seq(
      catsCore,
      catsEffect,
      circeCore,
      tethys,
      tethysJackson,
      slf4j,
      alleycats,
      scalatest,
      derevo,
      catsTagless
    )
  )
  .dependsOn(core, data)

lazy val loggingDer = project
  .in(file("logging/derivation"))
  .dependsOn(loggingStr)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(derevo, magnolia),
    publishName := "logging-derivation"
  )

lazy val loggingLayout = project
  .in(file("logging/layout"))
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(catsCore, catsEffect, logback, slf4j),
    publishName := "logging-layout"
  )
  .dependsOn(loggingStr)

lazy val loggingUtil = project
  .in(file("logging/util"))
  .settings(
    defaultSettings,
    publishName := "logging-util",
    libraryDependencies += slf4j,
  )
  .dependsOn(loggingStr, concurrent)

lazy val logging = project
  .dependsOn(loggingStr, loggingDer, loggingLayout, loggingUtil)
  .aggregate(loggingStr, loggingDer, loggingLayout, loggingUtil)
  .settings(defaultSettings)

lazy val env = project
  .dependsOn(core, memo)
  .settings(defaultSettings, libraryDependencies ++= Seq(catsCore, catsEffect, monix))

lazy val observable = project.settings(
  defaultSettings,
  libraryDependencies += monix,
  libraryDependencies += scalatest
)

lazy val concurrent =
  project dependsOn core settings (
    defaultSettings,
    libraryDependencies ++= Seq(catsEffect, catsTagless),
)

lazy val config = project dependsOn (core, data, opticsCore, concurrent) settings (
  defaultSettings,
  libraryDependencies ++= Seq(typesafeConfig, magnolia, derevo)
)

lazy val opticsCore = project
  .in(file("optics/core"))
  .settings(
    defaultSettings,
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
  .settings(
    defaultSettings,
    scalacOptions --= Seq(
      "-Ywarn-unused:params",
      "-Ywarn-unused:patvars"
    ),
    publishName := "optics-macro"
  )

lazy val enums = project
  .dependsOn(loggingStr)
  .settings(
    defaultSettings,
    libraryDependencies ++= Seq(enumeratum)
  )

lazy val data =
  project
    .settings(defaultSettings, libraryDependencies ++= Seq(catsFree))
    .dependsOn(core, opticsCore)

lazy val derivation =
  project
    .settings(
      defaultSettings,
      libraryDependencies ++= Seq(magnolia, derevo, catsTagless),
      publishName := "derivation"
    )
    .dependsOn(data)

lazy val zioCore =
  project
    .in(file("zio/core"))
    .settings(defaultSettings, libraryDependencies ++= List(zio, zioCats), publishName := "zio-core")
    .dependsOn(core, concurrent)

lazy val zioLogging =
  project
    .in(file("zio/logging"))
    .settings(defaultSettings, libraryDependencies ++= List(zio, slf4j), publishName := "zio-logging")
    .dependsOn(loggingStr)

lazy val zioInterop = project
  .in(file("zio"))
  .settings(
    publishName := "zio-interop",
    defaultSettings
  )
  .dependsOn(zioCore, zioLogging)
  .aggregate(zioCore, zioLogging)

lazy val fs2Interop = project
  .in(file("fs2"))
  .settings(
    publishName := "fs2-interop",
    libraryDependencies += fs2,
    defaultSettings
  )
  .dependsOn(concurrent)

lazy val coreModules = Seq(higherKindCore, core, memo, env, concurrent, opticsCore, data)

lazy val commonModules =
  Seq(observable, opticsInterop, opticsMacro, logging, enums, config, derivation, zioInterop, fs2Interop)

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
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value
  )
  .dependsOn(allModuleDeps: _*)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val tofu = project
  .in(file("."))
  .settings(defaultSettings)
  .aggregate((coreModules ++ commonModules).map(x => x: ProjectReference): _*)
  .dependsOn(coreModules.map(x => x: ClasspathDep[ProjectReference]): _*)

libraryDependencies += scalatest

lazy val simulacrumOptions = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
    simulacrum              % Provided
  ),
  pomPostProcess := { node =>
    import scala.xml.transform.{RewriteRule, RuleTransformer}

    new RuleTransformer(new RewriteRule {
      override def transform(node: xml.Node): Seq[xml.Node] = node match {
        case e: xml.Elem
            if e.label == "dependency" &&
              e.child.exists(child => child.label == "groupId" && child.text == simulacrum.organization) &&
              e.child.exists(child => child.label == "artifactId" && child.text.startsWith(s"${simulacrum.name}_")) =>
          Nil
        case _ => Seq(node)
      }
    }).transform(node).head
  }
)

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

lazy val publishSettings = Seq(
  organization := "ru.tinkoff",
  publishVersion := libVersion,
  publishMavenStyle := true,
  description := "Opinionated set of tools for functional programming in Scala",
  crossScalaVersions := Seq("2.12.10", "2.13.4"),
  publishTo := {
    if (isSnapshot.value) {
      Some(Opts.resolver.sonatypeSnapshots)
    } else sonatypePublishToBundle.value
  },
  credentials ++= ((Path.userHome / ".sbt" / ".ossrh-credentials") :: Nil)
    .filter(_.exists())
    .map(Credentials.apply),
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
inThisBuild(
  List(
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

lazy val scalacWarningConfig = scalacOptions += {
  // ignore unused imports that cannot be removed due to cross-compilation
  val suppressUnusedImports = Seq(
    "scala/tofu/config/typesafe.scala"
  ).map { src => s"src=${scala.util.matching.Regex.quote(src)}&cat=unused-imports:s" }.mkString(",")

  // print warning category for @nowarn("cat=...")
  val verboseWarnings    = "any:wv"
  val verboseInfoImports = "cat=unused-imports:info-verbose"
  s"-Wconf:$suppressUnusedImports,$verboseInfoImports,$verboseWarnings"
}

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("checkfmt", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
