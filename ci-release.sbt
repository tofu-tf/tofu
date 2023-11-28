ThisBuild / scalaVersion := Dependencies.Version.scala213

ThisBuild / crossScalaVersions := Vector(
  Dependencies.Version.scala213,
  Dependencies.Version.scala212,
  Dependencies.Version.scala3
)

ThisBuild / versionScheme := Some("semver-spec")

ThisBuild / licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

ThisBuild / developers := List(
  Developer("catostrophe", "λoλcat", "catostrophe@pm.me", url("https://github.com/catostrophe")),
  Developer("danslapman", "Daniil Smirnov", "danslapman@gmail.com", url("https://github.com/danslapman")),
  Developer("odomontois", "Oleg Nizhnik", "odomontois@gmail.com", url("https://github.com/odomontois")),
  Developer("oskin1", "Ilya Oskin", "ilya.arcadich@gmail.com", url("https://github.com/oskin1")),
)

ThisBuild / organization     := "tf.tofu"
ThisBuild / organizationName := "Tofu"

ThisBuild / homepage := Some(url("https://github.com/tofu-tf/tofu"))

ThisBuild / description := "Opinionated set of tools for functional programming in Scala"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/tofu-tf/tofu"),
    "git@github.com:tofu-tf/tofu.git"
  )
)
