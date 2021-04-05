logLevel := Level.Warn

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.27")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.7")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.19")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")

addSbtPlugin("com.timushev.sbt" % "sbt-rewarn" % "0.1.3")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.17")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.2")
addSbtPlugin("com.typesafe.sbt"       % "sbt-site"                   % "1.4.1")
addSbtPlugin("com.lightbend.paradox"  % "sbt-paradox"                % "0.8.0")
addSbtPlugin("com.lightbend.paradox"  % "sbt-paradox-apidoc"         % "0.10")
addSbtPlugin("io.github.jonas"        % "sbt-paradox-material-theme" % "0.6.0")
addSbtPlugin("com.typesafe.sbt"       % "sbt-license-report"         % "1.2.0")
