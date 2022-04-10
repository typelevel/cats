addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.34")
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")
addSbtPlugin("com.github.sbt" %% "sbt-release" % "1.1.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.2")
addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.10.0")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.12")
addSbtPlugin("com.47deg" % "sbt-microsites" % "1.3.4")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.24")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.7.1")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.14.2")

/* Temporarily disabling sbt-hydra, see #2870.
resolvers += Resolver.url(
  "Triplequote Plugins Releases",
  url("https://repo.triplequote.com/artifactory/sbt-plugins-release/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.triplequote" % "sbt-hydra" % "2.1.5")
 */
