addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.21")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")
addSbtPlugin("com.github.gseitz" %% "sbt-release" % "1.0.13")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.7.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.0")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.9.7")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.4")
addSbtPlugin("com.47deg" % "sbt-microsites" % "1.2.1")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.9")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.2.0")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")

/* Temporarily disabling sbt-hydra, see #2870.
resolvers += Resolver.url(
  "Triplequote Plugins Releases",
  url("https://repo.triplequote.com/artifactory/sbt-plugins-release/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.triplequote" % "sbt-hydra" % "2.1.5")
 */
