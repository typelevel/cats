addSbtCoursier
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")
addSbtPlugin("com.github.gseitz" %% "sbt-release" % "1.0.11")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0-M2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.3.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")
addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.8.0")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.5")
addSbtPlugin("com.47deg" % "sbt-microsites" % "0.9.1")
addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.2")
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.12")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.28")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.9")
addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

/* Temporarily disabling sbt-hydra, see #2870.
resolvers += Resolver.url(
  "Triplequote Plugins Releases",
  url("https://repo.triplequote.com/artifactory/sbt-plugins-release/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.triplequote" % "sbt-hydra" % "2.1.5")
 */
