val V = _root_.scalafix.Versions

inThisBuild(
  List(
    scalaVersion in ThisBuild := V.scala212,
    addCompilerPlugin(scalafixSemanticdb),
    scalacOptions += "-Yrangepos"
  ))

lazy val rules = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.version
)

lazy val input = project.settings(
  scalacOptions += {
    val sourceroot = sourceDirectory.in(Compile).value / "scala"
    s"-P:semanticdb:sourceroot:$sourceroot"
  },
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats" % "0.9.0"
  ),
  scalacOptions += "-language:higherKinds"
)

lazy val output = project.settings(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "1.0.0",
    "org.typelevel" %% "cats-free" % "1.0.0"
  ),
  scalacOptions += "-language:higherKinds"
)

lazy val tests = project
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.version % Test cross CrossVersion.full,
    scalafixTestkitOutputSourceDirectories :=
      sourceDirectories.in(output, Compile).value,
    scalafixTestkitInputSourceDirectories :=
      sourceDirectories.in(input, Compile).value,
    scalafixTestkitInputClasspath :=
      fullClasspath.in(input, Compile).value
  )
  .dependsOn(input, rules)
  .enablePlugins(ScalafixTestkitPlugin)
