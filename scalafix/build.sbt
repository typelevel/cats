val V = _root_.scalafix.sbt.BuildInfo

inThisBuild(
  List(
    scalaVersion in ThisBuild := V.scala212,
    addCompilerPlugin(scalafixSemanticdb),
    scalacOptions += "-Yrangepos"
  ))

lazy val rules = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion
)

lazy val input = project.settings(
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
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    compile.in(Compile) :=
      compile.in(Compile).dependsOn(compile.in(input, Compile)).value,
    scalafixTestkitOutputSourceDirectories :=
      sourceDirectories.in(output, Compile).value,
    scalafixTestkitInputSourceDirectories :=
      sourceDirectories.in(input, Compile).value,
    scalafixTestkitInputClasspath :=
      fullClasspath.in(input, Compile).value
  )
  .dependsOn(input, rules)
  .enablePlugins(ScalafixTestkitPlugin)
