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

lazy val v1_0_0_input = project.in(file("v1_0_0/input"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % "0.9.0"
    ),
    scalacOptions += "-language:higherKinds"
  )

lazy val v1_0_0_output = project.in(file("v1_0_0/output"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.0.0",
      "org.typelevel" %% "cats-free" % "1.0.0"
    ),
    scalacOptions += "-language:higherKinds"
  )

lazy val v1_0_0_tests = project.in(file("v1_0_0/tests"))
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    compile.in(Compile) :=
      compile.in(Compile).dependsOn(compile.in(v1_0_0_input, Compile)).value,
    scalafixTestkitOutputSourceDirectories :=
      sourceDirectories.in(v1_0_0_output, Compile).value,
    scalafixTestkitInputSourceDirectories :=
      sourceDirectories.in(v1_0_0_input, Compile).value,
    scalafixTestkitInputClasspath :=
      fullClasspath.in(v1_0_0_input, Compile).value
  )
  .dependsOn(v1_0_0_input, rules)
  .enablePlugins(ScalafixTestkitPlugin)

lazy val v2_2_0_input = project.in(file("v2_2_0/input"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.1.0"
    ),
    scalacOptions ++= Seq("-language:higherKinds", "-P:semanticdb:synthetics:on")
  )

lazy val v2_2_0_output = project.in(file("v2_2_0/output"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.2.0-RC2"
    ),
    scalacOptions += "-language:higherKinds"
  )

lazy val v2_2_0_tests = project.in(file("v2_2_0/tests"))
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    compile.in(Compile) :=
      compile.in(Compile).dependsOn(compile.in(v2_2_0_input, Compile)).value,
    scalafixTestkitOutputSourceDirectories :=
      sourceDirectories.in(v2_2_0_output, Compile).value,
    scalafixTestkitInputSourceDirectories :=
      sourceDirectories.in(v2_2_0_input, Compile).value,
    scalafixTestkitInputClasspath :=
      fullClasspath.in(v2_2_0_input, Compile).value
  )
  .dependsOn(v2_2_0_input, rules)
  .enablePlugins(ScalafixTestkitPlugin)
