// Use a scala version supported by Scalafix.
val V = _root_.scalafix.Versions
scalaVersion in ThisBuild := V.scala212

lazy val rules = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.version
)

lazy val input = project.settings(
  scalafixSourceroot := sourceDirectory.in(Compile).value,
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
    buildInfoPackage := "fix",
    buildInfoKeys := Seq[BuildInfoKey](
      "inputSourceroot" ->
        sourceDirectory.in(input, Compile).value,
      "outputSourceroot" ->
        sourceDirectory.in(output, Compile).value,
      "inputClassdirectory" ->
        classDirectory.in(input, Compile).value
    )
  )
  .dependsOn(input, rules)
  .enablePlugins(BuildInfoPlugin)
