// Use a scala version supported by scalafix.
val V = _root_.scalafix.Versions
scalaVersion in ThisBuild := V.scala212

lazy val rewrites = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.version
)

lazy val input = project.settings(
  scalafixSourceroot := sourceDirectory.in(Compile).value,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats" % "0.9.0"
  )
)

lazy val output = project.settings(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "1.0.0-MF",
    "org.typelevel" %% "cats-free" % "1.0.0-MF"
  )
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
  .dependsOn(input, rewrites)
  .enablePlugins(BuildInfoPlugin)
