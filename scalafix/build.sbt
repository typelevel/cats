// Use a scala version supported by scalafix.
scalaVersion in ThisBuild := org.scalameta.BuildInfo.supportedScalaVersions.last

lazy val rewrites = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % "0.5.0-M1"
)

lazy val input = project.settings(
  scalametaSourceroot := sourceDirectory.in(Compile).value,
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
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % "0.5.0-M1" % Test cross CrossVersion.full,
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
