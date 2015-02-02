name := "cats-laws"

libraryDependencies ++=
  "org.scalacheck" %% "scalacheck" % "1.11.3" ::
  "org.typelevel" %% "discipline" % "0.2.1" ::
  "org.spire-math" %% "algebra-laws" % "0.2.0-SNAPSHOT" ::
  Nil
