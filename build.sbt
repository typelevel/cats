organization in ThisBuild := "org.spire-math"
homepage in ThisBuild := Some(url("http://github.com/non/cats"))
licenses in ThisBuild := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

scalaVersion in ThisBuild := "2.11.5"
crossScalaVersions in ThisBuild := Seq("2.10.4", "2.11.5")

// comprehensive tpolecat-recommended scalac options
scalacOptions in ThisBuild ++= (
  "-deprecation" ::           
  "-encoding" :: "UTF-8" ::
  "-feature" ::                
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  "-unchecked" ::
  "-Xfatal-warnings" ::       
  "-Xlint" ::
  "-Yno-adapted-args" ::       
  "-Ywarn-dead-code" ::
  "-Ywarn-numeric-widen" ::   
  "-Ywarn-value-discard" ::
  "-Xfuture" ::
  Nil
)

resolvers in ThisBuild +=
  "bintray/non" at "http://dl.bintray.com/non/maven"

libraryDependencies in ThisBuild ++=
  "org.spire-math" %% "algebra" % "0.2.0-SNAPSHOT" ::
  "org.typelevel" %% "machinist" % "0.3.0" ::
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2") ::
  "com.github.mpilquist" %% "simulacrum" % "0.1.0" ::
  compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full) ::
  Nil

publishMavenStyle in ThisBuild := true
publishArtifact in ThisBuild in Test := false
pomIncludeRepository in ThisBuild := { _ => false }

publishTo in ThisBuild <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra in ThisBuild := (
<scm>
  <url>git@github.com:non/cats.git</url>
  <connection>scm:git:git@github.com:non/cats.git</connection>
</scm>
<developers>
  <developer>
    <id>non</id>
    <name>Erik Osheim</name>
    <url>http://github.com/non/</url>
  </developer>
</developers>
)
