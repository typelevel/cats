import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import pl.project13.scala.sbt.SbtJmh._
import sbtunidoc.Plugin.UnidocKeys._
import ScoverageSbtPlugin._

lazy val scoverageSettings = Seq(
  ScoverageKeys.coverageMinimum := 60,
  ScoverageKeys.coverageFailOnMinimum := false,
  ScoverageKeys.coverageHighlighting := scalaBinaryVersion.value != "2.10",
  ScoverageKeys.coverageExcludedPackages := "cats\\.bench\\..*"
)

lazy val buildSettings = Seq(
  organization := "org.spire-math",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7")
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yinline-warnings",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) => Seq("-Ywarn-unused-import")
    case _             => Seq.empty
  }),
  scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Ywarn-unused-import")),
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  resolvers ++= Seq(
    "bintray/non" at "http://dl.bintray.com/non/maven",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.3.0",
    "org.spire-math" %% "algebra" % "0.2.1",
    "org.typelevel" %% "machinist" % "0.3.0",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")
  ),
  scmInfo := Some(ScmInfo(url("https://github.com/non/cats"),
    "scm:git:git@github.com:non/cats.git")),
  commands += gitSnapshots
)

lazy val catsSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings

lazy val disciplineDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.4",
  "org.typelevel" %% "discipline" % "0.3"
)

lazy val docSettings = Seq(
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(core, free, std, state),
  site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "api"),
  site.addMappingsToSiteDir(tut, "_tut"),
  ghpagesNoJekyll := false,
  siteMappings += file("CONTRIBUTING.md") -> "contributing.md",
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath
  ),
  git.remoteRepo := "git@github.com:non/cats.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md"
)

lazy val docs = project
  .settings(moduleName := "cats-docs")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(unidocSettings)
  .settings(site.settings)
  .settings(ghpages.settings)
  .settings(docSettings)
  .settings(tutSettings)
  .settings(tutScalacOptions ~= (_.filterNot(_ == "-Ywarn-unused-import")))
  .dependsOn(core, std, free, state)

lazy val cats = project.in(file("."))
  .settings(moduleName := "cats")
  .settings(catsSettings)
  .aggregate(macros, core, laws, free, std, state, tests, docs, bench)
  .dependsOn(macros, core, laws, free, std, state % "compile;test-internal -> test",
             tests % "test-internal -> test", bench % "compile-internal;test-internal -> test")

lazy val macros = project
  .settings(moduleName := "cats-macros")
  .settings(catsSettings)

lazy val core = project.dependsOn(macros)
  .settings(moduleName := "cats-core")
  .settings(catsSettings)
  .settings(
    sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
  )

lazy val laws = project.dependsOn(macros, core, std)
  .settings(moduleName := "cats-laws")
  .settings(catsSettings)
  .settings(
    libraryDependencies ++= disciplineDependencies ++ Seq(
      "org.spire-math" %% "algebra-laws" % "0.2.1"
    )
  )

lazy val std = project.dependsOn(macros, core)
  .settings(moduleName := "cats-std")
  .settings(catsSettings)
  .settings(
    libraryDependencies += "org.spire-math" %% "algebra-std" % "0.2.1"
  )

lazy val tests = project.dependsOn(macros, core, std, laws)
  .settings(moduleName := "cats-tests")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++= disciplineDependencies ++ Seq(
      "org.scalatest" %% "scalatest" % "2.2.5" % "test"
    )
  )

lazy val bench = project.dependsOn(macros, core, free, std, laws)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(jmhSettings)

lazy val free = project.dependsOn(macros, core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free")
  .settings(catsSettings)

lazy val state = project.dependsOn(macros, core, free, tests % "test-internal -> test")
  .settings(moduleName := "cats-state")
  .settings(catsSettings)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/non/cats")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  autoAPIMappings := true,
  apiURL := Some(url("https://non.github.io/cats/api/")),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in packageDoc := false,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>non</id>
        <name>Erik Osheim</name>
        <url>http://github.com/non/</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

addCommandAlias("validate", ";compile;test;scalastyle;test:scalastyle;unidoc;tut")

def gitSnapshots = Command.command("gitSnapshots") { state =>
  val extracted = Project extract state
  val newVersion = Seq(version in ThisBuild := git.gitDescribedVersion.value.get + "-SNAPSHOT")
  extracted.append(newVersion, state)
}

// For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
