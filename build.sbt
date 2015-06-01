import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import pl.project13.scala.sbt.SbtJmh._
import sbtrelease.ReleaseStep
import sbtrelease.ReleasePlugin.ReleaseKeys.releaseProcess
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._
import sbtunidoc.Plugin.UnidocKeys._
import ScoverageSbtPlugin._

enablePlugins(GitBranchPrompt)

ScoverageKeys.coverageMinimum := 60
ScoverageKeys.coverageFailOnMinimum := false
ScoverageKeys.coverageHighlighting := {
  if (scalaBinaryVersion.value == "2.10") false
  else true
}
ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := "docs;bench"

lazy val buildSettings = Seq(
  organization := "org.spire-math",
  scalaVersion := "2.11.6",
  crossScalaVersions := Seq("2.11.6")
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
  ),
  resolvers ++= Seq(
    "bintray/non" at "http://dl.bintray.com/non/maven",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    "tpolecat" at "http://dl.bintray.com/tpolecat/maven"
  ),
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.3.0",
    "org.spire-math" %% "algebra" % "0.2.0-SNAPSHOT",
    "org.typelevel" %% "machinist" % "0.3.0",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")
  ),
  scmInfo := Some(ScmInfo(url("https://github.com/non/cats"),
    "scm:git:git@github.com:non/cats.git"))
)

lazy val catsSettings = buildSettings ++ commonSettings ++ publishSettings ++ releaseSettings

lazy val disciplineDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.3",
  "org.typelevel" %% "discipline" % "0.2.1"
)

lazy val disciplineDependenciesJS = Seq(
  libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.12.3",
  libraryDependencies += "org.typelevel" %% "discipline" % "0.2.1"
)

lazy val docSettings = Seq(
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(coreJVM, freeJVM, stdJVM),
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

lazy val cats = project.in(file("."))
  .settings(moduleName := "root")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(noSourceSettings)
  .settings(aggregate in Test in catsJS := false)
  .aggregate(catsJVM, catsJS)
  .dependsOn(catsJVM, catsJS)

lazy val catsJVM = project.in(file(".catsJVM"))
  .settings(moduleName := "cats")
  .settings(catsSettings)
  .aggregate(macrosJVM, coreJVM, lawsJVM, testsJVM, docs, freeJVM, stdJVM, bench, stateJVM)
  .dependsOn(macrosJVM, coreJVM, lawsJVM, testsJVM, docs, freeJVM, stdJVM, bench, stateJVM)
  
lazy val catsJS = project.in(file(".catsJS"))
  .settings(moduleName := "cats")
  .settings(catsSettings)
  .settings(noPublishSettings:_*)
  .aggregate(macrosJS, coreJS, lawsJS, testsJS, freeJS, stdJS, stateJS)
  .dependsOn(macrosJS, coreJS, lawsJS, testsJS, freeJS, stdJS, stateJS)
  .enablePlugins(ScalaJSPlugin)

lazy val docs = project
  .settings(moduleName := "cats-docs")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(unidocSettings)
  .settings(site.settings)
  .settings(ghpages.settings)
  .settings(docSettings)
  .settings(tutSettings)
  .dependsOn(coreJVM, stdJVM, freeJVM)

lazy val macros = crossProject
  .settings(moduleName := "cats-macros")
  .settings(catsSettings:_*)
  .jsSettings(noPublishSettings:_*)

lazy val macrosJVM = macros.jvm 
lazy val macrosJS = macros.js

lazy val core = crossProject.dependsOn(macros)
  .settings(moduleName := "cats-core")
  .settings(catsSettings:_*)
  .settings(
    sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
  )
  .jsSettings(noPublishSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val laws = crossProject.dependsOn(macros, core, free, std)
  .settings(moduleName := "cats-laws")
  .settings(catsSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "org.spire-math" %% "algebra-laws" % "0.2.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/algebra-laws_2.11-0.2.0-SNAPSHOT.jar"
    )
  )
  .jvmSettings(libraryDependencies ++= disciplineDependencies)
  .jsSettings(disciplineDependenciesJS:_*)
  .jsSettings(noPublishSettings:_*)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

lazy val std = crossProject.dependsOn(macros, core)
  .settings(moduleName := "cats-std")
  .settings(catsSettings:_*)
  .settings(
    libraryDependencies += "org.spire-math" %% "algebra-std" % "0.2.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/algebra-std_2.11-0.2.0-SNAPSHOT.jar"
  )
  .jsSettings(noPublishSettings:_*)

lazy val stdJVM = std.jvm
lazy val stdJS = std.js

lazy val tests = crossProject.dependsOn(macros, core, free, std, laws)
  .settings(moduleName := "cats-tests")
  .settings(catsSettings:_*)
  .settings(noPublishSettings:_*)
  .settings(
    libraryDependencies ++=   Seq(
      "org.scalatest" %% "scalatest" % "2.1.3" % "test"
    )
  )
  .jvmSettings(libraryDependencies ++= disciplineDependencies)
  .jsSettings(disciplineDependenciesJS:_*)
  .jsSettings(noPublishSettings:_*)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val bench = project.dependsOn(macrosJVM, coreJVM, freeJVM, stdJVM, lawsJVM)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(jmhSettings)

lazy val free = crossProject.dependsOn(macros, core)
  .settings(moduleName := "cats-free")
  .settings(catsSettings:_*)
  .jsSettings(noPublishSettings:_*)

lazy val freeJVM = free.jvm
lazy val freeJS = free.js

lazy val state = crossProject.dependsOn(macros, core, free, tests % "test -> test")
  .settings(moduleName := "cats-state")
  .settings(catsSettings:_*)
  .jsSettings(noPublishSettings:_*)

lazy val stateJVM = state.jvm
lazy val stateJS = state.js

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/non/cats")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  autoAPIMappings := true,
  apiURL := Some(url("https://non.github.io/cats/api/")),
  publishMavenStyle := true,
  publishArtifact in packageDoc := false,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
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
  ),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishSignedArtifacts,
    setNextVersion,
    commitNextVersion,
    pushChanges
  )
)

lazy val publishSignedArtifacts = ReleaseStep(
  action = { st =>
    val extracted = st.extract
    val ref = extracted.get(thisProjectRef)
    extracted.runAggregated(publishSigned in Global in ref, st)
  },
  check = { st =>
    // getPublishTo fails if no publish repository is set up.
    val ex = st.extract
    val ref = ex.get(thisProjectRef)
    Classpaths.getPublishTo(ex.get(publishTo in Global in ref))
    st
  },
  enableCrossBuild = true
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val noSourceSettings = Seq(
  sources in Compile := Seq(),
  sources in Test := Seq()
)

addCommandAlias("validate", ";compile;test;scalastyle;test:scalastyle;unidoc;tut")

// For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
