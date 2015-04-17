import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import pl.project13.scala.sbt.SbtJmh._
import sbtrelease.ReleaseStep
import sbtrelease.ReleasePlugin.ReleaseKeys.releaseProcess
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._
import sbtunidoc.Plugin.UnidocKeys._

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
    Resolver.sonatypeRepo("releases")
  ),
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.2.0",
    "org.spire-math" %% "algebra" % "0.2.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/algebra_2.11-0.2.0-SNAPSHOT.jar",
    "org.typelevel" %% "machinist" % "0.3.0",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2")
  ),
  scmInfo := Some(ScmInfo(url("https://github.com/non/cats"),
    "git@github.com:non/cats.git"))
)

lazy val catsSettings = buildSettings ++ commonSettings ++ publishSettings ++ releaseSettings

lazy val disciplineDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.3",
  "org.typelevel" %% "discipline" % "0.2.1"
)

lazy val docSettings = Seq(
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(core, laws, free, std),
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
  .settings(tutSettings)
  .settings(docSettings)
  .settings(tutSettings)
  .dependsOn(core, std, free)

lazy val cats = project.in(file("."))
  .settings(catsSettings)
  .settings(noPublishSettings)
  .aggregate(macros, core, laws, tests, docs, free, std, bench)
  .dependsOn(macros, core, laws, tests, docs, free, std, bench)

lazy val macros = project
  .settings(moduleName := "cats-macros")
  .settings(catsSettings)

lazy val core = project.dependsOn(macros)
  .settings(moduleName := "cats-core")
  .settings(catsSettings)
  .settings(
    sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
  )

lazy val laws = project.dependsOn(macros, core, free, std)
  .settings(moduleName := "cats-laws")
  .settings(catsSettings)
  .settings(
    libraryDependencies ++= disciplineDependencies ++ Seq(
      "org.spire-math" %% "algebra-laws" % "0.2.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/algebra-laws_2.11-0.2.0-SNAPSHOT.jar"
    )
  )

lazy val std = project.dependsOn(macros, core)
  .settings(moduleName := "cats-std")
  .settings(catsSettings)
  .settings(
    libraryDependencies += "org.spire-math" %% "algebra-std" % "0.2.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/algebra-std_2.11-0.2.0-SNAPSHOT.jar"
  )

lazy val tests = project.dependsOn(macros, core, free, std, laws)
  .settings(moduleName := "cats-tests")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++= disciplineDependencies ++ Seq(
      "org.scalatest" %% "scalatest" % "2.1.3" % "test"
    )
  )

lazy val bench = project.dependsOn(macros, core, free, std, laws)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(jmhSettings)

lazy val free = project.dependsOn(macros, core)
  .settings(moduleName := "cats-free")
  .settings(catsSettings)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/non/cats")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  autoAPIMappings := true,
  apiURL := Some(url("https://non.github.io/cats/api/")),
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

addCommandAlias("validate", ";compile;test;scalastyle;test:scalastyle;unidoc;tut")
