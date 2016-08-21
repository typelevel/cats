import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import sbtunidoc.Plugin.UnidocKeys._
import ReleaseTransformations._
import ScoverageSbtPlugin._
import scala.xml.transform.{RewriteRule, RuleTransformer}

lazy val botBuild = settingKey[Boolean]("Build by TravisCI instead of local dev environment")

lazy val scoverageSettings = Seq(
  ScoverageKeys.coverageMinimum := 60,
  ScoverageKeys.coverageFailOnMinimum := false,
  ScoverageKeys.coverageHighlighting := scalaBinaryVersion.value != "2.10",
  ScoverageKeys.coverageExcludedPackages := "cats\\.bench\\..*",
  // don't include scoverage as a dependency in the pom
  // see issue #980
  // this code was copied from https://github.com/mongodb/mongo-spark
  pomPostProcess := { (node: xml.Node) =>
    new RuleTransformer(
      new RewriteRule {
        override def transform(node: xml.Node): Seq[xml.Node] = node match {
          case e: xml.Elem
              if e.label == "dependency" && e.child.exists(child => child.label == "groupId" && child.text == "org.scoverage") => Nil
          case _ => Seq(node)

        }

      }).transform(node).head
  }
)

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8")
)

lazy val catsDoctestSettings = Seq(
  doctestWithDependencies := false
)

lazy val kernelSettings = Seq(
  // don't warn on value discarding because it's broken on 2.10 with @sp(Unit)
  scalacOptions ++= commonScalacOptions.filter(_ != "-Ywarn-value-discard"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")),
  parallelExecution in Test := false,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings")
) ++ warnUnusedImport

lazy val commonSettings = Seq(
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  scalacOptions ++= commonScalacOptions,
  resolvers ++= Seq(
    "bintray/non" at "http://dl.bintray.com/non/maven",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %%% "simulacrum" % "0.8.0",
    "org.typelevel" %%% "machinist" % "0.4.1",
    compilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
  ),
  fork in test := true,
  parallelExecution in Test := false,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  // workaround for https://github.com/scalastyle/scalastyle-sbt-plugin/issues/47
  (scalastyleSources in Compile) <++= unmanagedSourceDirectories in Compile
) ++ warnUnusedImport

lazy val tagName = Def.setting{
 s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val commonJsSettings = Seq(
  scalacOptions += {
    val tagOrHash =
      if (isSnapshot.value) sys.process.Process("git rev-parse HEAD").lines_!.head
      else tagName.value
    val a = (baseDirectory in LocalRootProject).value.toURI.toString
    val g = "https://raw.githubusercontent.com/typelevel/cats/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  // Using Rhino as jsEnv to build scala.js code can lead to OOM, switch to PhantomJS by default
  scalaJSUseRhino := false,
  requiresDOM := false,
  jsEnv := NodeJSEnv().value,
  // Only used for scala.js for now
  botBuild := scala.sys.env.get("TRAVIS").isDefined,
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(botBuild.value)
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
// currently sbt-doctest doesn't work in JS builds, so this has to go in the
// JVM settings. https://github.com/tkawachi/sbt-doctest/issues/52
) ++ catsDoctestSettings

lazy val catsSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings ++ javadocSettings

lazy val scalacheckVersion = "1.12.5"

lazy val disciplineDependencies = Seq(
  libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion,
  libraryDependencies += "org.typelevel" %%% "discipline" % "0.4")

lazy val testingDependencies = Seq(
  libraryDependencies += "org.typelevel" %%% "catalysts-platform" % "0.0.2",
  libraryDependencies += "org.typelevel" %%% "catalysts-macros" % "0.0.2" % "test",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0-M8" % "test")


/**
  * Remove 2.10 projects from doc generation, as the macros used in the projects
  * cause problems generating the documentation on scala 2.10. As the APIs for 2.10
  * and 2.11 are the same this has no effect on the resultant documentation, though
  * it does mean that the scaladocs cannot be generated when the build is in 2.10 mode.
  */
def docsSourcesAndProjects(sv: String): (Boolean, Seq[ProjectReference]) =
  CrossVersion.partialVersion(sv) match {
    case Some((2, 10)) => (false, Nil)
    case _ => (true, Seq(kernelJVM, coreJVM, freeJVM))
  }

lazy val javadocSettings = Seq(
  sources in (Compile, doc) := (if (docsSourcesAndProjects(scalaVersion.value)._1) (sources in (Compile, doc)).value else Nil)
)

lazy val docSettings = Seq(
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inProjects(docsSourcesAndProjects(scalaVersion.value)._2:_*),
  site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "api"),
  site.addMappingsToSiteDir(tut, "_tut"),
  ghpagesNoJekyll := false,
  fork in tut := true,
  fork in (ScalaUnidoc, unidoc) := true,
  siteMappings += file("CONTRIBUTING.md") -> "contributing.md",
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-Xfatal-warnings",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  ),
  git.remoteRepo := "git@github.com:typelevel/cats.git",
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
  .settings(tutScalacOptions ~= (_.filterNot(Set("-Ywarn-unused-import", "-Ywarn-dead-code"))))
  .settings(commonJvmSettings)
  .dependsOn(coreJVM, freeJVM)

lazy val cats = project.in(file("."))
  .settings(moduleName := "root")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .aggregate(catsJVM, catsJS)
  .dependsOn(catsJVM, catsJS, testsJVM % "test-internal -> test", bench % "compile-internal;test-internal -> test")

lazy val catsJVM = project.in(file(".catsJVM"))
  .settings(moduleName := "cats")
  .settings(catsSettings)
  .settings(commonJvmSettings)
  .aggregate(macrosJVM, kernelJVM, kernelLawsJVM, coreJVM, lawsJVM, freeJVM, testsJVM, jvm, docs, bench)
  .dependsOn(macrosJVM, kernelJVM, kernelLawsJVM, coreJVM, lawsJVM, freeJVM, testsJVM % "test-internal -> test", jvm, bench % "compile-internal;test-internal -> test")

lazy val catsJS = project.in(file(".catsJS"))
  .settings(moduleName := "cats")
  .settings(catsSettings)
  .settings(commonJsSettings)
  .aggregate(macrosJS, kernelJS, kernelLawsJS, coreJS, lawsJS, freeJS, testsJS, js)
  .dependsOn(macrosJS, kernelJS, kernelLawsJS, coreJS, lawsJS, freeJS, testsJS % "test-internal -> test", js)
  .enablePlugins(ScalaJSPlugin)


lazy val macros = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "cats-macros")
  .settings(catsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .settings(scalacOptions := scalacOptions.value.filter(_ != "-Xfatal-warnings"))

lazy val macrosJVM = macros.jvm
lazy val macrosJS = macros.js

lazy val kernel = crossProject.crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(moduleName := "cats-kernel")
  .settings(kernelSettings: _*)
  .settings(buildSettings: _*)
  .settings(publishSettings: _*)
  .settings(scoverageSettings: _*)
  .settings(sourceGenerators in Compile <+= (sourceManaged in Compile).map(KernelBoiler.gen))
  .jsSettings(commonJsSettings:_*)
  .jvmSettings((commonJvmSettings ++ (mimaPreviousArtifacts := Set("org.typelevel" %% "cats-kernel" % "0.6.0"))):_*)

lazy val kernelJVM = kernel.jvm
lazy val kernelJS = kernel.js

lazy val kernelLaws = crossProject.crossType(CrossType.Pure)
  .in(file("kernel-laws"))
  .settings(moduleName := "cats-kernel-laws")
  .settings(kernelSettings: _*)
  .settings(buildSettings: _*)
  .settings(publishSettings: _*)
  .settings(scoverageSettings: _*)
  .settings(disciplineDependencies: _*)
  .settings(testingDependencies: _*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .dependsOn(kernel)

lazy val kernelLawsJVM = kernelLaws.jvm
lazy val kernelLawsJS = kernelLaws.js

lazy val core = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, kernel)
  .settings(moduleName := "cats-core")
  .settings(catsSettings:_*)
  .settings(sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen))
  .settings(libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion % "test")
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val laws = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, kernel, core, kernelLaws)
  .settings(moduleName := "cats-laws")
  .settings(catsSettings:_*)
  .settings(disciplineDependencies:_*)
  .settings(libraryDependencies ++= Seq("org.typelevel" %%% "catalysts-platform" % "0.0.2"))
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

lazy val free = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free")
  .settings(catsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val freeJVM = free.jvm
lazy val freeJS = free.js

lazy val tests = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, laws)
  .settings(moduleName := "cats-tests")
  .settings(catsSettings:_*)
  .settings(disciplineDependencies:_*)
  .settings(noPublishSettings:_*)
  .settings(testingDependencies: _*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

// bench is currently JVM-only
lazy val bench = project.dependsOn(macrosJVM, coreJVM, freeJVM, lawsJVM)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .settings(libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.5"))
  .enablePlugins(JmhPlugin)

// cats-js is JS-only
lazy val js = project
  .dependsOn(macrosJS, coreJS, testsJS % "test-internal -> test")
  .settings(moduleName := "cats-js")
  .settings(catsSettings:_*)
  .settings(commonJsSettings:_*)
  .enablePlugins(ScalaJSPlugin)

// cats-jvm is JVM-only
lazy val jvm = project
  .dependsOn(macrosJVM, coreJVM, testsJVM % "test-internal -> test")
  .settings(moduleName := "cats-jvm")
  .settings(catsSettings:_*)
  .settings(commonJvmSettings:_*)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/typelevel/cats")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/typelevel/cats"), "scm:git:git@github.com:typelevel/cats.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://typelevel.org/cats/api/")),
  pomExtra := (
    <developers>
      <developer>
        <id>ceedubs</id>
        <name>Cody Allen</name>
        <url>https://github.com/ceedubs/</url>
      </developer>
      <developer>
        <id>rossabaker</id>
        <name>Ross Baker</name>
        <url>https://github.com/rossabaker/</url>
      </developer>
      <developer>
        <id>johnynek</id>
        <name>P. Oscar Boykin</name>
        <url>https://github.com/johnynek/</url>
      </developer>
      <developer>
        <id>travisbrown</id>
        <name>Travis Brown</name>
        <url>https://github.com/travisbrown/</url>
      </developer>
      <developer>
        <id>adelbertc</id>
        <name>Adelbert Chang</name>
        <url>https://github.com/adelbertc/</url>
      </developer>
      <developer>
        <id>peterneyens</id>
        <name>Peter Neyens</name>
        <url>https://github.com/peterneyens/</url>
      </developer>
      <developer>
        <id>tpolecat</id>
        <name>Rob Norris</name>
        <url>https://github.com/tpolecat/</url>
      </developer>
      <developer>
        <id>stew</id>
        <name>Mike O'Connor</name>
        <url>https://github.com/stew/</url>
      </developer>
      <developer>
        <id>non</id>
        <name>Erik Osheim</name>
        <url>https://github.com/non/</url>
      </developer>
      <developer>
        <id>mpilquist</id>
        <name>Michael Pilquist</name>
        <url>https://github.com/mpilquist/</url>
      </developer>
      <developer>
        <id>milessabin</id>
        <name>Miles Sabin</name>
        <url>https://github.com/milessabin/</url>
      </developer>
      <developer>
        <id>fthomas</id>
        <name>Frank Thomas</name>
        <url>https://github.com/fthomas/</url>
      </developer>
      <developer>
        <id>julien-truffaut</id>
        <name>Julien Truffaut</name>
        <url>https://github.com/julien-truffaut/</url>
      </developer>
      <developer>
        <id>kailuowang</id>
        <name>Kailuo Wang</name>
        <url>https://github.com/kailuowang/</url>
      </developer>
    </developers>
  )
) ++ credentialSettings ++ sharedPublishSettings ++ sharedReleaseProcess

// These aliases serialise the build for the benefit of Travis-CI.
addCommandAlias("buildJVM", ";macrosJVM/compile;coreJVM/compile;kernelLawsJVM/compile;lawsJVM/compile;freeJVM/compile;kernelLawsJVM/test;coreJVM/test;testsJVM/test;freeJVM/test;jvm/test;bench/test")

addCommandAlias("validateJVM", ";scalastyle;buildJVM;makeSite")

addCommandAlias("validateJS", ";macrosJS/compile;kernelJS/compile;coreJS/compile;kernelLawsJS/compile;lawsJS/compile;kernelLawsJS/test;testsJS/test;js/test")

addCommandAlias("validate", ";clean;validateJS;validateJVM")

////////////////////////////////////////////////////////////////////////////////////////////////////
// Base Build Settings - Should not need to edit below this line.
// These settings could also come from another file or a plugin.
// The only issue if coming from a plugin is that the Macro lib versions
// are hard coded, so an overided facility would be required.

addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.scala-lang" %%% "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" %% "paradise" % "2.0.1" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.0.1" cross CrossVersion.binary
        )
    }
  }
)

lazy val commonScalacOptions = Seq(
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
)

lazy val sharedPublishSettings = Seq(
  releaseCrossBuild := true,
  releaseTagName := tagName.value,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  }
)

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    //runClean, // disabled to reduce memory usage during release
    //runTest, // temporarily disabled for 0.7.0
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
    pushChanges)
)

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))
)

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)
