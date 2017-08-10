import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import sbtunidoc.Plugin.UnidocKeys._
import ReleaseTransformations._
import scala.xml.transform.{RewriteRule, RuleTransformer}
import org.scalajs.sbtplugin.cross.CrossProject

lazy val botBuild = settingKey[Boolean]("Build by TravisCI instead of local dev environment")

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  //https://github.com/scoverage/sbt-scoverage/issues/72
  coverageHighlighting := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) => false
      case _ => true
    }
  }
)

organization in ThisBuild := "org.typelevel"

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
) ++ warnUnusedImport ++ update2_12 ++ xlint

lazy val commonSettings = Seq(
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  scalacOptions ++= commonScalacOptions,
  resolvers ++= Seq(
    "bintray/non" at "http://dl.bintray.com/non/maven",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %%% "simulacrum" % "0.10.0" % "compile-time",
    "org.typelevel" %%% "machinist" % "0.6.2",
    compilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.patch),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
  ),
  fork in test := true,
  parallelExecution in Test := false,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  // workaround for https://github.com/scalastyle/scalastyle-sbt-plugin/issues/47
  scalastyleSources in Compile ++= (unmanagedSourceDirectories in Compile).value,
  ivyConfigurations += config("compile-time").hide,
  unmanagedClasspath in Compile ++= update.value.select(configurationFilter("compile-time")),
  unmanagedSourceDirectories in Test ++= {
    val bd = baseDirectory.value
    if (CrossVersion.partialVersion(scalaVersion.value) exists (_._2 >= 11))
      CrossType.Pure.sharedSrcDir(bd, "test").toList map (f => file(f.getPath + "-2.11+"))
    else
      Nil
  }
) ++ warnUnusedImport ++ update2_12 ++ xlint


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
  requiresDOM := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // Only used for scala.js for now
  botBuild := scala.sys.env.get("TRAVIS").isDefined,
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(botBuild.value),
  doctestGenTests := Seq.empty,
  doctestWithDependencies := false
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
// currently sbt-doctest doesn't work in JS builds, so this has to go in the
// JVM settings. https://github.com/tkawachi/sbt-doctest/issues/52
) ++ catsDoctestSettings

lazy val includeGeneratedSrc: Setting[_] = {
  mappings in (Compile, packageSrc) ++= {
    val base = (sourceManaged in Compile).value
    (managedSources in Compile).value.map { file =>
      file -> file.relativeTo(base).get.getPath
    }
  }
}

lazy val catsSettings = commonSettings ++ publishSettings ++ scoverageSettings ++ javadocSettings

lazy val scalaCheckVersion = "1.13.5"
lazy val scalaTestVersion = "3.0.1"
lazy val disciplineVersion = "0.8"
lazy val catalystsVersion = "0.0.5"

lazy val disciplineDependencies = Seq(
  libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion,
  libraryDependencies += "org.typelevel" %%% "discipline" % disciplineVersion)

lazy val testingDependencies = Seq(
  libraryDependencies += "org.typelevel" %%% "catalysts-platform" % catalystsVersion,
  libraryDependencies += "org.typelevel" %%% "catalysts-macros" % catalystsVersion % "test",
  libraryDependencies += "org.scalatest" %%% "scalatest" % scalaTestVersion % "test")


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

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

lazy val docSettings = Seq(
  micrositeName := "Cats",
  micrositeDescription := "Lightweight, modular, and extensible library for functional programming",
  micrositeAuthor := "Typelevel contributors",
  micrositeHighlightTheme := "atom-one-light",
  micrositeHomepage := "http://typelevel.org/cats",
  micrositeBaseUrl := "cats",
  micrositeDocumentationUrl := "api",
  micrositeGithubOwner := "typelevel",
  micrositeExtraMdFiles := Map(file("CONTRIBUTING.md") -> "contributing.md"),
  micrositeGithubRepo := "cats",
  micrositePalette := Map(
    "brand-primary" -> "#5B5988",
    "brand-secondary" -> "#292E53",
    "brand-tertiary" -> "#222749",
    "gray-dark" -> "#49494B",
    "gray" -> "#7B7B7E",
    "gray-light" -> "#E5E5E6",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"),
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inProjects(docsSourcesAndProjects(scalaVersion.value)._2:_*),
  docsMappingsAPIDir := "api",
  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docsMappingsAPIDir),
  ghpagesNoJekyll := false,
  fork in tut := true,
  fork in (ScalaUnidoc, unidoc) := true,
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-Xfatal-warnings",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  ),
  git.remoteRepo := "git@github.com:typelevel/cats.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg",
  includeFilter in Jekyll := (includeFilter in makeSite).value
)

lazy val docs = project
  .enablePlugins(MicrositesPlugin)
  .settings(moduleName := "cats-docs")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(unidocSettings)
  .settings(ghpages.settings)
  .settings(docSettings)
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
  .settings(noPublishSettings)
  .settings(catsSettings)
  .settings(commonJvmSettings)
  .aggregate(macrosJVM, kernelJVM, kernelLawsJVM, coreJVM, lawsJVM, freeJVM, testkitJVM, testsJVM, jvm, docs, bench)
  .dependsOn(macrosJVM, kernelJVM, kernelLawsJVM, coreJVM, lawsJVM, freeJVM, testkitJVM, testsJVM % "test-internal -> test", jvm, bench % "compile-internal;test-internal -> test")

lazy val catsJS = project.in(file(".catsJS"))
  .settings(moduleName := "cats")
  .settings(noPublishSettings)
  .settings(catsSettings)
  .settings(commonJsSettings)
  .aggregate(macrosJS, kernelJS, kernelLawsJS, coreJS, lawsJS, freeJS, testkitJS, testsJS, js)
  .dependsOn(macrosJS, kernelJS, kernelLawsJS, coreJS, lawsJS, freeJS, testkitJS, testsJS % "test-internal -> test", js)
  .enablePlugins(ScalaJSPlugin)


lazy val macros = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "cats-macros", name := "Cats macros")
  .settings(catsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(coverageEnabled := false)
  .settings(scalacOptions := scalacOptions.value.filter(_ != "-Xfatal-warnings"))

lazy val macrosJVM = macros.jvm
lazy val macrosJS = macros.js

val binaryCompatibleVersion = "0.8.0"

lazy val kernel = crossProject.crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(moduleName := "cats-kernel", name := "Cats kernel")
  .settings(kernelSettings: _*)
  .settings(publishSettings: _*)
  .settings(scoverageSettings: _*)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(KernelBoiler.gen).taskValue)
  .settings(includeGeneratedSrc)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings((commonJvmSettings ++
    (mimaPreviousArtifacts := {
      if (scalaVersion.value startsWith "2.12")
        Set()
      else
        Set("org.typelevel" %% "cats-kernel" % binaryCompatibleVersion)
    })):_*)

lazy val kernelJVM = kernel.jvm
lazy val kernelJS = kernel.js

lazy val kernelLaws = crossProject.crossType(CrossType.Pure)
  .in(file("kernel-laws"))
  .settings(moduleName := "cats-kernel-laws", name := "Cats kernel laws")
  .settings(kernelSettings: _*)
  .settings(publishSettings: _*)
  .settings(scoverageSettings: _*)
  .settings(disciplineDependencies: _*)
  .settings(testingDependencies: _*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(coverageEnabled := false)
  .dependsOn(kernel)

lazy val kernelLawsJVM = kernelLaws.jvm
lazy val kernelLawsJS = kernelLaws.js

lazy val core = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, kernel)
  .settings(moduleName := "cats-core", name := "Cats core")
  .settings(catsSettings:_*)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(Boilerplate.gen).taskValue)
  .settings(includeGeneratedSrc)
  .configureCross(disableScoverage210Jvm)
  .configureCross(disableScoverage210Js)
  .settings(libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % "test")
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val laws = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, kernel, core, kernelLaws)
  .settings(moduleName := "cats-laws", name := "Cats laws")
  .settings(catsSettings:_*)
  .settings(disciplineDependencies:_*)
  .configureCross(disableScoverage210Jvm)
  .settings(testingDependencies: _*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(coverageEnabled := false)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

lazy val free = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free", name := "Cats Free")
  .settings(catsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val freeJVM = free.jvm
lazy val freeJS = free.js

lazy val tests = crossProject.crossType(CrossType.Pure)
  .dependsOn(testkit % "test")
  .settings(moduleName := "cats-tests")
  .settings(catsSettings:_*)
  .settings(noPublishSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js


lazy val testkit = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, laws)
  .settings(moduleName := "cats-testkit")
  .settings(catsSettings:_*)
  .settings(disciplineDependencies:_*)
  .settings(
    libraryDependencies += "org.scalatest" %%% "scalatest" % scalaTestVersion)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val testkitJVM = testkit.jvm
lazy val testkitJS = testkit.js

// bench is currently JVM-only

lazy val bench = project.dependsOn(macrosJVM, coreJVM, freeJVM, lawsJVM)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .settings(coverageEnabled := false)
  .settings(libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.7"))
  .enablePlugins(JmhPlugin)

// cats-js is JS-only
lazy val js = project
  .dependsOn(macrosJS, coreJS, testsJS % "test-internal -> test")
  .settings(moduleName := "cats-js")
  .settings(catsSettings:_*)
  .settings(commonJsSettings:_*)
  .configure(disableScoverage210Js)
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
        <id>edmundnoble</id>
        <name>Edmund Noble</name>
        <url>https://github.com/edmundnoble/</url>
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
        <id>djspiewak</id>
        <name>Daniel Spiewak</name>
        <url>https://github.com/djspiewak/</url>
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
addCommandAlias("buildJVM", "catsJVM/test")

addCommandAlias("validateJVM", ";scalastyle;buildJVM;mimaReportBinaryIssues;makeMicrosite")

addCommandAlias("validateJS", ";catsJS/compile;testsJS/test;js/test")

addCommandAlias("validateKernelJS", "kernelLawsJS/test")

addCommandAlias("validateFreeJS", "freeJS/test") //separated due to memory constraint on travis

addCommandAlias("validate", ";clean;validateJS;validateKernelJS;validateFreeJS;validateJVM")

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
  libraryDependencies += scalaOrganization.value %%% "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.patch),
              "org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary
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
  releaseVcsSign := true,
  useGpg := true,   // bouncycastle has bugs with subkeys, so we use gpg instead
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
    runClean,
    releaseStepCommand("validate"),
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
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)

def disableScoverage210Js(crossProject: CrossProject) =
  crossProject
  .jsSettings(
    coverageEnabled := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) => false
        case _ => coverageEnabled.value
      }
    }
  )

def disableScoverage210Js: Project ⇒ Project = p =>
  p.settings(
    coverageEnabled := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) => false
        case _ => coverageEnabled.value
      }
    }
  )

def disableScoverage210Jvm(crossProject: CrossProject) =
  crossProject
  .jvmSettings(
    coverageEnabled := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) => false
        case _ => coverageEnabled.value
      }
    }
  )

lazy val update2_12 = Seq(
  scalacOptions -= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => "-Yinline-warnings"
      case _ => ""
    }
  }
)

lazy val xlint = Seq(
  scalacOptions += {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => "-Xlint:-unused,_"
      case _ => "-Xlint"
    }
  }
)
