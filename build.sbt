import microsites._
import ReleaseTransformations._
import scala.xml.transform.{RewriteRule, RuleTransformer}
import org.scalajs.sbtplugin.cross.CrossProject

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

val CompileTime = config("compile-time").hide

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
    "com.github.mpilquist" %%% "simulacrum" % "0.11.0" % CompileTime,
    "org.typelevel" %%% "machinist" % "0.6.2",
    compilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.patch),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
  ),
  fork in test := true,
  parallelExecution in Test := false,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  ivyConfigurations += CompileTime,
  unmanagedClasspath in Compile ++= update.value.select(configurationFilter(CompileTime.name)),
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
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(isTravisBuild.value),
  // currently sbt-doctest doesn't work in JS builds
  // https://github.com/tkawachi/sbt-doctest/issues/52
  doctestGenTests := Seq.empty
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

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
lazy val scalaTestVersion = "3.0.3"
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
  micrositeAuthor := "Cats contributors",
  micrositeFooterText := Some(
    """
      |<p>© 2017 <a href="https://github.com/typelevel/cats#maintainers">The Cats Maintainers</a></p>
      |<p style="font-size: 80%; margin-top: 10px">Website built with <a href="https://47deg.github.io/sbt-microsites/">sbt-microsites © 2016 47 Degrees</a></p>
      |""".stripMargin),
  micrositeHighlightTheme := "atom-one-light",
  micrositeHomepage := "http://typelevel.org/cats/",
  micrositeBaseUrl := "cats",
  micrositeDocumentationUrl := "api/",
  micrositeGithubOwner := "typelevel",
  micrositeExtraMdFiles := Map(
    file("CONTRIBUTING.md") -> ExtraMdFileConfig(
      "contributing.md",
      "home",
       Map("title" -> "Contributing", "section" -> "contributing", "position" -> "50")
    ),
    file("README.md") -> ExtraMdFileConfig(
      "index.md",
      "home",
      Map("title" -> "Home", "section" -> "home", "position" -> "0")
    )
  ),
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
  scalacOptions in Tut ~= (_.filterNot(Set("-Ywarn-unused-import", "-Ywarn-dead-code"))),
  git.remoteRepo := "git@github.com:typelevel/cats.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg",
  includeFilter in Jekyll := (includeFilter in makeSite).value
)

lazy val docs = project
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(moduleName := "cats-docs")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(docSettings)
  .settings(commonJvmSettings)
  .dependsOn(coreJVM, freeJVM, kernelLawsJVM, lawsJVM, testkitJVM)

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
  .aggregate(macrosJVM, kernelJVM, kernelLawsJVM, coreJVM, lawsJVM, freeJVM, testkitJVM, testsJVM, alleycatsCoreJVM, alleycatsLawsJVM, alleycatsTestsJVM, jvm, docs, bench)
  .dependsOn(macrosJVM, kernelJVM, kernelLawsJVM, coreJVM, lawsJVM, freeJVM, testkitJVM, testsJVM % "test-internal -> test", alleycatsCoreJVM, alleycatsLawsJVM, alleycatsTestsJVM % "test-internal -> test", jvm, bench % "compile-internal;test-internal -> test")

lazy val catsJS = project.in(file(".catsJS"))
  .settings(moduleName := "cats")
  .settings(noPublishSettings)
  .settings(catsSettings)
  .settings(commonJsSettings)
  .aggregate(macrosJS, kernelJS, kernelLawsJS, coreJS, lawsJS, freeJS, testkitJS, testsJS, alleycatsCoreJS, alleycatsLawsJS, alleycatsTestsJS, js)
  .dependsOn(macrosJS, kernelJS, kernelLawsJS, coreJS, lawsJS, freeJS, testkitJS, testsJS % "test-internal -> test", alleycatsCoreJS, alleycatsLawsJS, alleycatsTestsJS % "test-internal -> test", js)
  .enablePlugins(ScalaJSPlugin)


lazy val macros = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "cats-macros", name := "Cats macros")
  .settings(catsSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(coverageEnabled := false)
  .settings(scalacOptions := scalacOptions.value.filter(_ != "-Xfatal-warnings"))

lazy val macrosJVM = macros.jvm
lazy val macrosJS = macros.js

val binaryCompatibleVersion = "0.8.0"

val binaryCompatibleExceptions = {
  import com.typesafe.tools.mima.core._
  import com.typesafe.tools.mima.core.ProblemFilters._
  Seq( // todo: remove these once we release 1.0.0-RC1
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple5"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple3"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple5"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple2"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple2"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple20"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple18"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple13"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple17"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple14"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple6"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple13"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple11"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple17"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple16"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple8"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple12"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple21"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple16"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple20"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple14"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple22"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple11"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple15"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple9"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple10"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple1"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple4"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple19"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple4"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple21"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple4"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple15"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple7"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple7"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple1"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple20"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple7"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple1"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple13"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple18"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple8"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple7"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple22"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple18"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple2"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple4"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple17"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple13"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple16"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple1"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple12"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple10"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple12"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple22"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple19"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple16"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple11"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple5"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple15"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple21"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple14"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple19"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple10"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple3"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple9"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple9"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple11"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple3"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple6"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple6"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple3"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple20"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple14"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple7"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple6"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple13"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple9"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple12"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple20"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple6"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple17"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple9"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple14"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple18"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple15"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple21"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple17"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple1"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple12"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple16"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple22"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple3"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple10"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple19"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple11"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple18"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple21"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemigroupForTuple4"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple15"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple8"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple5"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple22"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple5"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdGroupForTuple8"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple8"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdSemilatticeForTuple2"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBandForTuple2"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple19"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdMonoidForTuple10"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple19"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple1"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple10"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple11"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple20"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple14"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple4"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple13"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple2"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple5"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple8"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple3"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple15"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple21"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple16"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple22"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple6"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple10"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple19"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple18"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple9"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple13"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple12"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple1"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple4"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple8"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple11"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple7"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple12"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple2"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple5"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple14"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple20"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple15"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple21"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple18"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple17"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple6"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple9"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple22"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple3"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdCommutativeGroupForTuple17"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple16"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.TupleInstances.catsKernelStdBoundedSemilatticeForTuple7"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple18"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple8"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple17"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple2"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple11"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple5"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple14"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple20"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple13"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple7"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple22"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple16"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple1"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple19"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple4"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple10"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple3"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple12"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple6"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple9"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple15"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple21"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple9"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple14"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple20"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple17"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple11"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple8"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple2"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple5"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple19"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple10"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple13"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple16"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple22"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple7"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple1"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple21"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple4"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple18"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple12"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple15"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple3"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple6"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple18"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple8"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple17"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple2"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple11"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple5"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple14"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple20"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple13"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple7"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple22"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple16"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple1"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple19"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple4"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple10"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple3"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple12"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple6"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple9"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple15"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdCommutativeMonoidForTuple21"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple9"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple14"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple20"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple17"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple11"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple8"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple2"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple5"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple19"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple10"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple13"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple16"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple22"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple7"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple1"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple21"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple4"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple18"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple12"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple15"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple3"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances2.catsKernelStdCommutativeSemigroupForTuple6"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.QueueInstances.*"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.QueueInstances1.*"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.QueueInstances2.*"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.DurationInstances.*"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.MapInstances.catsKernelStdEqForMap"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.MapInstances.catsKernelStdMonoidForMap"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.MapInstances.catsKernelStdHashForMap"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.OptionInstances0.catsKernelStdEqForOption"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple12"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple7"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple4"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple13"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple11"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple1"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple7"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple16"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple22"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple1"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple14"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple4"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple20"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple19"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple10"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple17"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple18"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple3"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple9"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple6"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple3"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple12"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple22"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple6"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple19"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple10"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple9"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple21"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple15"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple13"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple16"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple20"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple14"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple5"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple2"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple8"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple17"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple5"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple15"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple21"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple11"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdPartialOrderForTuple2"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple8"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.TupleInstances.catsKernelStdEqForTuple18"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple9"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple16"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple22"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple3"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple6"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple21"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple18"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple12"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple15"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple8"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple2"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple5"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple20"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple14"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple17"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple4"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple11"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple7"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple1"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple10"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple19"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple13"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.StreamInstances1.catsKernelStdHashForStream"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.ListInstances1.catsKernelStdHashForList"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.SetInstances.catsKernelStdPartialOrderForSet"),
    exclude[UpdateForwarderBodyProblem]("cats.kernel.instances.SetInstances.catsKernelStdSemilatticeForSet"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.SetInstances.catsKernelStdHashForSet"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.VectorInstances1.catsKernelStdHashForVector"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BitSetInstances.catsKernelStdPartialOrderForBitSet"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.BitSetInstances.cats$kernel$instances$BitSetInstances$_setter_$catsKernelStdOrderForBitSet_="),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.BitSetInstances.catsKernelStdOrderForBitSet"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple9"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple16"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple22"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple3"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple6"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple21"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple18"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple12"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple15"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple8"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple2"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple5"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple20"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple14"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple17"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple4"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple11"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple7"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple1"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple10"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple19"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.TupleInstances1.catsKernelStdHashForTuple13"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.EitherInstances1.catsStdEqForEither"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.EitherInstances.catsStdOrderForEither"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.EitherInstances.catsDataMonoidForEither"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.EitherInstances0.catsDataSemigroupForEither"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.EitherInstances0.catsStdHashForEither"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.EitherInstances0.catsStdPartialOrderForEither"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.OptionInstances1.catsKernelStdHashForOption"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.FunctionInstances0.catsKernelHashForFunction0"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.OptionInstances0.catsKernelStdPartialOrderForOption"),
    exclude[ReversedMissingMethodProblem]("cats.kernel.instances.EitherInstances0.catsStdHashForEither"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionInstances1.catsKernelStdPartialOrderForOption"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.all.package.catsKernelStdPartialOrderForBitSet"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.bitSet.package.catsKernelStdPartialOrderForBitSet"),
    exclude[MissingTypesProblem]("cats.kernel.instances.OptionInstances1"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.OptionInstances1.catsKernelStdHashForOption"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.QueueInstances.*"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.QueueInstances1.*"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.QueueInstances2.*"),
    exclude[InheritedNewAbstractMethodProblem]("cats.kernel.instances.DurationInstances.*"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Eq*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Eq*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Eq*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.PartialOrder*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.PartialOrder*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.PartialOrder*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.PartialOrder*.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Order*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Order*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Order*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Order*.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Order*.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Hash*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Hash*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.Hash*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BooleanOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BooleanOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BooleanOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BooleanOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BooleanOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SymbolOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SymbolOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SymbolOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SymbolOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SymbolOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionEq.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionEq.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionEq.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigDecimalOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigDecimalOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigDecimalOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigDecimalOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigDecimalOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.CharOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.CharOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.CharOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.CharOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.CharOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.LongOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.LongOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.LongOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.LongOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.LongOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorEq.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorEq.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorEq.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamPartialOrder*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamPartialOrder*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamPartialOrder*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamPartialOrder*.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigIntOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigIntOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigIntOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigIntOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BigIntOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.DoubleOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.DoubleOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.DoubleOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.DoubleOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.DoubleOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BitSetPartialOrder*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BitSetPartialOrder*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BitSetPartialOrder*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.BitSetPartialOrder*.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.UnitOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.UnitOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.UnitOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.UnitOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.UnitOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionPartialOrder*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionPartialOrder*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionPartialOrder*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionPartialOrder*.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorPartialOrder*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorPartialOrder*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorPartialOrder*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorPartialOrder*.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListPartialOrder*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListPartialOrder*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListPartialOrder*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListPartialOrder*.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SetPartialOrder*.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SetPartialOrder*.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SetPartialOrder*.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.SetPartialOrder*.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.MapEq.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.MapEq.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.MapEq.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.VectorOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.FloatOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.FloatOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.FloatOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.FloatOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.FloatOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamEq.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamEq.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StreamEq.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.IntOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.IntOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.IntOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.IntOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.IntOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListEq.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListEq.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ListEq.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ByteOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ByteOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ByteOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ByteOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ByteOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ShortOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ShortOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ShortOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ShortOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.ShortOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StringOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StringOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StringOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StringOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.StringOrder.whenEqual"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionOrder.or"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionOrder.and"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionOrder.on"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionOrder.reverse"),
    exclude[DirectMissingMethodProblem]("cats.kernel.instances.OptionOrder.whenEqual")
  )
}

lazy val kernel = crossProject.crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(moduleName := "cats-kernel", name := "Cats kernel")
  .settings(kernelSettings)
  .settings(publishSettings)
  .settings(scoverageSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(KernelBoiler.gen).taskValue)
  .settings(includeGeneratedSrc)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ Seq(
    mimaPreviousArtifacts := {
      if (scalaVersion.value startsWith "2.12")
        Set()
      else
        Set("org.typelevel" %% "cats-kernel" % binaryCompatibleVersion)
    },
    mimaBinaryIssueFilters ++= binaryCompatibleExceptions
  ))

lazy val kernelJVM = kernel.jvm
lazy val kernelJS = kernel.js

lazy val kernelLaws = crossProject.crossType(CrossType.Pure)
  .in(file("kernel-laws"))
  .settings(moduleName := "cats-kernel-laws", name := "Cats kernel laws")
  .settings(kernelSettings)
  .settings(publishSettings)
  .settings(scoverageSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(coverageEnabled := false)
  .dependsOn(kernel)

lazy val kernelLawsJVM = kernelLaws.jvm
lazy val kernelLawsJS = kernelLaws.js

lazy val core = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, kernel)
  .settings(moduleName := "cats-core", name := "Cats core")
  .settings(catsSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(Boilerplate.gen).taskValue)
  .settings(includeGeneratedSrc)
  .configureCross(disableScoverage210Jvm)
  .configureCross(disableScoverage210Js)
  .settings(libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % "test")
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val laws = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, kernel, core, kernelLaws)
  .settings(moduleName := "cats-laws", name := "Cats laws")
  .settings(catsSettings)
  .settings(disciplineDependencies)
  .configureCross(disableScoverage210Jvm)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(coverageEnabled := false)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

lazy val free = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free", name := "Cats Free")
  .settings(catsSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)

lazy val freeJVM = free.jvm
lazy val freeJS = free.js

lazy val tests = crossProject.crossType(CrossType.Pure)
  .dependsOn(testkit % "test")
  .settings(moduleName := "cats-tests")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js


lazy val testkit = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, laws)
  .settings(moduleName := "cats-testkit")
  .settings(catsSettings)
  .settings(disciplineDependencies)
  .settings(
    libraryDependencies += "org.scalatest" %%% "scalatest" % scalaTestVersion)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)

lazy val testkitJVM = testkit.jvm
lazy val testkitJS = testkit.js

lazy val alleycatsCore = crossProject.crossType(CrossType.Pure)
  .in(file("alleycats-core"))
  .dependsOn(core)
  .settings(moduleName := "alleycats-core", name := "Alleycats core")
  .settings(libraryDependencies ++= Seq(
    "org.typelevel" %% "export-hook" % "1.2.0"
  ))
  .settings(catsSettings)
  .settings(publishSettings)
  .settings(scoverageSettings)
  .settings(includeGeneratedSrc)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .settings(scalacOptions ~= {_.filterNot("-Ywarn-unused-import" == _)}) //export-hook triggers unused import


lazy val alleycatsCoreJVM = alleycatsCore.jvm
lazy val alleycatsCoreJS = alleycatsCore.js

lazy val alleycatsLaws = crossProject.crossType(CrossType.Pure)
  .in(file("alleycats-laws"))
  .dependsOn(alleycatsCore, laws)
  .settings(moduleName := "alleycats-laws", name := "Alleycats laws")
  .settings(catsSettings)
  .settings(publishSettings)
  .settings(scoverageSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(coverageEnabled := false)
  .dependsOn(alleycatsCore)

lazy val alleycatsLawsJVM = alleycatsLaws.jvm
lazy val alleycatsLawsJS = alleycatsLaws.js

lazy val alleycatsTests = crossProject.crossType(CrossType.Pure)
  .in(file("alleycats-tests"))
  .dependsOn(alleycatsLaws, testkit % "test")
  .settings(moduleName := "alleycats-tests")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)

lazy val alleycatsTestsJVM = alleycatsTests.jvm
lazy val alleycatsTestsJS = alleycatsTests.js


// bench is currently JVM-only

lazy val bench = project.dependsOn(macrosJVM, coreJVM, freeJVM, lawsJVM)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .settings(coverageEnabled := false)
  .settings(libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.15"))
  .enablePlugins(JmhPlugin)

// cats-js is JS-only
lazy val js = project
  .dependsOn(macrosJS, coreJS, testsJS % "test-internal -> test")
  .settings(moduleName := "cats-js")
  .settings(catsSettings)
  .settings(commonJsSettings)
  .configure(disableScoverage210Js)
  .enablePlugins(ScalaJSPlugin)


// cats-jvm is JVM-only
lazy val jvm = project
  .dependsOn(macrosJVM, coreJVM, testsJVM % "test-internal -> test")
  .settings(moduleName := "cats-jvm")
  .settings(catsSettings)
  .settings(commonJvmSettings)

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
        <id>LukaJCB</id>
        <name>LukaJCB</name>
        <url>https://github.com/LukaJCB/</url>
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
  publish := {},
  publishLocal := {},
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
