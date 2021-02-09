import microsites._
import ReleaseTransformations._
import sbt.io.Using
import com.jsuereth.sbtpgp.PgpKeys

import scala.xml.transform.{RewriteRule, RuleTransformer}
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val publishSignedIfRelevant = taskKey[Unit]("Runs publishSigned but only if scalaVersion in crossScalaVersions")
Global / publishSignedIfRelevant := PgpKeys.publishSigned.value

lazy val publishLocalSignedIfRelevant =
  taskKey[Unit]("Runs publishLocalSigned but only if scalaVersion in crossScalaVersions")
Global / publishLocalSignedIfRelevant := PgpKeys.publishLocalSigned.value

organization in ThisBuild := "org.typelevel"
scalafixDependencies in ThisBuild += "org.typelevel" %% "simulacrum-scalafix" % "0.5.3"

val scalaCheckVersion = "1.15.2"

val disciplineVersion = "1.1.3"

val disciplineScalatestVersion = "2.0.1"
val disciplineMunitVersion = "1.0.5"

val kindProjectorVersion = "0.11.3"

val PrimaryOS = "ubuntu-latest"
ThisBuild / githubWorkflowOSes := Seq(PrimaryOS)

val PrimaryJava = "adopt@1.8"
val LTSJava = "adopt@1.11"
val LatestJava = "adopt@1.15"
val GraalVM8 = "graalvm-ce-java8@20.2.0"

ThisBuild / githubWorkflowJavaVersions := Seq(PrimaryJava, LTSJava, LatestJava, GraalVM8)

val Scala212 = "2.12.13"
val Scala213 = "2.13.4"
val DottyOld = "3.0.0-M2"
val DottyNew = "3.0.0-M3"

ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, DottyOld, DottyNew)
ThisBuild / scalaVersion := Scala213

ThisBuild / githubWorkflowPublishTargetBranches := Seq() // disable publication for now

ThisBuild / githubWorkflowBuildMatrixAdditions +=
  "platform" -> List("jvm", "js", "native")

ThisBuild / githubWorkflowBuildMatrixExclusions ++=
  githubWorkflowJavaVersions.value.filterNot(Set(PrimaryJava)).flatMap { java =>
    Seq(MatrixExclude(Map("platform" -> "js", "java" -> java)),
        MatrixExclude(Map("platform" -> "native", "java" -> java))
    )
  }

ThisBuild / githubWorkflowBuildMatrixExclusions ++= Seq(DottyOld, DottyNew).map { dottyVersion =>
  MatrixExclude(Map("platform" -> "native", "scala" -> dottyVersion))
} // Dotty is not yet supported by Scala Native

// we don't need this since we aren't publishing
ThisBuild / githubWorkflowArtifactUpload := false

ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)

val JvmCond = s"matrix.platform == 'jvm'"
val JsCond = s"matrix.platform == 'js'"
val NativeCond = s"matrix.platform == 'native'"

val Scala2Cond = s"(matrix.scala != '$DottyOld' && matrix.scala != '$DottyNew')"
val Scala3Cond = s"(matrix.scala == '$DottyOld' || matrix.scala == '$DottyNew')"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("validateAllJS"), name = Some("Validate JavaScript"), cond = Some(JsCond)),
  WorkflowStep.Sbt(List("validateAllNative"), name = Some("Validate Scala Native"), cond = Some(NativeCond)),
  WorkflowStep.Sbt(List("buildJVM", "bench/test"),
                   name = Some("Validate JVM (scala 2)"),
                   cond = Some(JvmCond + " && " + Scala2Cond)
  ),
  WorkflowStep.Sbt(List("buildJVM", "bench/test"),
                   name = Some("Validate JVM (scala 3)"),
                   cond = Some(JvmCond + " && " + Scala3Cond)
  ),
  WorkflowStep.Sbt(
    List("clean", "validateBC"), // cleaning here to avoid issues with codecov
    name = Some("Binary compatibility ${{ matrix.scala }}"),
    cond = Some(JvmCond + " && " + Scala2Cond)
  )
)

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    "scalafix",
    "Scalafix",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.Run(List("cd scalafix", "sbt test"), name = Some("Scalafix tests"))
    ),
    javas = List(PrimaryJava),
    scalas = crossScalaVersions.value.toList
  ),
  WorkflowJob(
    "linting",
    "Linting",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.Sbt(List("fmtCheck"), name = Some("Check formatting"), cond = Some(Scala2Cond))
    ),
    javas = List(PrimaryJava),
    scalas = crossScalaVersions.value.toList
  ),
  WorkflowJob(
    "microsite",
    "Microsite",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.Use(UseRef.Public("ruby", "setup-ruby", "v1"),
                       params = Map("ruby-version" -> "2.7"),
                       name = Some("Setup Ruby")
      ),
      WorkflowStep.Run(List("gem install jekyll -v 4.0.0"), name = Some("Setup Jekyll")),
      WorkflowStep.Sbt(List("docs/makeMicrosite"), name = Some("Build the microsite"))
    ),
    javas = List(PrimaryJava),
    scalas = List(Scala212)
  )
)

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String) = {
  def extraDirs(suffix: String) =
    List(CrossType.Pure, CrossType.Full)
      .flatMap(_.sharedSrcDir(srcBaseDir, srcName).toList.map(f => file(f.getPath + suffix)))
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y))     => extraDirs("-2.x") ++ (if (y >= 13) extraDirs("-2.13+") else Nil)
    case Some((0 | 3, _)) => extraDirs("-2.13+") ++ extraDirs("-3.x")
    case _                => Nil
  }
}

ThisBuild / mimaFailOnNoPrevious := false

def doctestGenTestsDottyCompat(isDotty: Boolean, genTests: Seq[File]): Seq[File] =
  if (isDotty) Nil else genTests

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions(scalaVersion.value, isDotty.value),
  Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
  Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
  resolvers ++= Seq(Resolver.sonatypeRepo("releases"), Resolver.sonatypeRepo("snapshots")),
  parallelExecution in Test := false,
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  Compile / doc / sources := {
    val old = (Compile / doc / sources).value
    if (isDotty.value)
      Seq()
    else
      old
  }
) ++ warnUnusedImport

def macroDependencies(scalaVersion: String) =
  if (scalaVersion.startsWith("2")) Seq("org.scala-lang" % "scala-reflect" % scalaVersion % Provided) else Nil

lazy val catsSettings = Seq(
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  libraryDependencies ++= (
    if (isDotty.value) Nil
    else
      Seq(
        compilerPlugin(("org.typelevel" %% "kind-projector" % kindProjectorVersion).cross(CrossVersion.full))
      )
  ) ++ macroDependencies(scalaVersion.value)
) ++ commonSettings ++ publishSettings ++ simulacrumSettings

lazy val simulacrumSettings = Seq(
  libraryDependencies ++= (if (isDotty.value) Nil else Seq(compilerPlugin(scalafixSemanticdb))),
  scalacOptions ++= (
    if (isDotty.value) Nil else Seq(s"-P:semanticdb:targetroot:${baseDirectory.value}/target/.semanticdb", "-Yrangepos")
  ),
  libraryDependencies += "org.typelevel" %% "simulacrum-scalafix-annotations" % "0.5.3"
)

lazy val tagName = Def.setting {
  s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val commonJsSettings = Seq(
  publishConfiguration := publishConfiguration.value.withOverwrite(true), // needed since we double-publish on release
  scalacOptions ++= {
    if (isDotty.value) Seq()
    else {
      val tv = tagName.value
      val tagOrHash =
        if (isSnapshot.value) sys.process.Process("git rev-parse HEAD").lineStream_!.head
        else tv
      val a = (baseDirectory in LocalRootProject).value.toURI.toString
      val g = "https://raw.githubusercontent.com/typelevel/cats/" + tagOrHash
      Seq(s"-P:scalajs:mapSourceURI:$a->$g/")
    }
  },
  scalaJSStage in Global := FullOptStage,
  scalaJSStage in Test := FastOptStage,
  parallelExecution := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // batch mode decreases the amount of memory needed to compile Scala.js code
  scalaJSLinkerConfig := scalaJSLinkerConfig.value.withBatchMode(githubIsWorkflowBuild.value),
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  // currently sbt-doctest doesn't work in JS builds
  // https://github.com/tkawachi/sbt-doctest/issues/52
  doctestGenTests := Seq.empty
)

lazy val commonNativeSettings = Seq(
  publishConfiguration := publishConfiguration.value.withOverwrite(true), // needed since we double-publish on release
  // currently sbt-doctest doesn't work in Native/JS builds
  // https://github.com/tkawachi/sbt-doctest/issues/52
  doctestGenTests := Seq.empty,
  // Currently scala-native does not support Dotty
  crossScalaVersions := { crossScalaVersions.value.filterNot(Seq(DottyOld, DottyNew).contains) }
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += {
    val flag = if (githubIsWorkflowBuild.value) "-oCI" else "-oDF"
    Tests.Argument(TestFrameworks.ScalaTest, flag)
  },
  Test / fork := true,
  Test / javaOptions := Seq("-Xmx3G")
)

lazy val includeGeneratedSrc: Setting[_] = {
  mappings in (Compile, packageSrc) ++= {
    val base = (sourceManaged in Compile).value
    (managedSources in Compile).value.map { file =>
      file -> file.relativeTo(base).get.getPath
    }
  }
}

lazy val disciplineDependencies = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "discipline-core" % disciplineVersion
  )
)

lazy val testingDependencies = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion % Test
  )
)

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

lazy val docSettings = Seq(
  micrositeName := "Cats",
  micrositeDescription := "Lightweight, modular, and extensible library for functional programming",
  micrositeAuthor := "Cats contributors",
  micrositeFooterText := Some(
    """
      |<p>© 2020 <a href="https://github.com/typelevel/cats#maintainers">The Cats Maintainers</a></p>
      |<p style="font-size: 80%; margin-top: 10px">Website built with <a href="https://47deg.github.io/sbt-microsites/">sbt-microsites © 2020 47 Degrees</a></p>
      |""".stripMargin
  ),
  micrositeHighlightTheme := "atom-one-light",
  micrositeHomepage := "http://typelevel.org/cats/",
  micrositeBaseUrl := "cats",
  micrositeDocumentationUrl := "/cats/api/cats/index.html",
  micrositeDocumentationLabelDescription := "API Documentation",
  micrositeGithubOwner := "typelevel",
  micrositeExtraMdFilesOutput := resourceManaged.value / "main" / "jekyll",
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
  micrositeTheme := "pattern",
  micrositePalette := Map(
    "brand-primary" -> "#5B5988",
    "brand-secondary" -> "#292E53",
    "brand-tertiary" -> "#222749",
    "gray-dark" -> "#49494B",
    "gray" -> "#7B7B7E",
    "gray-light" -> "#E5E5E6",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"
  ),
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(kernel.jvm, core.jvm, free.jvm),
  docsMappingsAPIDir := "api",
  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docsMappingsAPIDir),
  ghpagesNoJekyll := false,
  fork in mdoc := true,
  fork in (ScalaUnidoc, unidoc) := true,
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-Xfatal-warnings",
    "-groups",
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  ) ++ (if (priorTo2_13(scalaVersion.value))
          Seq("-Yno-adapted-args")
        else
          Nil),
  scalacOptions ~= (_.filterNot(
    Set("-Ywarn-unused-import", "-Ywarn-unused:imports", "-Ywarn-dead-code", "-Xfatal-warnings")
  )),
  git.remoteRepo := "git@github.com:typelevel/cats.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg",
  includeFilter in Jekyll := (includeFilter in makeSite).value,
  mdocIn := baseDirectory.in(LocalRootProject).value / "docs" / "src" / "main" / "mdoc",
  mdocExtraArguments := Seq("--no-link-hygiene")
)

def mimaPrevious(moduleName: String, scalaVer: String, ver: String, includeCats1: Boolean = true): List[ModuleID] = {
  import sbtrelease.Version

  def semverBinCompatVersions(major: Int, minor: Int, patch: Int): List[(Int, Int, Int)] = {
    val majorVersions: List[Int] = List(major)
    val minorVersions: List[Int] =
      if (major >= 1) Range(0, minor).inclusive.toList
      else List(minor)
    def patchVersions(currentMinVersion: Int): List[Int] =
      if (minor == 0 && patch == 0) List.empty[Int]
      else if (currentMinVersion != minor) List(0)
      else Range(0, patch - 1).inclusive.toList

    for {
      maj <- majorVersions
      min <- minorVersions
      pat <- patchVersions(min)
    } yield (maj, min, pat)
  }

  val mimaVersions: List[String] = {
    Version(ver) match {
      case Some(Version(major, Seq(minor, patch), _)) =>
        semverBinCompatVersions(major.toInt, minor.toInt, patch.toInt)
          .map { case (maj, min, pat) => s"$maj.$min.$pat" }
      case _ =>
        List.empty[String]
    }
  }
  // Safety Net For Exclusions
  lazy val excludedVersions: List[String] = List()

  // Safety Net for Inclusions
  lazy val extraVersions: List[String] = List("1.0.1", "1.1.0", "1.2.0", "1.3.1", "1.4.0", "1.5.0", "1.6.1")

  (mimaVersions ++ (if (priorTo2_13(scalaVer) && includeCats1) extraVersions else Nil))
    .filterNot(excludedVersions.contains(_))
    .map(v => "org.typelevel" %% moduleName % v)
}

def mimaSettings(moduleName: String, includeCats1: Boolean = true) =
  Seq(
    mimaPreviousArtifacts := mimaPrevious(moduleName, scalaVersion.value, version.value, includeCats1).toSet,
    mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      //Only sealed abstract classes that provide implicit instances to companion objects are allowed here, since they don't affect usage outside of the file.
      Seq(
        exclude[DirectMissingMethodProblem]("cats.data.OptionTInstances2.catsDataTraverseForOptionT"),
        exclude[DirectMissingMethodProblem]("cats.data.KleisliInstances1.catsDataCommutativeArrowForKleisliId"),
        exclude[DirectMissingMethodProblem]("cats.data.OptionTInstances1.catsDataMonoidKForOptionT"),
        exclude[DirectMissingMethodProblem]("cats.data.OptionTInstances0.catsDataMonoidForOptionT"),
        exclude[DirectMissingMethodProblem]("cats.data.KleisliInstances0.catsDataMonadForKleisliId"),
        exclude[DirectMissingMethodProblem]("cats.data.KleisliInstances1.catsDataCommutativeArrowForKleisli"),
        exclude[DirectMissingMethodProblem]("cats.data.KleisliInstances4.catsDataCommutativeFlatMapForKleisli"),
        exclude[DirectMissingMethodProblem]("cats.data.IRWSTInstances1.catsDataStrongForIRWST"),
        exclude[DirectMissingMethodProblem]("cats.data.OptionTInstances1.catsDataMonadErrorMonadForOptionT"),
        exclude[DirectMissingMethodProblem]("cats.data.OptionTInstances1.catsDataMonadErrorForOptionT")
      ) ++
        //These things are Ops classes that shouldn't have the `value` exposed. These should have never been public because they don't
        //provide any value. Making them private because of issues like #2514 and #2613.
        Seq(
          exclude[DirectMissingMethodProblem]("cats.ApplicativeError#LiftFromOptionPartially.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.Const#OfPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.EitherT#CondPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.EitherT#FromEitherPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.EitherT#FromOptionPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.EitherT#LeftPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.EitherT#LeftTPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.EitherT#PurePartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.EitherT#RightPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#BothTPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#CondPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#FromEitherPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#FromIorPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#FromOptionPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#LeftPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#LeftTPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#PurePartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.IorT#RightPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.NonEmptyChainOps.value"),
          exclude[DirectMissingMethodProblem]("cats.data.OptionT#FromOptionPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.OptionT#PurePartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.data.Validated#CatchOnlyPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.free.Free#FreeInjectKPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.free.Free#FreeLiftInjectKPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.free.FreeT#FreeTLiftInjectKPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ApplicativeErrorIdOps.e"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ApplicativeErrorOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ApplicativeIdOps.a"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ApplicativeOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ApplyOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.BinestedIdOps.value"),
          exclude[DirectMissingMethodProblem]("cats.syntax.BitraverseOps.fab"),
          exclude[DirectMissingMethodProblem]("cats.syntax.DistributiveOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.EitherIdOps.obj"),
          exclude[DirectMissingMethodProblem]("cats.syntax.EitherIdOpsBinCompat0.value"),
          exclude[DirectMissingMethodProblem]("cats.syntax.EitherSyntax#CatchOnlyPartiallyApplied.dummy"),
          exclude[DirectMissingMethodProblem]("cats.syntax.EitherKOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.EitherObjectOps.either"),
          exclude[DirectMissingMethodProblem]("cats.syntax.EitherOps.eab"),
          exclude[DirectMissingMethodProblem]("cats.syntax.EitherOpsBinCompat0.value"),
          exclude[DirectMissingMethodProblem]("cats.syntax.FlatMapIdOps.a"),
          exclude[DirectMissingMethodProblem]("cats.syntax.FlatMapOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.FlatMapOptionOps.fopta"),
          exclude[DirectMissingMethodProblem]("cats.syntax.FlattenOps.ffa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.FoldableOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.GuardOps.condition"),
          exclude[DirectMissingMethodProblem]("cats.syntax.IfMOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.IndexOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.IorIdOps.a"),
          exclude[DirectMissingMethodProblem]("cats.syntax.LeftOps.left"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ListOps.la"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ListOpsBinCompat0.la"),
          exclude[DirectMissingMethodProblem]("cats.syntax.MonadErrorOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.MonadErrorRethrowOps.fea"),
          exclude[DirectMissingMethodProblem]("cats.syntax.MonadIdOps.a"),
          exclude[DirectMissingMethodProblem]("cats.syntax.MonadOps.fa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.MonoidOps.lhs"),
          exclude[DirectMissingMethodProblem]("cats.syntax.NestedBitraverseOps.fgagb"),
          exclude[DirectMissingMethodProblem]("cats.syntax.NestedFoldableOps.fga"),
          exclude[DirectMissingMethodProblem]("cats.syntax.NestedIdOps.value"),
          exclude[DirectMissingMethodProblem]("cats.syntax.NestedReducibleOps.fga"),
          exclude[DirectMissingMethodProblem]("cats.syntax.OptionIdOps.a"),
          exclude[DirectMissingMethodProblem]("cats.syntax.OptionOps.oa"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ParallelApOps.ma"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ParallelFlatSequenceOps.tmta"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ParallelFlatTraversableOps.ta"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ParallelSequence_Ops.tma"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ParallelSequenceOps.tma"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ParallelTraversable_Ops.ta"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ParallelTraversableOps.ta"),
          exclude[DirectMissingMethodProblem]("cats.syntax.RightOps.right"),
          exclude[DirectMissingMethodProblem]("cats.syntax.SeparateOps.fgab"),
          exclude[DirectMissingMethodProblem]("cats.syntax.SetOps.se"),
          exclude[DirectMissingMethodProblem]("cats.syntax.TabulateOps.f"),
          exclude[DirectMissingMethodProblem]("cats.syntax.TryOps.self"),
          exclude[DirectMissingMethodProblem]("cats.syntax.UniteOps.fga"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ValidatedExtension.self"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ValidatedIdOpsBinCompat0.a"),
          exclude[DirectMissingMethodProblem]("cats.syntax.ValidatedIdSyntax.a"),
          exclude[DirectMissingMethodProblem]("cats.syntax.VectorOps.va"),
          exclude[DirectMissingMethodProblem]("cats.syntax.WriterIdSyntax.a")
        ) ++ // Only compile-time abstractions (macros) allowed here
        Seq(
          exclude[IncompatibleMethTypeProblem]("cats.arrow.FunctionKMacros.lift"),
          exclude[MissingTypesProblem]("cats.arrow.FunctionKMacros$"),
          exclude[IncompatibleMethTypeProblem]("cats.arrow.FunctionKMacros#Lifter.this"),
          exclude[IncompatibleResultTypeProblem]("cats.arrow.FunctionKMacros#Lifter.c"),
          exclude[DirectMissingMethodProblem]("cats.arrow.FunctionKMacros.compatNewTypeName")
        ) ++ //package private classes no longer needed
        Seq(
          exclude[MissingClassProblem]("cats.kernel.compat.scalaVersionMoreSpecific$"),
          exclude[MissingClassProblem]("cats.kernel.compat.scalaVersionMoreSpecific"),
          exclude[MissingClassProblem](
            "cats.kernel.compat.scalaVersionMoreSpecific$suppressUnusedImportWarningForScalaVersionMoreSpecific"
          )
        ) ++ // Only narrowing of types allowed here
        Seq(
          exclude[IncompatibleSignatureProblem]("*")
        ) ++ // New issues found since mima 0.8.0 (#3596, #3641)
        Seq(
          exclude[NewMixinForwarderProblem]("cats.kernel.Band#mcI#sp.combineN"),
          exclude[NewMixinForwarderProblem]("cats.kernel.Band#mcD#sp.combineN"),
          exclude[NewMixinForwarderProblem]("cats.kernel.Band#mcJ#sp.combineN"),
          exclude[NewMixinForwarderProblem]("cats.kernel.Band.combineN"),
          exclude[NewMixinForwarderProblem]("cats.kernel.Band#mcF#sp.combineN"),
          exclude[NewMixinForwarderProblem]("cats.data.Tuple2KApply.product"),
          exclude[NewMixinForwarderProblem]("cats.InvariantInstances0.catsApplicativeForArrow")
        ) ++ // Additional methods in package-private traits
        Seq(
          exclude[ReversedMissingMethodProblem]("cats.data.NonEmptyCollection.grouped")
        )
    }
  )

lazy val docs = project
  .in(file("cats-docs"))
  .enablePlugins(MdocPlugin)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(moduleName := "cats-docs")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(docSettings)
  .settings(commonJvmSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "discipline-scalatest" % disciplineScalatestVersion
    ),
    scalacOptions in (ScalaUnidoc, unidoc) ~= { _.filter(_ != "-Xlint:-unused,_") }
  )
  .dependsOn(core.jvm, free.jvm, kernelLaws.jvm, laws.jvm)

lazy val cats = project
  .in(file("."))
  .settings(moduleName := "root")
  .settings(publishSettings) // these settings are needed to release all aggregated modules under this root module
  .settings(noPublishSettings) // this is to exclude the root module itself from being published.
  .aggregate(catsJVM, catsJS, catsNative)
  .dependsOn(catsJVM, catsJS, catsNative, tests.jvm % "test-internal -> test")

lazy val catsJVM = project
  .in(file(".catsJVM"))
  .settings(moduleName := "cats")
  .settings(noPublishSettings)
  .settings(catsSettings)
  .settings(commonJvmSettings)
  .aggregate(kernel.jvm,
             kernelLaws.jvm,
             core.jvm,
             laws.jvm,
             free.jvm,
             testkit.jvm,
             tests.jvm,
             alleycatsCore.jvm,
             alleycatsLaws.jvm,
             alleycatsTests.jvm,
             jvm
  )
  .dependsOn(
    kernel.jvm,
    kernelLaws.jvm,
    core.jvm,
    laws.jvm,
    free.jvm,
    testkit.jvm,
    tests.jvm % "test-internal -> test",
    alleycatsCore.jvm,
    alleycatsLaws.jvm,
    alleycatsTests.jvm % "test-internal -> test",
    jvm
  )

lazy val catsJS = project
  .in(file(".catsJS"))
  .settings(moduleName := "cats")
  .settings(noPublishSettings)
  .settings(catsSettings)
  .settings(commonJsSettings)
  .aggregate(kernel.js,
             kernelLaws.js,
             core.js,
             laws.js,
             free.js,
             testkit.js,
             tests.js,
             alleycatsCore.js,
             alleycatsLaws.js,
             alleycatsTests.js,
             js
  )
  .dependsOn(
    kernel.js,
    kernelLaws.js,
    core.js,
    laws.js,
    free.js,
    testkit.js,
    tests.js % "test-internal -> test",
    alleycatsCore.js,
    alleycatsLaws.js,
    alleycatsTests.js % "test-internal -> test",
    js
  )
  .enablePlugins(ScalaJSPlugin)

lazy val catsNative = project
  .in(file(".catsNative"))
  .settings(moduleName := "cats")
  .settings(noPublishSettings)
  .settings(catsSettings)
  .settings(commonNativeSettings)
  .aggregate(
    kernel.native,
    kernelLaws.native,
    core.native,
    laws.native,
    free.native,
    testkit.native,
    tests.native,
    alleycatsCore.native,
    alleycatsLaws.native,
    alleycatsTests.native,
    native
  )
  .dependsOn(
    kernel.native,
    kernelLaws.native,
    core.native,
    laws.native,
    free.native,
    testkit.native,
    tests.native % "test-internal -> test",
    alleycatsCore.native,
    alleycatsLaws.native,
    alleycatsTests.native % "test-internal -> test",
    native
  )
  .enablePlugins(ScalaNativePlugin)

lazy val kernel = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(moduleName := "cats-kernel", name := "Cats kernel")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(KernelBoiler.gen).taskValue)
  .settings(includeGeneratedSrc)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-kernel"))
  .nativeSettings(commonNativeSettings)
  .settings(testingDependencies)
  .settings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test
  )

lazy val kernelLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("kernel-laws"))
  .settings(moduleName := "cats-kernel-laws", name := "Cats kernel laws")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .settings(scalacOptions in Test := (scalacOptions in Test).value.filter(_ != "-Xfatal-warnings"))
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-kernel-laws", includeCats1 = false))
  .dependsOn(kernel)
  .nativeSettings(commonNativeSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kernel)
  .settings(moduleName := "cats-core", name := "Cats core")
  .settings(catsSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(Boilerplate.gen).taskValue)
  .settings(includeGeneratedSrc)
  .settings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test,
    doctestGenTests := doctestGenTestsDottyCompat(isDotty.value, doctestGenTests.value)
  )
  .settings(
    scalacOptions in Compile :=
      (scalacOptions in Compile).value.filter {
        case "-Xfatal-warnings" if isDotty.value => false
        case _                                   => true
      }
  )
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-core"))
  .settings(testingDependencies)
  .nativeSettings(commonNativeSettings)

lazy val laws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kernel, core, kernelLaws)
  .settings(moduleName := "cats-laws", name := "Cats laws")
  .settings(catsSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-laws", includeCats1 = false))
  .nativeSettings(commonNativeSettings)

lazy val free = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free", name := "Cats Free")
  .settings(catsSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-free"))
  .nativeSettings(commonNativeSettings)

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(testkit % Test)
  .settings(moduleName := "cats-tests")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .settings(scalacOptions in Test := (scalacOptions in Test).value.filter(_ != "-Xfatal-warnings"))
  .nativeSettings(commonNativeSettings)

lazy val testkit = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, laws)
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoKeys := Seq[BuildInfoKey](scalaVersion), buildInfoPackage := "cats.tests")
  .settings(moduleName := "cats-testkit")
  .settings(catsSettings)
  .settings(disciplineDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-testkit", includeCats1 = false))
  .settings(scalacOptions := scalacOptions.value.filter(_ != "-Xfatal-warnings"))
  .nativeSettings(commonNativeSettings)

lazy val alleycatsCore = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("alleycats-core"))
  .dependsOn(core)
  .settings(moduleName := "alleycats-core", name := "Alleycats core")
  .settings(catsSettings)
  .settings(publishSettings)
  .settings(includeGeneratedSrc)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("alleycats-core", includeCats1 = false))
  .nativeSettings(commonNativeSettings)

lazy val alleycatsLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("alleycats-laws"))
  .dependsOn(alleycatsCore, laws)
  .settings(moduleName := "alleycats-laws", name := "Alleycats laws")
  .settings(catsSettings)
  .settings(publishSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("alleycats-laws", includeCats1 = false))
  .nativeSettings(commonNativeSettings)

lazy val alleycatsTests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("alleycats-tests"))
  .dependsOn(alleycatsLaws, tests % "test-internal -> test")
  .settings(moduleName := "alleycats-tests")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .settings(scalacOptions in Test := (scalacOptions in Test).value.filter(_ != "-Xfatal-warnings"))
  .nativeSettings(commonNativeSettings)

// bench is currently JVM-only

lazy val bench = project
  .dependsOn(core.jvm, free.jvm, laws.jvm)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .settings(
    libraryDependencies ++= {
      if (priorTo2_13(scalaVersion.value))
        Seq(
          "org.scalaz" %% "scalaz-core" % "7.2.23",
          "org.spire-math" %% "chain" % "0.3.0",
          "co.fs2" %% "fs2-core" % "0.10.4"
        )
      else Nil
    }
  )
  .enablePlugins(JmhPlugin)

lazy val binCompatTest = project
  .settings(noPublishSettings)
  .settings(
    // workaround because coursier doesn't understand dependsOn(core.jvm % Test)
    // see https://github.com/typelevel/cats/pull/3079#discussion_r327181584
    // see https://github.com/typelevel/cats/pull/3026#discussion_r321984342
    useCoursier := false,
    addCompilerPlugin(("org.typelevel" %% "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)),
    libraryDependencies += mimaPrevious("cats-core", scalaVersion.value, version.value).last % Provided,
    scalacOptions ++= (if (priorTo2_13(scalaVersion.value)) Seq("-Ypartial-unification") else Nil)
  )
  .settings(testingDependencies)
  .dependsOn(core.jvm % Test)

// cats-js is JS-only
lazy val js = project
  .dependsOn(core.js, tests.js % "test-internal -> test")
  .settings(moduleName := "cats-js")
  .settings(catsSettings)
  .settings(commonJsSettings)
  .enablePlugins(ScalaJSPlugin)

// cats-native is Native-only
lazy val native = project
  .dependsOn(core.native, tests.native % "test-internal -> test")
  .settings(moduleName := "cats-native")
  .settings(catsSettings)
  .settings(commonNativeSettings)
  .enablePlugins(ScalaNativePlugin)

// cats-jvm is JVM-only
lazy val jvm = project
  .dependsOn(core.jvm, tests.jvm % "test-internal -> test")
  .settings(moduleName := "cats-jvm")
  .settings(catsSettings)
  .settings(commonJvmSettings)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/typelevel/cats")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/typelevel/cats"), "scm:git:git@github.com:typelevel/cats.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://typelevel.org/cats/api/")),
  pomExtra :=
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
) ++ sharedPublishSettings ++ sharedReleaseProcess

// Scalafmt
addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; compile:scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

// These aliases serialise the build for the benefit of Travis-CI.
addCommandAlias("buildKernelJVM", ";kernelJVM/test;kernelLawsJVM/test")
addCommandAlias("buildCoreJVM", ";coreJVM/test")
addCommandAlias("buildTestsJVM", ";lawsJVM/test;testkitJVM/test;testsJVM/test;jvm/test")
addCommandAlias("buildFreeJVM", ";freeJVM/test")
addCommandAlias("buildAlleycatsJVM", ";alleycatsCoreJVM/test;alleycatsLawsJVM/test;alleycatsTestsJVM/test")
addCommandAlias("buildJVM", ";buildKernelJVM;buildCoreJVM;buildTestsJVM;buildFreeJVM;buildAlleycatsJVM")
addCommandAlias("validateBC", ";binCompatTest/test;catsJVM/mimaReportBinaryIssues")
addCommandAlias("validateJVM", ";fmtCheck;buildJVM;bench/test;validateBC;makeMicrosite")
addCommandAlias("validateJS", ";testsJS/test;js/test")
addCommandAlias("validateKernelJS", "kernelLawsJS/test")
addCommandAlias("validateFreeJS", "freeJS/test")
addCommandAlias("validateAlleycatsJS", "alleycatsTestsJS/test")
addCommandAlias("validateAllJS", "all testsJS/test js/test kernelLawsJS/test freeJS/test alleycatsTestsJS/test")
addCommandAlias("validateNative", ";testsNative/test;native/test")
addCommandAlias("validateKernelNative", "kernelLawsNative/test")
addCommandAlias("validateFreeNative", "freeNative/test")
addCommandAlias("validateAlleycatsNative", "alleycatsTestsNative/test")
addCommandAlias("validateAllNative",
                "all testsNative/test native/test kernelLawsNative/test freeNative/test alleycatsTestsNative/test"
)
addCommandAlias(
  "validate",
  ";clean;validateJS;validateKernelJS;validateFreeJS;validateNative;validateKernelNative;validateFreeNative;validateJVM"
)

addCommandAlias("prePR", "fmt")

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
      (unmanagedSourceDirectories in sc).value.map { dir: File =>
        new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

def commonScalacOptions(scalaVersion: String, isDotty: Boolean) =
  Seq(
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-deprecation"
  ) ++ (if (priorTo2_13(scalaVersion))
          Seq(
            "-Yno-adapted-args",
            "-Ypartial-unification",
            "-Xfuture"
          )
        else
          Nil) ++ (if (isDotty)
                     Seq("-language:implicitConversions", "-Ykind-projector", "-Xignore-scala2-macros")
                   else
                     Seq(
                       "-language:existentials",
                       "-language:higherKinds",
                       "-language:implicitConversions",
                       "-Ywarn-dead-code",
                       "-Ywarn-numeric-widen",
                       "-Ywarn-value-discard",
                       "-Xlint:-unused,_"
                     ))

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

lazy val sharedPublishSettings = Seq(
  releaseTagName := tagName.value,
  releaseVcsSign := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots".at(nexus + "content/repositories/snapshots"))
    else
      Some("Releases".at(nexus + "service/local/staging/deploy/maven2"))
  }
)

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest, //temporarily only run test in current scala version because docs won't build in 2.13 yet
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("+publishSigned"),
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges
  )
)

lazy val warnUnusedImport = Seq(
  scalacOptions ++= (if (isDotty.value) Nil else Seq("-Ywarn-unused:imports")),
  scalacOptions in (Compile, console) ~= { _.filterNot(Set("-Ywarn-unused-import", "-Ywarn-unused:imports")) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)
