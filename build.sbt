import microsites._
import ReleaseTransformations._
import sbt.io.Using

import scala.xml.transform.{RewriteRule, RuleTransformer}
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageHighlighting := true
)

organization in ThisBuild := "org.typelevel"
scalafixDependencies in ThisBuild += "org.typelevel" %% "simulacrum-scalafix" % "0.5.0"

val isTravisBuild = settingKey[Boolean]("Flag indicating whether the current build is running under Travis")
val crossScalaVersionsFromTravis = settingKey[Seq[String]]("Scala versions set in .travis.yml as scala_version_XXX")
isTravisBuild in Global := sys.env.get("TRAVIS").isDefined

val scalaCheckVersion = "1.14.3"

val scalatestVersion = "3.2.0"
val scalatestplusScalaCheckVersion = "3.2.0.0"

val disciplineVersion = "1.0.3"

val disciplineScalatestVersion = "2.0.0"

val kindProjectorVersion = "0.11.0"

crossScalaVersionsFromTravis in Global := {
  val manifest = (baseDirectory in ThisBuild).value / ".travis.yml"
  import collection.JavaConverters._
  Using.fileInputStream(manifest) { fis =>
    new org.yaml.snakeyaml.Yaml().loadAs(fis, classOf[java.util.Map[_, _]]).asScala.toList.collect {
      case (k: String, v: String) if k.contains("scala_version_") => v
    }
  }
}

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String) = {
  def extraDirs(suffix: String) =
    List(CrossType.Pure, CrossType.Full)
      .flatMap(_.sharedSrcDir(srcBaseDir, srcName).toList.map(f => file(f.getPath + suffix)))
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y)) => extraDirs("-2.x") ++ (if (y >= 13) extraDirs("-2.13+") else Nil)
    case Some((0, _)) => extraDirs("-2.13+") ++ extraDirs("-3.x")
    case _            => Nil
  }
}

lazy val commonScalaVersionSettings = Seq(
  crossScalaVersions := (crossScalaVersionsFromTravis in Global).value,
  scalaVersion := crossScalaVersions.value.find(_.contains("2.12")).get
)

commonScalaVersionSettings

ThisBuild / mimaFailOnNoPrevious := false

def doctestGenTestsDottyCompat(isDotty: Boolean, genTests: Seq[File]): Seq[File] =
  if (isDotty) Nil else genTests

lazy val commonSettings = commonScalaVersionSettings ++ Seq(
  scalacOptions ++= commonScalacOptions(scalaVersion.value, isDotty.value),
  Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
  Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
  resolvers ++= Seq(Resolver.sonatypeRepo("releases"), Resolver.sonatypeRepo("snapshots")),
  parallelExecution in Test := false,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings")
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
) ++ commonSettings ++ publishSettings ++ scoverageSettings ++ simulacrumSettings

lazy val simulacrumSettings = Seq(
  libraryDependencies ++= (if (isDotty.value) Nil else Seq(compilerPlugin(scalafixSemanticdb))),
  scalacOptions ++= (
    if (isDotty.value) Nil else Seq(s"-P:semanticdb:targetroot:${baseDirectory.value}/target/.semanticdb", "-Yrangepos")
  ),
  libraryDependencies +=
    ("org.typelevel" %% "simulacrum-scalafix-annotations" % "0.5.0" % Provided).withDottyCompat(scalaVersion.value),
  pomPostProcess := { (node: xml.Node) =>
    new RuleTransformer(new RewriteRule {
      override def transform(node: xml.Node): Seq[xml.Node] =
        node match {
          case e: xml.Elem
              if e.label == "dependency" &&
                e.child.exists(child => child.label == "groupId" && child.text == "org.typelevel") &&
                e.child.exists(child => child.label == "artifactId" && child.text.startsWith("simulacrum")) =>
            Nil
          case _ => Seq(node)
        }
    }).transform(node).head
  }
)

lazy val tagName = Def.setting {
  s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val commonJsSettings = Seq(
  scalacOptions += {
    val tv = tagName.value
    val tagOrHash =
      if (isSnapshot.value) sys.process.Process("git rev-parse HEAD").lineStream_!.head
      else tv
    val a = (baseDirectory in LocalRootProject).value.toURI.toString
    val g = "https://raw.githubusercontent.com/typelevel/cats/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  scalaJSStage in Global := FullOptStage,
  parallelExecution := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // batch mode decreases the amount of memory needed to compile Scala.js code
  scalaJSLinkerConfig := scalaJSLinkerConfig.value.withBatchMode(isTravisBuild.value),
  // currently sbt-doctest doesn't work in JS builds
  // https://github.com/tkawachi/sbt-doctest/issues/52
  doctestGenTests := Seq.empty,
  coverageEnabled := false
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += {
    val flag = if ((isTravisBuild in Global).value) "-oCI" else "-oDF"
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
    "org.scalacheck" %%% "scalacheck" % scalaCheckVersion,
    "org.typelevel" %%% "discipline-core" % disciplineVersion
  ).map(_.withDottyCompat(scalaVersion.value))
)

lazy val testingDependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest-shouldmatchers" % scalatestVersion % Test,
    "org.scalatest" %%% "scalatest-funsuite" % scalatestVersion % Test,
    "org.scalatestplus" %%% "scalacheck-1-14" % scalatestplusScalaCheckVersion % Test
  ),
  libraryDependencies ++= Seq(
    ("org.typelevel" %%% "discipline-scalatest" % disciplineScalatestVersion % Test)
  ).map(
    _.exclude("org.scalatestplus", "scalacheck-1-14_2.13")
      .exclude("org.scalactic", "scalactic_2.13")
      .exclude("org.scalatest", "scalatest_2.13")
      .withDottyCompat(scalaVersion.value)
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
  micrositeCompilingDocsTool := WithTut,
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(kernel.jvm, core.jvm, free.jvm),
  docsMappingsAPIDir := "api",
  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docsMappingsAPIDir),
  ghpagesNoJekyll := false,
  fork in tut := true,
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
  scalacOptions in Tut ~= (_.filterNot(Set("-Ywarn-unused-import", "-Ywarn-unused:imports", "-Ywarn-dead-code"))),
  git.remoteRepo := "git@github.com:typelevel/cats.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg",
  includeFilter in Jekyll := (includeFilter in makeSite).value
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

    val versions = for {
      maj <- majorVersions
      min <- minorVersions
      pat <- patchVersions(min)
    } yield (maj, min, pat)
    versions.toList
  }

  val mimaVersions: List[String] = {
    Version(ver) match {
      case Some(Version(major, Seq(minor, patch), _)) =>
        semverBinCompatVersions(major.toInt, minor.toInt, patch.toInt)
          .map { case (maj, min, pat) => s"${maj}.${min}.${pat}" }
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
        )
    }
  )

lazy val docs = project
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
  .settings(moduleName := "root", crossScalaVersions := Nil)
  .settings(publishSettings) // these settings are needed to release all aggregated modules under this root module
  .settings(noPublishSettings) // this is to exclude the root module itself from being published.
  .aggregate(catsJVM, catsJS)
  .dependsOn(catsJVM, catsJS, tests.jvm % "test-internal -> test")

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
             jvm,
             docs
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

lazy val kernel = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(moduleName := "cats-kernel", name := "Cats kernel")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(scoverageSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(KernelBoiler.gen).taskValue)
  .settings(includeGeneratedSrc)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-kernel"))
  .settings(
    libraryDependencies += ("org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test)
      .withDottyCompat(scalaVersion.value)
  )

lazy val kernelLaws = crossProject(JSPlatform, JVMPlatform)
  .in(file("kernel-laws"))
  .settings(moduleName := "cats-kernel-laws", name := "Cats kernel laws")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(scoverageSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .settings(scalacOptions in Test := (scalacOptions in Test).value.filter(_ != "-Xfatal-warnings"))
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-kernel-laws", includeCats1 = false))
  .jsSettings(coverageEnabled := false)
  .dependsOn(kernel)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kernel)
  .settings(moduleName := "cats-core", name := "Cats core")
  .settings(catsSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(Boilerplate.gen).taskValue)
  .settings(includeGeneratedSrc)
  .settings(
    libraryDependencies += ("org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test)
      .withDottyCompat(scalaVersion.value),
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

lazy val laws = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kernel, core, kernelLaws)
  .settings(moduleName := "cats-laws", name := "Cats laws")
  .settings(catsSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-laws", includeCats1 = false))
  .jsSettings(coverageEnabled := false)

lazy val free = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free", name := "Cats Free")
  .settings(catsSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("cats-free"))

lazy val tests = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(testkit % Test)
  .settings(moduleName := "cats-tests")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .settings(scalacOptions in Test := (scalacOptions in Test).value.filter(_ != "-Xfatal-warnings"))

lazy val testkit = crossProject(JSPlatform, JVMPlatform)
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

lazy val alleycatsCore = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("alleycats-core"))
  .dependsOn(core)
  .settings(moduleName := "alleycats-core", name := "Alleycats core")
  .settings(catsSettings)
  .settings(publishSettings)
  .settings(scoverageSettings)
  .settings(includeGeneratedSrc)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("alleycats-core", includeCats1 = false))

lazy val alleycatsLaws = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("alleycats-laws"))
  .dependsOn(alleycatsCore, laws)
  .settings(moduleName := "alleycats-laws", name := "Alleycats laws")
  .settings(catsSettings)
  .settings(publishSettings)
  .settings(scoverageSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings ++ mimaSettings("alleycats-laws", includeCats1 = false))
  .jsSettings(coverageEnabled := false)

lazy val alleycatsTests = crossProject(JSPlatform, JVMPlatform)
  .in(file("alleycats-tests"))
  .dependsOn(alleycatsLaws, tests % "test-internal -> test")
  .settings(moduleName := "alleycats-tests")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .settings(scalacOptions in Test := (scalacOptions in Test).value.filter(_ != "-Xfatal-warnings"))

// bench is currently JVM-only

lazy val bench = project
  .dependsOn(core.jvm, free.jvm, laws.jvm)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .settings(coverageEnabled := false)
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
    commonScalaVersionSettings,
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
addCommandAlias("validateBC", ";binCompatTest/test;mimaReportBinaryIssues")
addCommandAlias("validateJVM", ";fmtCheck;buildJVM;bench/test;validateBC;makeMicrosite")
addCommandAlias("validateJS", ";catsJS/compile;testsJS/test;js/test")
addCommandAlias("validateKernelJS", "kernelLawsJS/test")
addCommandAlias("validateFreeJS", "freeJS/test") //separated due to memory constraint on travis
addCommandAlias("validateDotty", ";++0.24.0!;alleycatsLawsJVM/compile")
addCommandAlias("validate", ";clean;validateJS;validateKernelJS;validateFreeJS;validateJVM;validateDotty")

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

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)
