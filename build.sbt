ThisBuild / tlBaseVersion := "2.8"

ThisBuild / scalafixDependencies += "org.typelevel" %% "simulacrum-scalafix" % "0.5.3"

val scalaCheckVersion = "1.15.4"

val disciplineVersion = "1.4.0"

val disciplineMunitVersion = "1.0.9"

val munitVersion = "0.7.29"

val kindProjectorVersion = "0.13.2"

val PrimaryJava = JavaSpec.temurin("8")
val LTSJava = JavaSpec.temurin("17")
val GraalVM11 = JavaSpec.graalvm("11")

ThisBuild / githubWorkflowJavaVersions := Seq(PrimaryJava, LTSJava, GraalVM11)

val Scala212 = "2.12.15"
val Scala213 = "2.13.8"
val Scala3 = "3.0.2"

ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, Scala3)
ThisBuild / scalaVersion := Scala212

ThisBuild / tlFatalWarnings := {
  githubIsWorkflowBuild.value && !tlIsScala3.value
}

ThisBuild / tlCiReleaseBranches := Seq("main")

ThisBuild / githubWorkflowBuildMatrixExclusions ++= {
  for {
    scala <- githubWorkflowScalaVersions.value.filterNot(_ == (ThisBuild / scalaVersion).value)
    java <- githubWorkflowJavaVersions.value.tail
  } yield MatrixExclude(Map("scala" -> scala, "java" -> java.render))
}

ThisBuild / githubWorkflowBuildMatrixExclusions +=
  MatrixExclude(Map("project" -> "rootNative", "scala" -> Scala3))
// Dotty is not yet supported by Scala Native

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    "scalafix",
    "Scalafix",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.Run(List("cd scalafix", "sbt test"), name = Some("Scalafix tests"))
    ),
    javas = List(PrimaryJava),
    scalas = List((ThisBuild / scalaVersion).value)
  ),
  WorkflowJob(
    "linting",
    "Linting",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.Sbt(List("scalafmtSbtCheck", "+scalafmtCheckAll"), name = Some("Check formatting"))
    ),
    javas = List(PrimaryJava),
    scalas = List((ThisBuild / scalaVersion).value)
  )
)

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String) = {
  def extraDirs(suffix: String) =
    List(CrossType.Pure, CrossType.Full)
      .flatMap(_.sharedSrcDir(srcBaseDir, srcName).toList.map(f => file(f.getPath + suffix)))
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y))     => if (y >= 13) extraDirs("-2.13+") else Nil
    case Some((0 | 3, _)) => extraDirs("-2.13+")
    case _                => Nil
  }
}

lazy val commonSettings = Seq(
  Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
  Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
  Compile / packageSrc / mappings ++= {
    val base = (Compile / sourceManaged).value
    (Compile / managedSources).value.map { file =>
      file -> file.relativeTo(base).get.getPath
    }
  },
  scalacOptions ~= { _.filterNot(x => x.startsWith("-Wunused:")) },
  Compile / doc / scalacOptions := (Compile / doc / scalacOptions).value.filter(_ != "-Xfatal-warnings")
)

lazy val macroSettings = Seq(
  libraryDependencies ++= {
    if (tlIsScala3.value)
      Nil
    else
      Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)
  }
)

lazy val cats1BincompatSettings = Seq(
  tlMimaPreviousVersions ++= {
    if (scalaVersion.value.startsWith("2.12")) Set("1.0.1", "1.1.0", "1.2.0", "1.3.1", "1.4.0", "1.5.0", "1.6.1")
    else Set.empty
  }
)

lazy val simulacrumSettings = Seq(
  libraryDependencies ++= (if (tlIsScala3.value) Nil else Seq(compilerPlugin(scalafixSemanticdb))),
  scalacOptions ++= (
    if (tlIsScala3.value) Nil
    else Seq(s"-P:semanticdb:targetroot:${baseDirectory.value}/target/.semanticdb", "-Yrangepos")
  ),
  libraryDependencies += "org.typelevel" %% "simulacrum-scalafix-annotations" % "0.5.4"
)

ThisBuild / tlVersionIntroduced := Map("3" -> "2.6.1")

lazy val commonJvmSettings = Seq(
  Test / fork := true,
  Test / javaOptions := Seq("-Xmx3G"),
  doctestGenTests := { if (tlIsScala3.value) Nil else doctestGenTests.value }
)

lazy val commonJsSettings = Seq(
  doctestGenTests := Seq.empty,
  tlVersionIntroduced ++= List("2.12", "2.13").map(_ -> "2.1.0").toMap
)

lazy val commonNativeSettings = Seq(
  doctestGenTests := Seq.empty,
  // Currently scala-native does not support Dotty
  crossScalaVersions := { (ThisBuild / crossScalaVersions).value.filterNot(Scala3 == _) },
  tlVersionIntroduced ++= List("2.12", "2.13").map(_ -> "2.4.0").toMap
)

lazy val disciplineDependencies = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "discipline-core" % disciplineVersion
  )
)

lazy val testingDependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % munitVersion % Test,
    "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion % Test
  )
)

lazy val root = tlCrossRootProject
  .aggregate(
    kernel,
    kernelLaws,
    algebra,
    algebraLaws,
    core,
    laws,
    free,
    testkit,
    tests,
    alleycatsCore,
    alleycatsLaws,
    alleycatsTests,
    unidocs,
    bench,
    binCompatTest
  )

lazy val kernel = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(moduleName := "cats-kernel", name := "Cats kernel")
  .settings(commonSettings, testingDependencies)
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(KernelBoiler.gen).taskValue)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings, cats1BincompatSettings)
  .nativeSettings(commonNativeSettings)

lazy val kernelLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("kernel-laws"))
  .dependsOn(kernel)
  .settings(moduleName := "cats-kernel-laws", name := "Cats kernel laws")
  .settings(commonSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .settings(Test / scalacOptions := (Test / scalacOptions).value.filter(_ != "-Xfatal-warnings"))
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val algebraSettings = Seq[Setting[_]](
  tlMimaPreviousVersions += "2.2.3",
  tlVersionIntroduced := List("2.12", "2.13", "3").map(_ -> "2.7.0").toMap
)

lazy val algebra = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("algebra-core"))
  .dependsOn(kernel)
  .settings(moduleName := "algebra", name := "Cats algebra")
  .settings(commonSettings)
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(AlgebraBoilerplate.gen).taskValue)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)
  .settings(
    algebraSettings,
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test,
    testingDependencies
  )

lazy val algebraLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("algebra-laws"))
  .dependsOn(kernelLaws, algebra)
  .settings(moduleName := "algebra-laws", name := "Cats algebra laws")
  .settings(commonSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .settings(
    Test / scalacOptions := (Test / scalacOptions).value.filter(_ != "-Xfatal-warnings")
  )
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)
  .settings(algebraSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kernel)
  .settings(moduleName := "cats-core", name := "Cats core")
  .settings(commonSettings, macroSettings, simulacrumSettings)
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.gen).taskValue)
  .settings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test
  )
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings, cats1BincompatSettings)
  .nativeSettings(commonNativeSettings)

lazy val laws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kernel, core, kernelLaws)
  .settings(moduleName := "cats-laws", name := "Cats laws")
  .settings(commonSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val free = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free", name := "Cats Free")
  .settings(commonSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings, cats1BincompatSettings)
  .nativeSettings(commonNativeSettings)

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(testkit % Test)
  .enablePlugins(NoPublishPlugin)
  .settings(moduleName := "cats-tests")
  .settings(commonSettings)
  .settings(Test / scalacOptions := (Test / scalacOptions).value.filter(_ != "-Xfatal-warnings"))
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val testkit = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, laws)
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoKeys := Seq[BuildInfoKey](scalaVersion), buildInfoPackage := "cats.tests")
  .settings(moduleName := "cats-testkit")
  .settings(commonSettings)
  .settings(tlFatalWarnings := false)
  .settings(disciplineDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val alleycatsCore = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("alleycats-core"))
  .dependsOn(core)
  .settings(moduleName := "alleycats-core", name := "Alleycats core")
  .settings(commonSettings)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val alleycatsLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("alleycats-laws"))
  .dependsOn(alleycatsCore, laws)
  .settings(moduleName := "alleycats-laws", name := "Alleycats laws")
  .settings(commonSettings)
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val alleycatsTests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("alleycats-tests"))
  .dependsOn(alleycatsLaws, tests % "test-internal -> test")
  .enablePlugins(NoPublishPlugin)
  .settings(moduleName := "alleycats-tests")
  .settings(commonSettings)
  .settings(Test / scalacOptions := (Test / scalacOptions).value.filter(_ != "-Xfatal-warnings"))
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val unidocs = project
  .enablePlugins(TypelevelUnidocPlugin)
  .settings(
    name := "cats-docs",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(kernel.jvm,
                                                             core.jvm,
                                                             free.jvm,
                                                             algebra.jvm,
                                                             alleycatsCore.jvm
    ),
    scalacOptions ~= { _.filterNot(_.startsWith("-W")) }, // weird nsc bug
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-groups", "-diagrams")
  )
  .settings(commonSettings)

// bench is currently JVM-only

lazy val bench = project
  .dependsOn(core.jvm, free.jvm, laws.jvm)
  .settings(moduleName := "cats-bench")
  .settings(commonSettings)
  .settings(commonJvmSettings)
  .settings(
    libraryDependencies ++= {
      if (scalaVersion.value.startsWith("2.12"))
        Seq(
          "org.scalaz" %% "scalaz-core" % "7.2.23",
          "org.spire-math" %% "chain" % "0.3.0",
          "co.fs2" %% "fs2-core" % "0.10.4"
        )
      else Nil
    },
    evictionErrorLevel := Level.Warn
  )
  .enablePlugins(NoPublishPlugin, JmhPlugin)

lazy val binCompatTest = project
  .enablePlugins(NoPublishPlugin)
  .settings(
    useCoursier := false, // workaround so we can use an old version in compile
    libraryDependencies += {
      val oldV = if (tlIsScala3.value) "2.6.1" else "2.0.0"
      "org.typelevel" %%% "cats-core" % oldV % Provided
    }
  )
  .settings(testingDependencies)
  .dependsOn(core.jvm % Test)

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .settings(
    tlFatalWarnings := false,
    laikaConfig ~= { _.withRawContent },
    tlSiteApiUrl := Some(url("https://typelevel.org/cats/api/")),
    tlSiteRelatedProjects := Seq(
      "Cats Effect" -> url("https://typelevel.org/cats-effect"),
      "mouse" -> url("https://typelevel.org/mouse"),
      "Discipline" -> url("https://github.com/typelevel/discipline")
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion
    )
  )
  .dependsOn(core.jvm, free.jvm, laws.jvm)

ThisBuild / organization := "org.typelevel"
ThisBuild / organizationName := "Typelevel"
ThisBuild / organizationHomepage := Some(url("https://typelevel.org"))
ThisBuild / licenses += License.MIT
ThisBuild / developers ++= List(
  tlGitHubDev("ceedubs", "Cody Allen"),
  tlGitHubDev("rossabaker", "Ross Baker"),
  tlGitHubDev("johnynek", "P. Oscar Boykin"),
  tlGitHubDev("travisbrown", "Travis Brown"),
  tlGitHubDev("adelbertc", "Adelbert Chang"),
  tlGitHubDev("peterneyens", "Peter Neyens"),
  tlGitHubDev("tpolecat", "Rob Norris"),
  tlGitHubDev("non", "Erik Osheim"),
  tlGitHubDev("LukaJCB", "LukaJCB"),
  tlGitHubDev("mpilquist", "Michael Pilquist"),
  tlGitHubDev("milessabin", "Miles Sabin"),
  tlGitHubDev("djspiewak", "Daniel Spiewak"),
  tlGitHubDev("fthomas", "Frank Thomas"),
  tlGitHubDev("julien-truffaut", "Julien Truffaut"),
  tlGitHubDev("kailuowang", "Kailuo Wang")
)
