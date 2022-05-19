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

ThisBuild / tlFatalWarnings := false
ThisBuild / tlFatalWarningsInCi := false

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
  )
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
  .settings(testingDependencies)
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(KernelBoiler.gen).taskValue)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings, cats1BincompatSettings)
  .nativeSettings(commonNativeSettings)

lazy val kernelLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("kernel-laws"))
  .dependsOn(kernel)
  .settings(moduleName := "cats-kernel-laws", name := "Cats kernel laws")
  .settings(disciplineDependencies)
  .settings(testingDependencies)
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
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)
  .settings(algebraSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kernel)
  .settings(moduleName := "cats-core", name := "Cats core")
  .settings(macroSettings, simulacrumSettings)
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.gen).taskValue)
  .settings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test,
    Compile / doc / scalacOptions ~= { _.filterNot(_.startsWith("-W")) } // weird bug
  )
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings, cats1BincompatSettings)
  .nativeSettings(commonNativeSettings)

lazy val laws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kernel, core, kernelLaws)
  .settings(moduleName := "cats-laws", name := "Cats laws")
  .settings(disciplineDependencies)
  .settings(testingDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val free = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free", name := "Cats Free")
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings, cats1BincompatSettings)
  .nativeSettings(commonNativeSettings)

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(testkit % Test)
  .enablePlugins(NoPublishPlugin)
  .settings(moduleName := "cats-tests")
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
  .settings(disciplineDependencies)
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val alleycatsCore = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("alleycats-core"))
  .dependsOn(core)
  .settings(moduleName := "alleycats-core", name := "Alleycats core")
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .nativeSettings(commonNativeSettings)

lazy val alleycatsLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("alleycats-laws"))
  .dependsOn(alleycatsCore, laws)
  .settings(moduleName := "alleycats-laws", name := "Alleycats laws")
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

// bench is currently JVM-only

lazy val bench = project
  .dependsOn(core.jvm, free.jvm, laws.jvm)
  .settings(moduleName := "cats-bench")
  .settings(commonJvmSettings)
  .settings(
    libraryDependencies ++= {
      if (scalaVersion.value.startsWith("2.12"))
        Seq(
          "org.scalaz" %% "scalaz-core" % "7.3.6",
          "org.spire-math" %% "chain" % "0.3.0",
          "co.fs2" %% "fs2-core" % "0.10.7"
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
      TypelevelProject.CatsEffect,
      "Mouse" -> url("https://typelevel.org/mouse"),
      TypelevelProject.Discipline
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion
    )
  )
  .dependsOn(core.jvm, free.jvm, laws.jvm)

ThisBuild / licenses := List(License.MIT)
ThisBuild / startYear := Some(2015)
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
