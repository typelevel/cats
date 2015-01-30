import sbt._
import sbt.Keys._

import com.typesafe.sbt.pgp.PgpKeys._
 
import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._
 
object CatsBuild extends Build {

  lazy val core =
    Project("core", file("core")).settings(catsSettings: _*)

  lazy val laws =
    Project("laws", file("laws")).settings(catsSettings: _*).dependsOn(core/*, std*/)

  lazy val tests =
    Project("tests", file("tests")).settings(noPublishSettings: _*).dependsOn(core, laws)

  lazy val examples =
    Project("examples", file("examples")).settings(noPublishSettings: _*).dependsOn(core)

  lazy val aggregate =
    Project("aggregate", file(".")).settings(noPublishSettings: _*).aggregate(core, laws, tests).dependsOn(core, laws, tests, examples)

  lazy val catsSettings = Defaults.defaultSettings ++ releaseSettings ++ Seq(
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

  lazy val noPublishSettings = catsSettings ++ Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
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
}
