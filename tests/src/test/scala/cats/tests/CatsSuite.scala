package cats
package tests

import catalysts.Platform

import cats.std.AllInstances
import cats.syntax.{AllSyntax, EqOps}

import org.scalactic.anyvals.{PosZDouble, PosInt}
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}
import org.typelevel.discipline.scalatest.Discipline

trait TestSettings extends Configuration with Matchers {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = if (Platform.isJvm) PosInt(100) else PosInt(10),
      maxDiscardedFactor = if (Platform.isJvm) PosZDouble(5.0) else PosZDouble(50.0))

  lazy val slowCheckConfiguration: PropertyCheckConfiguration =
    if (Platform.isJvm) checkConfiguration
    else PropertyCheckConfig(maxSize = 1, minSuccessful = 1)
}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Discipline with TestSettings with AllInstances with AllSyntax with StrictCatsEquality {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  // disable Eq syntax (by making `eqSyntax` not implicit), since it collides
  // with scalactic's equality
  override def eqSyntax[A: Eq](a: A): EqOps[A] = new EqOps[A](a)
}

trait SlowCatsSuite extends CatsSuite {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    slowCheckConfiguration
}
