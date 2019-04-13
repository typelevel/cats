package alleycats
package tests

import alleycats.std.MapInstances
import cats._
import cats.instances.AllInstances
import cats.syntax.{AllSyntax, EqOps}
import cats.tests.StrictCatsEquality
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

import scala.util.{Failure, Success, Try}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Alleycats tests. Derived from Cats.
 */
trait AlleycatsSuite
    extends FunSuite
    with Matchers
    with GeneratorDrivenPropertyChecks
    with Discipline
    with TestSettings
    with AllInstances
    with AllSyntax
    with TestInstances
    with StrictCatsEquality
    with MapInstances {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  // disable Eq syntax (by making `catsSyntaxEq` not implicit), since it collides
  // with scalactic's equality
  override def catsSyntaxEq[A: Eq](a: A): EqOps[A] = new EqOps[A](a)
}

sealed trait TestInstances {
  // To be replaced by https://github.com/rickynils/scalacheck/pull/170
  implicit def arbitraryTry[A: Arbitrary]: Arbitrary[Try[A]] =
    Arbitrary(Gen.oneOf(arbitrary[A].map(Success(_)), arbitrary[Throwable].map(Failure(_))))
}
