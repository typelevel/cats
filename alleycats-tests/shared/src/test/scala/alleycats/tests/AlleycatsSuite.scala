package alleycats.tests

import alleycats.std.MapInstances
import cats._
import cats.instances.all._
import cats.tests.StrictCatsEquality
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.matchers.should.Matchers
import scala.util.{Failure, Success, Try}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Alleycats tests. Derived from Cats.
 */
trait AlleycatsSuite
    extends AnyFunSuiteLike
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with FunSuiteDiscipline
    with TestSettings
    with TestInstances
    with StrictCatsEquality
    with MapInstances {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  implicit def EqIterable[A: Eq]: Eq[Iterable[A]] = Eq.by(_.toList)
}

sealed trait TestInstances {
  // To be replaced by https://github.com/rickynils/scalacheck/pull/170
  implicit def arbitraryTry[A: Arbitrary]: Arbitrary[Try[A]] =
    Arbitrary(Gen.oneOf(arbitrary[A].map(Success(_)), arbitrary[Throwable].map(Failure(_))))
}
