package alleycats.tests

import alleycats.std.MapInstances
import cats._
import cats.instances.all._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import scala.util.{Failure, Success, Try}
import org.scalacheck.Test.Parameters

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Alleycats tests. Derived from Cats.
 */
trait AlleycatsSuite extends munit.DisciplineSuite with TestSettings with TestInstances with MapInstances {
  implicit override def scalaCheckTestParameters: Parameters =
    checkConfiguration

  implicit def EqIterable[A: Eq]: Eq[Iterable[A]] = Eq.by(_.toList)
}

sealed trait TestInstances {
  // To be replaced by https://github.com/rickynils/scalacheck/pull/170
  implicit def arbitraryTry[A: Arbitrary]: Arbitrary[Try[A]] =
    Arbitrary(Gen.oneOf(arbitrary[A].map(Success(_)), arbitrary[Throwable].map(Failure(_))))
}
