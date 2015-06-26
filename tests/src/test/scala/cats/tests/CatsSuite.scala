package cats
package tests

import cats.std.AllInstances
import cats.syntax.AllSyntax
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

import scala.util.{Failure, Success, Try}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite extends FunSuite with Discipline with AllInstances with AllSyntax with TestInstances

sealed trait TestInstances {
  // To be replaced by https://github.com/rickynils/scalacheck/pull/170
  implicit def arbitraryTry[A : Arbitrary]: Arbitrary[Try[A]] =
    Arbitrary {
      for {
        success <- arbitrary[Boolean]
        t       <- if (success) arbitrary[A].map(Success(_))
                   else Gen.const(Failure(new Throwable {}))
      } yield t
    }
}
