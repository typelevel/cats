package cats
package tests

import cats.std.AllInstances
import cats.syntax.AllSyntax
import org.scalatest.{ FunSuite, PropSpec, Matchers }
import org.scalatest.prop.PropertyChecks
import org.typelevel.discipline.scalatest.Discipline

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

import scala.util.{Failure, Success, Try}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite extends FunSuite with Matchers with Discipline with AllInstances with AllSyntax with TestInstances {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = Platform.minSuccessful,
      maxDiscardedFactor = Platform.maxDiscardedFactor)
  // disable scalatest's ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

trait CatsProps extends PropSpec with Matchers with PropertyChecks with TestInstances {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = Platform.minSuccessful,
      maxDiscardedFactor = Platform.maxDiscardedFactor)
  // disable scalatest's ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

sealed trait TestInstances {
  // To be replaced by https://github.com/rickynils/scalacheck/pull/170
  implicit def arbitraryTry[A: Arbitrary]: Arbitrary[Try[A]] =
    Arbitrary(Gen.oneOf(
      arbitrary[A].map(Success(_)),
      arbitrary[Throwable].map(Failure(_))))
}
