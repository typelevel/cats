package cats
package tests

import cats.functor.Invariant
import cats.laws.discipline.{InvariantTests, InvariantMonoidalTests, SerializableTests}
import cats.laws.discipline.eq._

import org.scalacheck.{Arbitrary, Gen}

class AlgebraInvariantTests extends CatsSuite {

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    val empty = 1
    def combine(x: Int, y: Int): Int = x * y
  }

  val maxInt: Monoid[Int] = new Monoid[Int] {
    val empty = Int.MinValue
    def combine(x: Int, y: Int): Int = if (x > y) x else y
  }

  val genMonoidInt: Gen[Monoid[Int]] =
    Gen.oneOf(implicitly[Monoid[Int]], intMultiplication, maxInt)

  implicit val arbMonoidInt: Arbitrary[Monoid[Int]] =
    Arbitrary(genMonoidInt)

  implicit val arbSemigoupInt: Arbitrary[Semigroup[Int]] =
    Arbitrary(genMonoidInt)

  checkAll("Invariant[Semigroup]", InvariantTests[Semigroup].invariant[Int, Int, Int])
  checkAll("Invariant[Semigroup]", SerializableTests.serializable(Invariant[Semigroup]))

  checkAll("Invariant[Monoid]", InvariantTests[Monoid].invariant[Int, Int, Int])
  checkAll("Invariant[Monoid]", SerializableTests.serializable(Invariant[Monoid]))

  checkAll("InvariantMonoidal[Semigroup]", InvariantMonoidalTests[Semigroup].invariantMonoidal[Int, Int, Int])
  checkAll("InvariantMonoidal[Semigroup]", SerializableTests.serializable(InvariantMonoidal[Semigroup]))

  checkAll("InvariantMonoidal[Monoid]", InvariantMonoidalTests[Monoid].invariantMonoidal[Int, Int, Int])
  checkAll("InvariantMonoidal[Monoid]", SerializableTests.serializable(InvariantMonoidal[Monoid]))
}
