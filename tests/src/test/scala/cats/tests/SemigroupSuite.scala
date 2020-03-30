package cats.tests

import cats.{Invariant, InvariantMonoidal, Semigroupal}
import cats.kernel.Semigroup
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SemigroupSuite extends AnyFunSuiteLike with Matchers with ScalaCheckDrivenPropertyChecks {
  {
    Invariant[Semigroup]
    Semigroupal[Semigroup]
    InvariantMonoidal[Semigroup]
  }

  {
    Invariant[Semigroup]
    Semigroupal[Semigroup]
    InvariantMonoidal[Semigroup]
  }

  test("Semigroup.instance creates a Semigroup from the given function") {
    val mult: (Int, Int) => Int = (a, b) => a * b
    val add: (Int, Int) => Int = (a, b) => a + b

    forAll { (a: Int, b: Int) =>
      Semigroup.instance(mult).combine(a, b) should ===(a * b)
      Semigroup.instance(add).combine(a, b) should ===(a + b)
    }
  }
}
