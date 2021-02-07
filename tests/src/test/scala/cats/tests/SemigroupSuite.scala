package cats.tests

import cats.{Invariant, InvariantMonoidal, Semigroupal}
import cats.kernel.Semigroup
import cats.syntax.eq._
import cats.kernel.laws.discipline.SemigroupTests
import org.scalacheck.Prop._

class SemigroupSuite extends CatsSuite {
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

  property("Semigroup.instance creates a Semigroup from the given function") {
    val mult: (Int, Int) => Int = (a, b) => a * b
    val add: (Int, Int) => Int = (a, b) => a + b

    forAll { (a: Int, b: Int) =>
      assert(Semigroup.instance(mult).combine(a, b) === (a * b))
      assert(Semigroup.instance(add).combine(a, b) === (a + b))
    }
  }

  {
    val S = Semigroup.first[Int]
    checkAll("Semigroup.first", SemigroupTests[Int](S).semigroup)
  }

  {
    val S = Semigroup.last[Int]
    checkAll("Semigroup.last", SemigroupTests[Int](S).semigroup)
  }
}
