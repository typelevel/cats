package cats.tests

import cats.{Invariant, InvariantSemigroupal}
import cats.kernel.Monoid
import cats.syntax.eq._

class MonoidSuite extends CatsSuite {
  {
    Invariant[Monoid]
    InvariantSemigroupal[Monoid]
  }

  test("companion object syntax") {
    assert(Monoid.empty[Int] === 0)
    assert(Monoid.isEmpty(1) === false)
    assert(Monoid.isEmpty(0) === true)
  }
}

object MonoidSuite {
  def summonInstance(): Unit = {
    Invariant[Monoid]
    ()
  }

}
