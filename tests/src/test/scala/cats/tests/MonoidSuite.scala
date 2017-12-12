package cats
package tests



class MonoidSuite extends CatsSuite {
  {
    Invariant[Monoid]
    InvariantSemigroupal[Monoid]
  }

  test("companion object syntax") {
    Monoid.empty[Int] should ===(0)
    Monoid.isEmpty(1) should ===(false)
    Monoid.isEmpty(0) should ===(true)
  }
}

object MonoidSuite {
  def summonInstance(): Unit = {
    Invariant[Monoid]
    InvariantSemigroupal[Monoid]
    ()
  }

}
