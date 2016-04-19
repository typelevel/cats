package cats
package tests

import cats.data.State

class MonadStateTest extends CatsSuite {
  val testInstance = MonadState[State[Int, ?], Int]

  test("MonadState#modify identity does not modify the state") {
    forAll { (i: Int) =>
      val x = testInstance.modify(identity).runS(i).value
      x should === (i)
    }
  }

  test("MonadState#inspect identity does not modify the state") {
    forAll { (i: Int) =>
      val x = testInstance.inspect(identity).runA(i).value
      x should === (i)
    }
  }
}
