package cats.tests

import cats.Eval
import cats.FlatMap
import cats.syntax.eq._

class FlatMapSuite extends CatsSuite {

  test("ifElseM") {
    val actual = FlatMap.ifElseM(Eval.later(false) -> Eval.later(1), Eval.later(true) -> Eval.later(2))(Eval.later(5))
    assert(actual.value === 2)
  }

}
