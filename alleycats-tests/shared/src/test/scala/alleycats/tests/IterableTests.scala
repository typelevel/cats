package alleycats.tests

import alleycats.std.all._
import cats.{Eval, Foldable}
import cats.instances.all._
import cats.laws.discipline.FoldableTests

class IterableTests extends AlleycatsSuite {

  test("foldLeft sum == sum") {
    val it = Iterable(1, 2, 3)
    Foldable[Iterable].foldLeft(it, 0) {
      case (b, a) => a + b
    } shouldEqual (it.sum)
  }

  test("foldRight early termination") {
    Foldable[Iterable]
      .foldRight(Iterable(1, 2, 3), Eval.now("KO")) {
        case (2, _) => Eval.now("OK")
        case (a, b) => b
      }
      .value shouldEqual (Eval.now("OK").value)
  }

  checkAll("Foldable[Iterable]", FoldableTests[Iterable].foldable[Int, Int])
}
