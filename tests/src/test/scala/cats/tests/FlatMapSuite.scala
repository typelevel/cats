package cats.tests

import cats.{Eval, FlatMap}
import cats.syntax.eq._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._

class FlatMapSuite extends CatsSuite {

  private[this] val smallPosInt: Gen[Int] = Gen.choose(1, 5000)

  private[this] val tupleGen =
    for {
      b <- Arbitrary.arbitrary[Boolean]
      i <- smallPosInt
    } yield b -> i

  private[this] val smallPosIntList: Gen[List[Int]] = Gen.listOf(smallPosInt)

  private[this] val tupleList: Gen[List[(Boolean, Int)]] = Gen.listOf(tupleGen)

  test("ifElseM") {
    forAll(tupleList) { xs: List[(Boolean, Int)] =>
      val expected = xs.collectFirst { case (true, x) => x }.getOrElse(-1)
      val branches = xs.map { case (b, x) => (Eval.now(b), Eval.now(x)) }
      assert(FlatMap.ifElseM(branches: _*)(Eval.now(-1)).value === expected)
    }
  }

  test("ifElseM resort to default") {
    forAll(smallPosIntList) { xs: List[Int] =>
      val branches = xs.map(x => (Eval.now(false), Eval.now(x)))
      assert(FlatMap.ifElseM(branches: _*)(Eval.now(-1)).value === -1)
    }
  }

  // this example is in the doctest and I'm not sure how useful it is to have here as well except as a way
  // to play with
  test("ifElseM example") {
    val actual = FlatMap.ifElseM(Eval.later(false) -> Eval.later(1), Eval.later(true) -> Eval.later(2))(Eval.later(5))
    assert(actual.value === 2)
  }

}
