package cats.tests

import cats.{Eval, Id, Monad}
import cats.catsInstancesForId
import cats.data.{IndexedStateT, StateT}
import cats.syntax.apply._
import cats.syntax.monad._
import org.scalacheck.{Arbitrary, Gen}
import cats.syntax.eq._
import org.scalacheck.Prop._

class MonadSuite extends CatsSuite {
  implicit val testInstance: Monad[StateT[Id, Int, *]] = IndexedStateT.catsDataMonadForIndexedStateT[Id, Int]

  val smallPosInt = Gen.choose(1, 5000)

  val increment: StateT[Id, Int, Unit] = StateT.modify(_ + 1)
  val incrementAndGet: StateT[Id, Int, Int] = increment *> StateT.get

  test("whileM_") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, _) = increment.whileM_(StateT.inspect(i => !(i >= max))).run(0)
      assert(result === (Math.max(0, max)))
    }
  }

  test("whileM") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, aggregation) = incrementAndGet.whileM[Vector](StateT.inspect(i => !(i >= max))).run(0)
      assert(result === (Math.max(0, max)))
      assert(aggregation === (if (max > 0) (1 to max).toVector else Vector.empty))
    }
  }

  test("untilM_") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, _) = increment.untilM_(StateT.inspect(_ >= max)).run(-1)
      assert(result === max)
    }
  }

  test("untilM") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, aggregation) = incrementAndGet.untilM[Vector](StateT.inspect(_ >= max)).run(-1)
      assert(result === max)
      assert(aggregation === ((0 to max).toVector))
    }
  }

  test("whileM_ stack safety") {
    val (result, _) = increment.whileM_(StateT.inspect(i => !(i >= 50000))).run(0)
    assert(result === 50000)
  }

  test("whileM stack safety") {
    val (result, _) = incrementAndGet.whileM[Vector](StateT.inspect(i => !(i >= 50000))).run(0)
    assert(result === 50000)
  }

  test("iterateWhile") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, _) = incrementAndGet.iterateWhile(_ < max).run(-1)
      assert(result === (Math.max(0, max)))
    }
  }

  test("iterateWhile stack safety") {
    val (result, _) = incrementAndGet.iterateWhile(_ < 50000).run(-1)
    assert(result === 50000)
  }

  test("iterateUntil") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, _) = incrementAndGet.iterateUntil(_ == max).run(-1)
      assert(result === (Math.max(0, max)))
    }
  }

  test("iterateUntil stack safety") {
    val (result, _) = incrementAndGet.iterateUntil(_ == 50000).run(-1)
    assert(result === 50000)
  }

  test("iterateWhileM") {
    forAll(smallPosInt) { (max: Int) =>
      val (n, sum) = 0.iterateWhileM(s => incrementAndGet.map(_ + s))(_ < max).run(0)
      assert(sum === (n * (n + 1) / 2))
    }
  }

  test("iterateWhileM is stack safe") {
    val (n, sum) = 0.iterateWhileM(s => incrementAndGet.map(_ + s))(_ < 50000000).run(0)
    assert(sum === (n * (n + 1) / 2))
  }

  test("iterateUntilM") {
    forAll(smallPosInt) { (max: Int) =>
      val (n, sum) = 0.iterateUntilM(s => incrementAndGet.map(_ + s))(_ > max).run(0)
      assert(sum === (n * (n + 1) / 2))
    }
  }

  test("iterateUntilM is stack safe") {
    val (n, sum) = 0.iterateUntilM(s => incrementAndGet.map(_ + s))(_ > 50000000).run(0)
    assert(sum === (n * (n + 1) / 2))
  }

  test("ifElseM") {
    val tupleGen =
      for {
        b <- Arbitrary.arbitrary[Boolean]
        i <- smallPosInt
      } yield b -> i

    forAll(Gen.listOf(tupleGen)) { (xs: List[(Boolean, Int)]) =>
      val expected = xs.collectFirst { case (true, x) => x }.getOrElse(-1)
      val branches = xs.map { case (b, x) => (Eval.now(b), Eval.now(x)) }
      assert(Monad[Eval].ifElseM(branches: _*)(Eval.now(-1)).value === expected)
    }
  }

  test("ifElseM resorts to default") {
    forAll(Gen.listOf(smallPosInt)) { (xs: List[Int]) =>
      val branches = xs.map(x => (Eval.now(false), Eval.now(x)))
      assert(Monad[Eval].ifElseM(branches: _*)(Eval.now(-1)).value === -1)
    }
  }

  // this example is in the doctest and I'm not sure how useful it is to have here as well except as a way
  // to play with
  test("ifElseM example") {
    val actual =
      Monad[Eval].ifElseM(Eval.later(false) -> Eval.later(1), Eval.later(true) -> Eval.later(2))(Eval.later(5))
    assert(actual.value === 2)
  }

}
