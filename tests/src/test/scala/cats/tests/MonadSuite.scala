package cats.tests

import cats.{Id, Monad}
import cats.data.{IndexedStateT, StateT}
import cats.instances.all._
import cats.syntax.all._
import org.scalacheck.Gen

class MonadSuite extends CatsSuite {
  implicit val testInstance: Monad[StateT[Id, Int, *]] = IndexedStateT.catsDataMonadForIndexedStateT[Id, Int]

  val smallPosInt = Gen.choose(1, 5000)

  val increment: StateT[Id, Int, Unit] = StateT.modify(_ + 1)
  val incrementAndGet: StateT[Id, Int, Int] = increment *> StateT.get

  test("whileM_") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, _) = increment.whileM_(StateT.inspect(i => !(i >= max))).run(0)
      result should ===(Math.max(0, max))
    }
  }

  test("whileM") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, aggregation) = incrementAndGet.whileM[Vector](StateT.inspect(i => !(i >= max))).run(0)
      result should ===(Math.max(0, max))
      aggregation should ===(if (max > 0) (1 to max).toVector else Vector.empty)
    }
  }

  test("untilM_") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, _) = increment.untilM_(StateT.inspect(_ >= max)).run(-1)
      result should ===(max)
    }
  }

  test("untilM") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, aggregation) = incrementAndGet.untilM[Vector](StateT.inspect(_ >= max)).run(-1)
      result should ===(max)
      aggregation should ===((0 to max).toVector)
    }
  }

  test("whileM_ stack safety") {
    val (result, _) = increment.whileM_(StateT.inspect(i => !(i >= 50000))).run(0)
    result should ===(50000)
  }

  test("whileM stack safety") {
    val (result, _) = incrementAndGet.whileM[Vector](StateT.inspect(i => !(i >= 50000))).run(0)
    result should ===(50000)
  }

  test("iterateWhile") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, _) = incrementAndGet.iterateWhile(_ < max).run(-1)
      result should ===(Math.max(0, max))
    }
  }

  test("iterateWhile stack safety") {
    val (result, _) = incrementAndGet.iterateWhile(_ < 50000).run(-1)
    result should ===(50000)
  }

  test("iterateUntil") {
    forAll(smallPosInt) { (max: Int) =>
      val (result, _) = incrementAndGet.iterateUntil(_ == max).run(-1)
      result should ===(Math.max(0, max))
    }
  }

  test("iterateUntil stack safety") {
    val (result, _) = incrementAndGet.iterateUntil(_ == 50000).run(-1)
    result should ===(50000)
  }

  test("iterateWhileM") {
    forAll(smallPosInt) { (max: Int) =>
      val (n, sum) = 0.iterateWhileM(s => incrementAndGet.map(_ + s))(_ < max).run(0)
      sum should ===(n * (n + 1) / 2)
    }
  }

  test("iterateWhileM is stack safe") {
    val (n, sum) = 0.iterateWhileM(s => incrementAndGet.map(_ + s))(_ < 50000000).run(0)
    sum should ===(n * (n + 1) / 2)
  }

  test("iterateUntilM") {
    forAll(smallPosInt) { (max: Int) =>
      val (n, sum) = 0.iterateUntilM(s => incrementAndGet.map(_ + s))(_ > max).run(0)
      sum should ===(n * (n + 1) / 2)
    }
  }

  test("iterateUntilM is stack safe") {
    val (n, sum) = 0.iterateUntilM(s => incrementAndGet.map(_ + s))(_ > 50000000).run(0)
    sum should ===(n * (n + 1) / 2)
  }

}
