package cats
package tests

import cats.data.{IndexedStateT, StateT}
import org.scalacheck.Gen

class MonadTest extends CatsSuite {
  implicit val testInstance: Monad[StateT[Id, Int, ?]] = IndexedStateT.catsDataMonadForIndexedStateT[Id, Int]

  val increment: StateT[Id, Int, Unit] = StateT.modify(_ + 1)
  val incrementAndGet: StateT[Id, Int, Int] = increment >> StateT.get

  test("whileM_") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, _) = increment.whileM_(StateT.inspect(i => !(i >= max))).run(0)
      result should ===(Math.max(0, max))
    }
  }

  test("whileM") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, aggregation) = incrementAndGet.whileM[Vector](StateT.inspect(i => !(i >= max))).run(0)
      result should ===(Math.max(0, max))
      aggregation should === ( if(max > 0) (1 to max).toVector else Vector.empty )
    }
  }

  test("untilM_") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, _) = increment.untilM_(StateT.inspect(_ >= max)).run(-1)
      result should ===(max)
    }
  }

  test("untilM") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, aggregation) = incrementAndGet.untilM[Vector](StateT.inspect(_ >= max)).run(-1)
      result should ===(max)
      aggregation should === ((0 to max).toVector)
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
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, _) = incrementAndGet.iterateWhile(_ < max).run(-1)
      result should ===(Math.max(0, max))
    }
  }

  test("iterateWhile stack safety") {
    val (result, _) = incrementAndGet.iterateWhile(_ < 50000).run(-1)
    result should ===(50000)
  }

  test("iterateUntil") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, _) = incrementAndGet.iterateUntil(_ == max).run(-1)
      result should ===(Math.max(0, max))
    }
  }

  test("iterateUntil stack safety") {
    val (result, _) = incrementAndGet.iterateUntil(_ == 50000).run(-1)
    result should ===(50000)
  }

}
