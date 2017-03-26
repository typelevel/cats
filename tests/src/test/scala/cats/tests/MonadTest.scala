package cats
package tests

import cats.data.{StateT}
import org.scalacheck.Gen

class MonadTest extends CatsSuite {
  val testInstance = StateT.catsDataMonadStateForStateT[Id, Int]
  import testInstance._

  test("whileM_") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, _) = whileM_(inspect(i => !(i >= max)))(modify(_ + 1)).run(0)
      result should ===(Math.max(0, max))
    }
  }

  test("whileM") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, aggregation) = whileM[Vector, Int](inspect(i => !(i >= max)))(flatMap(modify(_ + 1))(_ => get)).run(0)
      result should ===(Math.max(0, max))
      aggregation should === ( if(max > 0) (1 to max).toVector else Vector.empty )
    }
  }

  test("untilM_") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, _) = untilM_(modify(_ + 1))(inspect(_ >= max)).run(-1)
      result should ===(max)
    }
  }

  test("untilM") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, aggregation) = untilM[List, Int](flatMap(modify(_ + 1))(_ => get))(inspect(_ >= max)).run(-1)
      result should ===(max)
      aggregation should === ((0 to max).toList)
    }
  }

  test("whileM_ stack safety") {
    val (result, _) = whileM_(inspect(i => !(i >= 50000)))(modify(_ + 1)).run(0)
    result should ===(50000)
  }

  test("whileM stack safety") {
    val (result, _) = whileM[Vector, Unit](inspect(i => !(i >= 50000)))(modify(_ + 1)).run(0)
    result should ===(50000)
  }

  test("iterateWhile") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, _) = iterateWhile(flatMap(modify(_ + 1))(_ => get))(_ < max).run(-1)
      result should ===(Math.max(0, max))
    }
  }

  test("iterateWhile stack safety") {
    val (result, _) = iterateWhile(flatMap(modify(_ + 1))(_ => get))(_ < 50000).run(-1)
    result should ===(50000)
  }

  test("iterateUntil") {
    forAll(Gen.posNum[Int]) { (max: Int) =>
      val (result, _) = iterateUntil(flatMap(modify(_ + 1))(_ => get))(_ == max).run(-1)
      result should ===(Math.max(0, max))
    }
  }

  test("iterateUntil stack safety") {
    val (result, _) = iterateUntil(flatMap(modify(_ + 1))(_ => get))(_ == 50000).run(-1)
    result should ===(50000)
  }

}
