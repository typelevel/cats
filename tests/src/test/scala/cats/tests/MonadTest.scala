package cats
package tests

import cats.data.{State}
import org.scalacheck.Gen

class MonadTest extends CatsSuite {
  val testInstance = MonadState[State[Int, ?], Int]
  import testInstance._

  val smallInteger = Gen.choose(0,100)

  test("whileM_") {
    forAll(smallInteger) { (max: Int) =>
      val (result, _) = whileM_(inspect(i => !(i > max)), modify(i => i + 1)).run(0).value
      result should ===(max + 1)
    }
  }

  test("whileM") {
    forAll(smallInteger) { (max: Int) =>
      val (result,aggregation) = whileM[List, Unit](inspect(i => !(i > max)))(modify(i => i + 1)).run(0).value
      result should ===(max + 1)
      aggregation.size should ===(max + 1)
      aggregation.distinct should ===(List(()))
    }
  }

  test("whileM_ stack safety") {
    val (result, _) = whileM_(inspect(i => !(i > 1000000)), modify(i => i + 1)).run(0).value
    result should ===(1000001)
  }

  test("whileM stack safety") {
    val (result,_) = whileM[List, Unit](inspect(i => !(i > 1000000)))(modify(i => i + 1)).run(0).value
    result should ===(1000001)
  }
}
