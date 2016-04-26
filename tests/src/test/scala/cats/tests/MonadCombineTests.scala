package cats
package tests

import cats.data.Xor
import cats.laws.discipline.arbitrary.xorArbitrary

class MonadCombineTest extends CatsSuite {
  test("separate") {
    forAll { (list: List[Xor[Int, String]]) =>
      val ints = list.collect { case Xor.Left(i) => i }
      val strings = list.collect { case Xor.Right(s) => s }
      val expected = (ints, strings)

      MonadCombine[List].separate(list) should === (expected)
    }
  }
}
