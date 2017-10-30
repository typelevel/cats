package cats
package tests

class MonadCombineSuite extends CatsSuite {
  test("separate") {
    forAll { (list: List[Either[Int, String]]) =>
      val ints = list.collect { case Left(i) => i }
      val strings = list.collect { case Right(s) => s }
      val expected = (ints, strings)

      Alternative[List].separate(list) should === (expected)
    }
  }
}
