package cats
package tests

class AlternativeSuite extends CatsSuite {
  test("unite") {
    forAll { (list: List[Option[String]]) =>
      val expected = list.collect { case Some(s) => s }

      Alternative[List].unite(list) should === (expected)
    }
  }

  test("lefts") {
    forAll { (list: List[Either[Int, String]]) =>
      val expected = list.collect { case Left(i) => i }

      list.lefts should === (expected)
    }
  }

  test("rights") {
    forAll { (list: List[Either[Int, String]]) =>
      val expected = list.collect { case Right(s) => s }

      list.rights should === (expected)
    }
  }

  test("separate") {
    forAll { (list: List[Either[Int, String]]) =>
      val ints = list.collect { case Left(i) => i }
      val strings = list.collect { case Right(s) => s }
      val expected = (ints, strings)

      Alternative[List].separate(list) should === (expected)
    }
  }

  test("guard") {
    assert(Alternative[Option].guard(true).isDefined)
    assert(Alternative[Option].guard(false).isEmpty)
  }
}
