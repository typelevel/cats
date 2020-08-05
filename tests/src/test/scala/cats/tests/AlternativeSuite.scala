package cats.tests

import cats.Alternative
import cats.syntax.eq._
import org.scalacheck.Prop._

class AlternativeSuite extends CatsSuite {
  property("unite") {
    forAll { (list: List[Option[String]]) =>
      val expected = list.collect { case Some(s) => s }

      assert(Alternative[List].unite(list) === (expected))
    }
  }

  property("separate") {
    forAll { (list: List[Either[Int, String]]) =>
      val ints = list.collect { case Left(i) => i }
      val strings = list.collect { case Right(s) => s }
      val expected = (ints, strings)

      assert(Alternative[List].separate(list) === (expected))
    }
  }

  property("separateFoldable") {
    forAll { (list: List[Either[Int, String]]) =>
      val ints = list.collect { case Left(i) => i }
      val strings = list.collect { case Right(s) => s }
      val expected = (ints, strings)

      assert(Alternative[List].separateFoldable(list) === (expected))
    }
  }

  test("guard") {
    assert(Alternative[Option].guard(true).isDefined)
    assert(Alternative[Option].guard(false).isEmpty)
  }
}
