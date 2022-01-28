package cats.tests

import cats.Alternative
import cats.FlatMap
import cats.laws.discipline.AlternativeTests
import cats.syntax.eq._
import org.scalacheck.Prop._

class AlternativeSuite extends CatsSuite {

  // Alternative[ListWrapper] is different from Alternative[List] since the former does not override any default implementation.
  implicit val listWrapperAlternative: Alternative[ListWrapper] = ListWrapper.alternative

  checkAll("compose List[List[Int]]", AlternativeTests.composed[List, List].alternative[Int, Int, Int])
  checkAll("compose List[Option[Int]]", AlternativeTests.composed[List, Option].alternative[Int, Int, Int])
  checkAll("compose Option[List[Int]]", AlternativeTests.composed[Option, List].alternative[Int, Int, Int])
  checkAll("compose Option[Option[Int]]", AlternativeTests.composed[Option, Option].alternative[Int, Int, Int])
  checkAll("compose List[ListWrapper[Int]]", AlternativeTests.composed[List, ListWrapper].alternative[Int, Int, Int])
  checkAll("compose ListWrapper[List[Int]]", AlternativeTests.composed[ListWrapper, List].alternative[Int, Int, Int])

  property("unite") {
    forAll { (list: List[Option[String]]) =>
      val expected = list.collect { case Some(s) => s }

      assert(Alternative[List].unite(list) === expected)

      // See #3997: check that correct `unite` version is picked up.
      implicit val listWrapperAlternative: Alternative[ListWrapper] = ListWrapper.alternative
      implicit val listWrapperFlatMap: FlatMap[ListWrapper] = ListWrapper.flatMap

      assert(Alternative[ListWrapper].unite(ListWrapper(list)).list === expected)
    }
  }

  property("separate") {
    forAll { (list: List[Either[Int, String]]) =>
      val expectedInts = list.collect { case Left(i) => i }
      val expectedStrings = list.collect { case Right(s) => s }
      val expected = (expectedInts, expectedStrings)

      assert(Alternative[List].separate(list) === expected)

      // See #3997: check that correct `separate` version is picked up.
      implicit val listWrapperAlternative: Alternative[ListWrapper] = ListWrapper.alternative
      implicit val listWrapperFlatMap: FlatMap[ListWrapper] = ListWrapper.flatMap

      val (obtainedLwInts, obtainedLwStrings) = Alternative[ListWrapper].separate(ListWrapper(list))
      assert(obtainedLwInts.list === expectedInts)
      assert(obtainedLwStrings.list === expectedStrings)
    }
  }

  property("separateFoldable") {
    forAll { (list: List[Either[Int, String]]) =>
      val ints = list.collect { case Left(i) => i }
      val strings = list.collect { case Right(s) => s }
      val expected = (ints, strings)

      assert(Alternative[List].separateFoldable(list) === expected)
    }
  }

  test("guard") {
    assert(Alternative[Option].guard(true).isDefined)
    assert(Alternative[Option].guard(false).isEmpty)
  }
}
