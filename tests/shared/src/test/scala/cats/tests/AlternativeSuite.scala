/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.Alternative
import cats.FlatMap
import cats.laws.discipline.AlternativeTests
import cats.syntax.eq.*
import org.scalacheck.Prop.*

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
