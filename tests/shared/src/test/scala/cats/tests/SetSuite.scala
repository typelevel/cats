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

import cats.{MonoidK, Show, UnorderedTraverse}
import cats.data.Validated
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{MonoidKTests, SerializableTests, UnorderedTraverseTests}
import cats.laws.discipline.arbitrary._
import cats.syntax.show._
import cats.syntax.eq._

class SetSuite extends CatsSuite {
  checkAll("Set[Int]", MonoidTests[Set[Int]].monoid)

  checkAll("Set[Int]", MonoidKTests[Set].monoidK[Int])
  checkAll("MonoidK[Set]", SerializableTests.serializable(MonoidK[Set]))

  checkAll("Set[Int]", UnorderedTraverseTests[Set].unorderedTraverse[Int, Int, Int, Validated[Int, *], Option])
  checkAll("UnorderedTraverse[Set]", SerializableTests.serializable(UnorderedTraverse[Set]))

  test("show") {
    assert(Set(1, 1, 2, 3).show === "Set(1, 2, 3)")
    assert(Set.empty[String].show === "Set()")
  }

  test("show keeps separate entries for items that map to identical strings") {
    // note: this val name has to be the same to shadow the cats.instances instance
    implicit val catsStdShowForInt: Show[Int] = _ => "1"
    // an implementation implemented as set.map(_.show).mkString(", ") would
    // only show one entry in the result instead of 3, because Set.map combines
    // duplicate items in the codomain.
    assert(Set(1, 2, 3).show === "Set(1, 1, 1)")
  }
}
