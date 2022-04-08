/*
 * Copyright (c) 2022 Typelevel
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

import cats.NonEmptyAlternative
import cats.laws.discipline.NonEmptyAlternativeTests

class NonEmptyAlternativeSuite extends CatsSuite {
  implicit val listWrapperNeAlternative: NonEmptyAlternative[ListWrapper] = ListWrapper.nonEmptyAlternative

  checkAll("Option[Int]", NonEmptyAlternativeTests[Option].nonEmptyAlternative[Int, Int, Int])
  checkAll("List[Int]", NonEmptyAlternativeTests[List].nonEmptyAlternative[Int, Int, Int])
  checkAll("ListWrapper[List[Int]]", NonEmptyAlternativeTests[ListWrapper].nonEmptyAlternative[Int, Int, Int])
  checkAll(
    "compose ListWrapper[List[Int]]",
    NonEmptyAlternativeTests.composed[ListWrapper, List].nonEmptyAlternative[Int, Int, Int]
  )
  checkAll(
    "compose List[ListWrapper[Int]]",
    NonEmptyAlternativeTests.composed[List, ListWrapper].nonEmptyAlternative[Int, Int, Int]
  )
  checkAll(
    "compose ListWrapper[ListWrapper[Int]]",
    NonEmptyAlternativeTests.composed[ListWrapper, ListWrapper].nonEmptyAlternative[Int, Int, Int]
  )
}
