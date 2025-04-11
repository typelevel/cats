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

import cats.Bifoldable
import cats.laws.discipline.{BifoldableTests, SerializableTests}
import cats.syntax.either.*
import cats.syntax.eq.*

class BifoldableSuite extends CatsSuite {
  type EitherEither[A, B] = Either[Either[A, B], Either[A, B]]
  val eitherComposeEither: Bifoldable[EitherEither] =
    Bifoldable[Either].compose[Either]

  checkAll("Either compose Either", BifoldableTests(using eitherComposeEither).bifoldable[Int, Int, Int])
  checkAll("Bifoldable[Either compose Either]", SerializableTests.serializable(eitherComposeEither))

  test("bifold works for 2 monoids") {
    assert(Bifoldable[Either].bifold(Either.right[Int, String]("something")) === ((0, "something")))
    assert(Bifoldable[Either].bifold(Either.left[Int, String](5)) === ((5, "")))
  }
}
