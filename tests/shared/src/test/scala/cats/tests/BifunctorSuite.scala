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

import cats.{Bifunctor, Functor}
import cats.laws.discipline.{BifunctorTests, FunctorTests, SerializableTests}

class BifunctorSuite extends CatsSuite {
  type Tuple2Either[A, B] = (Either[A, B], Either[A, B])
  val tuple2ComposeEither: Bifunctor[Tuple2Either] =
    Bifunctor[Tuple2].compose[Either]

  checkAll("Tuple2 compose Either",
           BifunctorTests(tuple2ComposeEither).bifunctor[Int, Int, Int, String, String, String]
  )
  checkAll("Bifunctor[Tuple2 compose Either]", SerializableTests.serializable(tuple2ComposeEither))

  {
    type LeftFunctor[A] = (Either[A, Int], Either[A, Int])
    implicit val leftFunctor: Functor[LeftFunctor] = tuple2ComposeEither.leftFunctor
    checkAll("Bifunctor[Tuple2 compose Either].leftFunctor", FunctorTests[LeftFunctor].functor[Int, Int, Int])
  }

  {
    type RightFunctor[A] = (Either[Int, A], Either[Int, A])
    implicit val leftFunctor: Functor[RightFunctor] = tuple2ComposeEither.rightFunctor
    checkAll("Bifunctor[Tuple2 compose Either].rightFunctor", FunctorTests[RightFunctor].functor[Int, Int, Int])
  }

  {
    type Tuple2InsideOption[A, B] = Option[(A, B)]
    checkAll(
      "Bifunctor[Option[(A, B)]",
      BifunctorTests[Tuple2InsideOption].bifunctor[String, String, String, Int, Int, Int]
    )
  }
}
