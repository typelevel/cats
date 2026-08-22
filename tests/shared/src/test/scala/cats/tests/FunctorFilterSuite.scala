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

import cats.syntax.all.*
import cats.Functor
import cats.FunctorFilter

class FunctorFilterSuite extends CatsSuite {

  test("withFilter alias allows for-comprehensions with guards") {
    // Explicitly use FunctorFilter to provide the withFilter method
    // to prove that for-comprehension guards work on any FunctorFilter
    def filterEvens[F[_]: FunctorFilter, A](fa: F[A])(implicit
      ev: A =:= Int
    ): F[A] = {
      implicit val F: Functor[F] = FunctorFilter[F].functor
      for {
        a <- fa
        if ev(a) % 2 == 0
      } yield a
    }

    val list = List(1, 2, 3, 4, 5)
    val evens = filterEvens(list)

    assertEquals(evens, List(2, 4))
  }

  test("withFilter is lazy and does not evaluate until map is called") {
    var evaluationCount = 0

    val list = List(1, 2, 3)

    def getWrapper[F[_]: FunctorFilter, A](fa: F[A])(f: A => Boolean) =
      fa.withFilter(f)

    // Create the lazy WithFilter wrapper.
    // If it were strict, it would iterate immediately.
    val lazyWrapper = getWrapper(list) { x =>
      evaluationCount += 1
      x > 1
    }

    // It has not been mapped yet, so the evaluation count should be 0.
    assertEquals(evaluationCount, 0)

    // Now we map over it. This forces the evaluation.
    val result = lazyWrapper.map(_ * 2)

    // The list has 3 elements, so the predicate should be called 3 times.
    assertEquals(evaluationCount, 3)
    assertEquals(result, List(4, 6))
  }

}
