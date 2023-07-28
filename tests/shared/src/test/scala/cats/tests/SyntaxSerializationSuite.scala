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

import cats.laws.discipline.SerializableTests
import cats.syntax.either._
import cats.Semigroupal

/**
 * Test that our syntax implicits are serializable.
 */
class SyntaxSerializationSuite extends CatsSuite {
  checkAll(
    "Tuple3SemigroupalOps[Option, Boolean, Int, Long]",
    SerializableTests.serializable(
      cats.syntax.all.catsSyntaxTuple3Semigroupal[Option, Boolean, Int, Long]((None, None, None))
    )
  )

  checkAll("SemigroupalOps[Option, Int]",
           SerializableTests.serializable(cats.syntax.all.catsSyntaxSemigroupal[Option, Int](None, Semigroupal[Option]))
  )

  checkAll(
    "Tuple3ParallelOps[Either[String, *], Boolean, Int, Long]",
    SerializableTests.serializable(
      cats.syntax.all.catsSyntaxTuple3Parallel(("a".asLeft[Boolean], "b".asLeft[Int], "c".asLeft[Long]))
    )
  )
}
