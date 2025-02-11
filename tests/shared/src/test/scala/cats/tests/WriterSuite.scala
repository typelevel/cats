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

import cats.data.Writer
import cats.syntax.applicative.*
import cats.syntax.writer.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class WriterSuite extends CatsSuite {
  test("pure syntax creates a writer with an empty log") {
    forAll { (result: String) =>
      type Logged[A] = Writer[List[Int], A]
      assert(result.pure[Logged] === (Writer(List.empty[Int], result)))
    }
  }

  test("tell syntax creates a writer with a unit result") {
    forAll { (log: List[Int]) =>
      assert(log.tell === (Writer(log, ())))
    }
  }

  test("writer syntax creates a writer with the specified result and log") {
    forAll { (result: String, log: List[Int]) =>
      assert(result.writer(log) === (Writer(log, result)))
    }
  }

  test("catsDataCommutativeMonadForWriterT and catsDataTraverseForWriterTId instances are not ambiguous") {
    import cats.Functor
    Functor[Writer[Int, *]]
  }
}
