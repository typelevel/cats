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

import cats.*
import cats.data.ZipStream
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import org.scalacheck.Prop.*

@annotation.nowarn("cat=deprecation")
class StreamSuite extends CatsSuite {
  checkAll("Stream[Int]", SemigroupalTests[Stream].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Stream]", SerializableTests.serializable(Semigroupal[Stream]))

  checkAll("Stream[Int]", CoflatMapTests[Stream].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Stream]", SerializableTests.serializable(CoflatMap[Stream]))

  checkAll("Stream[Int]", AlternativeTests[Stream].alternative[Int, Int, Int])
  checkAll("Alternative[Stream]", SerializableTests.serializable(Alternative[Stream]))

  checkAll("Stream[Int]", MonadTests[Stream].monad[Int, Int, Int])
  checkAll("Monad[Stream]", SerializableTests.serializable(Monad[Stream]))

  checkAll("Stream[Int] with Option", TraverseTests[Stream].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[Stream]", SerializableTests.serializable(Traverse[Stream]))

  checkAll("Stream[Int]", TraverseFilterTests[Stream].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Stream]", SerializableTests.serializable(TraverseFilter[Stream]))

  checkAll("Stream[Int]", AlignTests[Stream].align[Int, Int, Int, Int])
  checkAll("Align[Stream]", SerializableTests.serializable(Align[Stream]))

  checkAll("Stream[Int]", ShortCircuitingTests[Stream].foldable[Int])
  checkAll("Stream[Int]", ShortCircuitingTests[Stream].traverseFilter[Int])

  // Can't test applicative laws as they don't terminate
  checkAll("ZipStream[Int]", CommutativeApplyTests[ZipStream].apply[Int, Int, Int])

  test("show") {
    assert(Stream(1, 2, 3).show === s"Stream(1, ?)")
    assert(Stream.empty[Int].show === s"Stream()")
  }

  test("Show[Stream] is referentially transparent, unlike Stream.toString") {
    forAll { (stream: Stream[Int]) =>
      if (!stream.isEmpty) {
        val unevaluatedLL = stream.map(identity)
        val initialShow = unevaluatedLL.show

        // Evaluating the tail can cause Stream.toString to return different values,
        // depending on the internal state of the Stream. Show[Stream] should return
        // consistent values independent of internal state.
        unevaluatedLL.tail
        assert(initialShow === (unevaluatedLL.show))
      } else {
        assert(stream.show === (stream.toString))
      }
    }
  }

  test("NonEmptyParallel instance in cats.instances.stream") {
    forAll { (s1: Stream[Int], s2: Stream[String]) =>
      assert((s1, s2).parTupled === s1.zip(s2))
    }
  }
}
