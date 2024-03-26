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

import cats.{Alternative, CoflatMap, Eval, Monad, Semigroupal, Traverse, TraverseFilter}
import cats.laws.discipline.{
  AlternativeTests,
  CoflatMapTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  ShortCircuitingTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.show._
import scala.collection.immutable.Queue
import cats.syntax.eq._

class QueueSuite extends CatsSuite {
  checkAll("Queue[Int]", SemigroupalTests[Queue].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Queue]", SerializableTests.serializable(Semigroupal[Queue]))

  checkAll("Queue[Int]", CoflatMapTests[Queue].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Queue]", SerializableTests.serializable(CoflatMap[Queue]))

  checkAll("Queue[Int]", AlternativeTests[Queue].alternative[Int, Int, Int])
  checkAll("Alternative[Queue]", SerializableTests.serializable(Alternative[Queue]))

  checkAll("Queue[Int]", MonadTests[Queue].monad[Int, Int, Int])
  checkAll("Monad[Queue]", SerializableTests.serializable(Monad[Queue]))

  // Traverse behaviour discriminates on the Runtime type of the Applicative
  checkAll("Queue[Int] with Option", TraverseTests[Queue].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Queue[Int] with Eval", TraverseTests[Queue].traverse[Int, Int, Int, Set[Int], Eval, Eval])
  checkAll("Traverse[Queue]", SerializableTests.serializable(Traverse[Queue]))

  checkAll("Queue[Int]", TraverseFilterTests[Queue].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Queue]", SerializableTests.serializable(TraverseFilter[Queue]))

  checkAll("Queue[Int]", ShortCircuitingTests[Queue].foldable[Int])
  checkAll("Queue[Int]", ShortCircuitingTests[Queue].traverseFilter[Int])

  test("show") {
    assert(Queue(1, 2, 3).show === "Queue(1, 2, 3)")
    assert(Queue.empty[Int].show === "Queue()")
  }

  test("traverse is stack-safe") {
    val queue = (0 until 100000).foldLeft(Queue.empty[Int])(_ :+ _)
    val sumAll = Traverse[Queue]
      .traverse(queue) { i => () => i }
      .apply()
      .iterator
      .sum

    assert(sumAll == queue.sum)
  }
}
