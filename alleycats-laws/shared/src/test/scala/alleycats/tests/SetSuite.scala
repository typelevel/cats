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

package alleycats.tests

import alleycats.laws.discipline.*
import alleycats.std.all.*
import cats.{Alternative, Foldable}
import cats.instances.all.*
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.{AlternativeTests, FunctorTests, ShortCircuitingTests, TraverseFilterTests}

class SetSuite extends AlleycatsSuite {
  implicit val iso: Isomorphisms[Set] = Isomorphisms.invariant[Set](alleycatsStdInstancesForSet)

  checkAll("FlatMapRec[Set]", FlatMapRecTests[Set].tailRecM[Int])

  checkAll("Foldable[Set]", SerializableTests.serializable(Foldable[Set]))

  checkAll("TraverseFilter[Set]", TraverseFilterTests[Set].traverseFilter[Int, Int, Int])

  checkAll("Set[Int]", AlternativeTests[Set].alternative[Int, Int, Int])

  checkAll("Functor[Int]", FunctorTests[Set].functor[Int, Int, Int])

  checkAll("Alternative[Set]", SerializableTests.serializable(Alternative[Set]))

  checkAll("Set[Int]", ShortCircuitingTests[Set].traverseFilter[Int])
}
