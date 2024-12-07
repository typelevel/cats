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

import cats.{Align, FlatMap, MonoidK, Semigroupal, Show, Traverse, TraverseFilter}
import cats.kernel.{CommutativeMonoid, Monoid}
import cats.kernel.laws.discipline.{CommutativeMonoidTests, HashTests, MonoidTests}
import cats.laws.discipline.{
  AlignTests,
  FlatMapTests,
  MonoidKTests,
  SemigroupalTests,
  SerializableTests,
  ShortCircuitingTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import cats.syntax.show.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*

import scala.collection.immutable.SortedMap

class SortedMapSuite extends CatsSuite {
  implicit val iso: Isomorphisms[SortedMap[Int, *]] = Isomorphisms.invariant[SortedMap[Int, *]]

  checkAll("SortedMap[Int, Int]", SemigroupalTests[SortedMap[Int, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[SortedMap[Int, *]]", SerializableTests.serializable(Semigroupal[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, Int]", FlatMapTests[SortedMap[Int, *]].flatMap[Int, Int, Int])
  checkAll("FlatMap[SortedMap[Int, *]]", SerializableTests.serializable(FlatMap[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, Int] with Option",
           TraverseTests[SortedMap[Int, *]].traverse[Int, Int, Int, Int, Option, Option]
  )
  checkAll("Traverse[SortedMap[Int, *]]", SerializableTests.serializable(Traverse[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, Int]", TraverseFilterTests[SortedMap[Int, *]].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[SortedMap[Int, *]]", SerializableTests.serializable(TraverseFilter[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, Int]", AlignTests[SortedMap[Int, *]].align[Int, Int, Int, Int])
  checkAll("Align[SortedMap[Int, *]]", SerializableTests.serializable(Align[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, *]", ShortCircuitingTests[SortedMap[Int, *]].foldable[Int])
  checkAll("SortedMap[Int, *]", ShortCircuitingTests[SortedMap[Int, *]].traverseFilter[Int])

  test("show isn't empty and is formatted as expected") {
    forAll { (map: SortedMap[Int, String]) =>
      assert(map.show.nonEmpty)
      assert(map.show.startsWith("SortedMap("))
      assert(map.show === (implicitly[Show[SortedMap[Int, String]]].show(map)))
    }
  }

  checkAll("Hash[SortedMap[Int, String]]", HashTests[SortedMap[Int, String]].hash)
  checkAll("CommutativeMonoid[SortedMap[String, Int]]",
           CommutativeMonoidTests[SortedMap[String, Int]].commutativeMonoid
  )
  checkAll("CommutativeMonoid[SortedMap[String, Int]]",
           SerializableTests.serializable(CommutativeMonoid[SortedMap[String, Int]])
  )
  checkAll("Monoid[SortedMap[String, String]]", MonoidTests[SortedMap[String, String]].monoid)
  checkAll("Monoid[SortedMap[String, String]]", SerializableTests.serializable(Monoid[SortedMap[String, String]]))

  checkAll("SortedMap[String, String]", MonoidKTests[SortedMap[String, *]].monoidK[String])
  checkAll("MonoidK[SortedMap[String, *]]", SerializableTests.serializable(MonoidK[SortedMap[String, *]]))

  test("traverse is stack-safe") {
    val items = SortedMap((0 until 100000).map { i => (i.toString, i) }: _*)
    val sumAll = Traverse[SortedMap[String, *]]
      .traverse(items) { i => () => i }
      .apply()
      .values
      .sum

    assert(sumAll == items.values.sum)
  }
}
