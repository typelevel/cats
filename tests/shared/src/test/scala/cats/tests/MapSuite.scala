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

import cats.{Align, Eval, FlatMap, FunctorFilter, MonoidK, Semigroupal, Show, UnorderedTraverse}
import cats.arrow.Compose
import cats.kernel.{CommutativeMonoid, Monoid}
import cats.kernel.instances.StaticMethods.wrapMutableMap
import cats.kernel.laws.discipline.{CommutativeMonoidTests, HashTests, MonoidTests}
import cats.laws.discipline.{
  AlignTests,
  ComposeTests,
  FlatMapTests,
  FunctorFilterTests,
  MonoidKTests,
  SemigroupalTests,
  SerializableTests,
  UnorderedTraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.all._
import org.scalacheck.Prop._

class MapSuite extends CatsSuite {

  checkAll("Map[Int, Int]", SemigroupalTests[Map[Int, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Map[Int, *]]", SerializableTests.serializable(Semigroupal[Map[Int, *]]))

  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, *]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Map[Int, *]]", SerializableTests.serializable(FlatMap[Map[Int, *]]))

  // Traverse behaviour discriminates on the Runtime type of the Applicative
  checkAll("Map[Int, Int] with Option",
           UnorderedTraverseTests[Map[Int, *]].unorderedTraverse[Int, Int, Int, Option, Option]
  )
  checkAll("Map[Int, Int] with Eval", UnorderedTraverseTests[Map[Int, *]].unorderedTraverse[Int, Int, Int, Eval, Eval])
  checkAll("UnorderedTraverse[Map[Int, *]]", SerializableTests.serializable(UnorderedTraverse[Map[Int, *]]))

  checkAll("Map[Int, Int]", FunctorFilterTests[Map[Int, *]].functorFilter[Int, Int, Int])
  checkAll("FunctorFilter[Map[Int, *]]", SerializableTests.serializable(FunctorFilter[Map[Int, *]]))

  checkAll("Map[Int, Long]", ComposeTests[Map].compose[Int, Long, String, Double])
  checkAll("Compose[Map]", SerializableTests.serializable(Compose[Map]))

  checkAll("Map[Int, Int]", MonoidKTests[Map[Int, *]].monoidK[Int])
  checkAll("MonoidK[Map[Int, *]]", SerializableTests.serializable(MonoidK[Map[Int, *]]))

  checkAll("Map[Int, Int]", AlignTests[Map[Int, *]].align[Int, Int, Int, Int])
  checkAll("Align[Map]", SerializableTests.serializable(Align[Map[Int, *]]))

  checkAll("Hash[Map[Int, String]]", HashTests[Map[Int, String]].hash)
  checkAll("CommutativeMonoid[Map[String, Int]]", CommutativeMonoidTests[Map[String, Int]].commutativeMonoid)
  checkAll("CommutativeMonoid[Map[String, Int]]", SerializableTests.serializable(CommutativeMonoid[Map[String, Int]]))
  checkAll("Monoid[Map[String, String]]", MonoidTests[Map[String, String]].monoid)
  checkAll("Monoid[Map[String, String]]", SerializableTests.serializable(Monoid[Map[String, String]]))

  test("show isn't empty and is formatted as expected") {
    forAll { (map: Map[Int, String]) =>
      assert(map.show.nonEmpty === true)
      assert(map.show.startsWith("Map(") === true)
      assert(map.show === (implicitly[Show[Map[Int, String]]].show(map)))
    }
  }

  {
    val m = wrapMutableMap(scala.collection.mutable.Map(1 -> "one", 2 -> "two"))
    checkAll("WrappedMutableMap", SerializableTests.serializable(m))
  }

  test("unorderedTraverse doesn't stack overflow with tuples") {
    // https://github.com/typelevel/cats/issues/4461
    val sum = (1 to 1000000).sum
    val map = (1 to 1000000).map(x => x -> x).toMap
    val resL = map.unorderedTraverse(x => (x, x))
    val resV = (sum, map)
    assert(resL === resV)
  }

  test("unorderedTraverse doesn't stack overflow with Options") {
    // https://github.com/typelevel/cats/issues/4461
    val map = (1 to 1000000).map(x => x -> x).toMap
    val resL = map.unorderedTraverse(x => x.some)
    val resV = Some(map)
    assert(resL === resV)
  }
}
