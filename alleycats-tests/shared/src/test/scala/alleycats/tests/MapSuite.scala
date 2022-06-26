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

import cats.Traverse
import cats.instances.all._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{SerializableTests, ShortCircuitingTests, TraverseFilterTests}

class MapSuite extends AlleycatsSuite {
  checkAll("Traverse[Map[Int, *]]", SerializableTests.serializable(Traverse[Map[Int, *]]))

  checkAll("TraverseFilter[Map[Int, *]]", TraverseFilterTests[Map[Int, *]].traverseFilter[Int, Int, Int])

  checkAll("Map[Int, *]", ShortCircuitingTests[Map[Int, *]].traverseFilter[Int])

  test("traverse is stack-safe") {
    val items = Map((0 until 100000).map { i => (i.toString, i) }: _*)
    val sumAll = Traverse[Map[String, *]]
      .traverse(items) { i => () => i }
      .apply()
      .values
      .sum

    assert(sumAll == items.values.sum)
  }
}
