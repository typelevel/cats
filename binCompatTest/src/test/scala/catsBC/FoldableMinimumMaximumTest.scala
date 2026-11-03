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

package catsBC

import cats.instances.int.*
import munit.FunSuite

class FoldableMinimumMaximumTest extends FunSuite {
  private val expected = (Some(1), Some(3))

  test("old direct Foldable callers run against current Cats") {
    assertEquals(FoldableMinimumMaximum.direct, expected)
  }

  test("old Foldable syntax callers run against current Cats") {
    assertEquals(FoldableMinimumMaximum.syntax, expected)
  }

  test("old Foldable Ops and AllOps implementations run against current Cats") {
    assertEquals(FoldableMinimumMaximum.throughOps, expected)
    assertEquals(FoldableMinimumMaximum.throughAllOps, expected)
    assertEquals(FoldableMinimumMaximum.throughTraverseOps, expected)
  }

  test("old UnorderedFoldable implementations inherit the new default methods") {
    val F = FoldableMinimumMaximum.unorderedFoldable
    assertEquals(F.minimumOption(FoldableMinimumMaximum.values), Some(1))
    assertEquals(F.maximumOption(FoldableMinimumMaximum.values), Some(3))
    assertEquals(F.minimumOption(List.empty[Int]), None)
    assertEquals(F.maximumOption(List.empty[Int]), None)
  }
}
