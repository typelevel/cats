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

import cats.Endo
import cats.arrow.Compose
import cats.kernel.laws.discipline.SemigroupTests
import cats.laws.discipline.{MiniInt, SemigroupKTests, SerializableTests}
import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive
import cats.laws.discipline.arbitrary.*
import cats.syntax.compose.*

class ComposeSuite extends CatsSuite {
  val functionCompose = Compose[Function1]

  checkAll("Compose[Function1].algebraK", SemigroupKTests[Endo](using functionCompose.algebraK).semigroupK[MiniInt])
  checkAll("Compose[Function1].algebraK", SerializableTests.serializable(functionCompose.algebraK))

  val functionAlgebra = functionCompose.algebra[MiniInt]
  checkAll("Compose[Function1].algebra[MiniInt]", SemigroupTests[Endo[MiniInt]](using functionAlgebra).semigroup)

  test("syntax") {
    assertEquals((((_: Int) + 1) <<< ((_: Int) / 2))(2), 2)
    assertEquals((((_: Int) + 1) >>> ((_: Int) / 2))(5), 3)
  }
}
