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

package cats
package laws
package discipline

import cats.arrow.Category
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop.*

trait CategoryTests[F[_, _]] extends ComposeTests[F] {
  def laws: CategoryLaws[F]

  def category[A, B, C, D](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "category",
      parent = Some(compose[A, B, C, D]),
      "category left identity" -> forAll(laws.categoryLeftIdentity[A, B] _),
      "category right identity" -> forAll(laws.categoryRightIdentity[A, B] _)
    )
}

object CategoryTests {
  def apply[F[_, _]: Category]: CategoryTests[F] =
    new CategoryTests[F] { def laws: CategoryLaws[F] = CategoryLaws[F] }
}
