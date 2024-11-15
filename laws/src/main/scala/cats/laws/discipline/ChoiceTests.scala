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

import cats.arrow.Choice
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*

trait ChoiceTests[F[_, _]] extends CategoryTests[F] {
  def laws: ChoiceLaws[F]

  def choice[A, B, C, D](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFAC: Arbitrary[F[A, C]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]],
    EqFEitherABD: Eq[F[Either[A, B], D]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "choice",
      parent = Some(category[A, B, C, D]),
      "choice composition distributivity" -> forAll(laws.choiceCompositionDistributivity[A, B, C, D] _)
    )
}

object ChoiceTests {
  def apply[F[_, _]: Choice]: ChoiceTests[F] =
    new ChoiceTests[F] { def laws: ChoiceLaws[F] = ChoiceLaws[F] }
}
