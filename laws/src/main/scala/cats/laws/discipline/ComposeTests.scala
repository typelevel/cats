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

import cats.arrow.Compose
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop.*
import org.typelevel.discipline.Laws

trait ComposeTests[F[_, _]] extends Laws {
  def laws: ComposeLaws[F]

  def compose[A, B, C, D](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    EqFAD: Eq[F[A, D]]
  ): RuleSet =
    new DefaultRuleSet(name = "compose",
                       parent = None,
                       "compose associativity" -> forAll(laws.composeAssociativity[A, B, C, D] _)
    )
}

object ComposeTests {
  def apply[F[_, _]: Compose]: ComposeTests[F] =
    new ComposeTests[F] { def laws: ComposeLaws[F] = ComposeLaws[F] }
}
