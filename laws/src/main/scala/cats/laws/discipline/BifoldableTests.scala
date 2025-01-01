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

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

trait BifoldableTests[F[_, _]] extends Laws {
  def laws: BifoldableLaws[F]

  def bifoldable[A: Arbitrary, B: Arbitrary, C: Arbitrary: Monoid: Eq](implicit
    ArbFAB: Arbitrary[F[A, B]],
    CogenA: Cogen[A],
    CogenB: Cogen[B]
  ): RuleSet =
    new DefaultRuleSet(
      name = "bifoldable",
      parent = None,
      "bifoldLeft consistent with bifoldMap" -> forAll(laws.bifoldLeftConsistentWithBifoldMap[A, B, C] _),
      "bifoldRight consistent with bifoldMap" -> forAll(laws.bifoldRightConsistentWithBifoldMap[A, B, C] _)
    )
}

object BifoldableTests {
  def apply[F[_, _]: Bifoldable]: BifoldableTests[F] =
    new BifoldableTests[F] { def laws: BifoldableLaws[F] = BifoldableLaws[F] }
}
