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

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Prop._

import arbitrary._

trait BireducibleTests[F[_, _]] extends BifoldableTests[F] {
  def laws: BireducibleLaws[F]

  def bireducible[
    A: Arbitrary: Cogen: Eq: Semigroup,
    B: Arbitrary: Cogen: Eq: Semigroup,
    C: Arbitrary: Cogen: Eq: Monoid
  ](implicit arbF: Arbitrary[F[A, B]]): RuleSet =
    new DefaultRuleSet(
      "bireducible",
      Some(bifoldable[A, B, C]),
      "bireduceLeft consistent with default implementation" ->
        forAll(laws.bireduceLeftConsistentWithDefaultImplementation[A, B] _),
      "bireduceRight consistent with default implementation" ->
        forAll(laws.bireduceRightConsistentWithDefaultImplementation[A, B] _),
      "bireduceMap consistent with default implementation" ->
        forAll(laws.bireduceMapConsistentWithDefaultImplementation[A, B, C] _),
      "bireduce consistent with default implementation" ->
        forAll(laws.bireduceConsistentWithDefaultImplementation[A, B] _),
      "bireduceLeftTo consistent with bireduceRightTo" ->
        forAll(laws.bireduceLeftToConsistentWithBireduceRightTo[A, B, C] _)
    )
}

object BireducibleTests {
  def apply[F[_, _]: Bireducible]: BireducibleTests[F] =
    new BireducibleTests[F] { def laws: BireducibleLaws[F] = BireducibleLaws[F] }
}
