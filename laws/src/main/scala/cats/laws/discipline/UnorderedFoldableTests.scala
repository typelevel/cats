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

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._
import org.typelevel.discipline.Laws
import cats.kernel.CommutativeMonoid
import cats.instances.boolean._

trait UnorderedFoldableTests[F[_]] extends Laws {
  def laws: UnorderedFoldableLaws[F]

  def unorderedFoldable[A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbF: Arbitrary[A => B],
    CogenA: Cogen[A],
    A: CommutativeMonoid[A],
    B: CommutativeMonoid[B],
    EqFA: Eq[A],
    EqFB: Eq[B]
  ): RuleSet =
    new DefaultRuleSet(
      name = "unorderedFoldable",
      parent = None,
      "unorderedFold consistent with unorderedFoldMap" -> forAll(laws.unorderedFoldConsistentWithUnorderedFoldMap[A] _),
      "unorderedFoldMapA identity" -> forAll(laws.unorderedFoldMapAIdentity[A, B] _),
      "forall consistent with exists" -> forAll(laws.forallConsistentWithExists[A] _),
      "forall true if empty" -> forAll(laws.forallEmpty[A] _),
      "nonEmpty reference" -> forAll(laws.nonEmptyRef[A] _),
      "exists is lazy" -> forAll(laws.existsLazy[A] _),
      "forall is lazy" -> forAll(laws.forallLazy[A] _),
      "contains consistent with exists" -> forAll(laws.containsConsistentWithExists[A] _),
      "contains consistent with forall" -> forAll(laws.containsConsistentWithForall[A] _),
      "contains all elements from itself" -> forAll(laws.containsAllElementsFromItself[A] _)
    )
}

object UnorderedFoldableTests {
  def apply[F[_]: UnorderedFoldable]: UnorderedFoldableTests[F] =
    new UnorderedFoldableTests[F] { def laws: UnorderedFoldableLaws[F] = UnorderedFoldableLaws[F] }
}
