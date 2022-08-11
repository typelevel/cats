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
import cats.kernel.CommutativeMonoid

trait UnorderedTraverseTests[F[_]] extends UnorderedFoldableTests[F] {
  def laws: UnorderedTraverseLaws[F]

  def unorderedTraverse[
    A: Arbitrary,
    B: Arbitrary,
    C: Arbitrary,
    X[_]: CommutativeApplicative,
    Y[_]: CommutativeApplicative
  ](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFXB: Arbitrary[F[X[B]]],
    ArbXB: Arbitrary[X[B]],
    ArbYB: Arbitrary[Y[B]],
    ArbYC: Arbitrary[Y[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    Ca: CommutativeMonoid[A],
    Cb: CommutativeMonoid[B],
    EqA: Eq[A],
    EqB: Eq[B],
    EqXYFC: Eq[X[Y[F[C]]]],
    EqXFB: Eq[X[F[B]]],
    EqYFB: Eq[Y[F[B]]]
  ): RuleSet = new DefaultRuleSet(
    name = "unorderedTraverse",
    parent = Some(unorderedFoldable[A, B]),
    "unordered traverse sequential composition" -> forAll(
      laws.unorderedTraverseSequentialComposition[A, B, C, X, Y] _
    ),
    "unordered traverse parallel composition" -> forAll(laws.unorderedTraverseParallelComposition[A, B, X, Y] _),
    "unordered traverse consistent with sequence" -> forAll(laws.unorderedSequenceConsistent[B, X] _)
  )
}

object UnorderedTraverseTests {
  def apply[F[_]: UnorderedTraverse]: UnorderedTraverseTests[F] =
    new UnorderedTraverseTests[F] { def laws: UnorderedTraverseLaws[F] = UnorderedTraverseLaws[F] }
}
