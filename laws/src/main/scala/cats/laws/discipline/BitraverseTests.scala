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
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait BitraverseTests[F[_, _]] extends BifoldableTests[F] with BifunctorTests[F] {
  def laws: BitraverseLaws[F]

  def bitraverse[G[_], A, B, C, D, E, H](implicit
    G: Applicative[G],
    C: Monoid[C],
    ArbFAB: Arbitrary[F[A, B]],
    ArbFAD: Arbitrary[F[A, D]],
    ArbGC: Arbitrary[G[C]],
    ArbGD: Arbitrary[G[D]],
    ArbGE: Arbitrary[G[E]],
    ArbGH: Arbitrary[G[H]],
    ArbA: Arbitrary[A],
    ArbB: Arbitrary[B],
    ArbC: Arbitrary[C],
    ArbE: Arbitrary[E],
    ArbH: Arbitrary[H],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenD: Cogen[D],
    CogenE: Cogen[E],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]],
    EqFAH: Eq[F[A, H]],
    EqFCD: Eq[F[C, D]],
    EqFCH: Eq[F[C, H]],
    EqGGFEH: Eq[G[G[F[E, H]]]],
    EqC: Eq[C]
  ): RuleSet =
    new RuleSet {
      val name = "bitraverse"
      val parents = Seq(bifoldable[A, B, C], bifunctor[A, B, C, D, E, H])
      val bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      val props = Seq(
        "bitraverse identity" -> forAll(laws.bitraverseIdentity[A, B] _),
        "bitraverse composition" -> forAll(laws.bitraverseCompose[G, A, B, C, D, E, H] _)
      )
    }
}

object BitraverseTests {
  def apply[F[_, _]: Bitraverse]: BitraverseTests[F] =
    new BitraverseTests[F] { def laws: BitraverseLaws[F] = BitraverseLaws[F] }
}
