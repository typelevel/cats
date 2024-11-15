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
import Prop.*
import cats.arrow.Profunctor
import org.typelevel.discipline.Laws

trait ProfunctorTests[F[_, _]] extends Laws {
  def laws: ProfunctorLaws[F]

  def profunctor[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFCD: Arbitrary[F[C, D]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenD: Cogen[D],
    CogenE: Cogen[E],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]],
    EqFAG: Eq[F[A, G]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "profunctor",
      parent = None,
      "profunctor identity" -> forAll(laws.profunctorIdentity[A, B] _),
      "profunctor composition" -> forAll(laws.profunctorComposition[A, B, C, D, E, G] _),
      "profunctor lmap identity" -> forAll(laws.profunctorLmapIdentity[A, B] _),
      "profunctor rmap identity" -> forAll(laws.profunctorRmapIdentity[A, B] _),
      "profunctor lmap composition" -> forAll(laws.profunctorLmapComposition[A, B, C, D] _),
      "profunctor rmap composition" -> forAll(laws.profunctorRmapComposition[A, D, C, B] _)
    )
}

object ProfunctorTests {
  def apply[F[_, _]: Profunctor]: ProfunctorTests[F] =
    new ProfunctorTests[F] { def laws: ProfunctorLaws[F] = ProfunctorLaws[F] }
}
