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

package cats.laws.discipline

import cats.{Bifunctor, Eq}
import cats.laws.BifunctorLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

trait BifunctorTests[F[_, _]] extends Laws {
  def laws: BifunctorLaws[F]

  def bifunctor[A, A2, A3, B, B2, B3](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbA2: Arbitrary[A => A2],
    ArbA3: Arbitrary[A2 => A3],
    ArbB2: Arbitrary[B => B2],
    ArbB3: Arbitrary[B2 => B3],
    EqFAB: Eq[F[A, B]],
    EqFCZ: Eq[F[A3, B3]],
    EqFA3B: Eq[F[A3, B]],
    EqFAB3: Eq[F[A, B3]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "Bifunctor",
      parent = None,
      "Bifunctor Identity" -> forAll(laws.bifunctorIdentity[A, B] _),
      "Bifunctor associativity" -> forAll(laws.bifunctorComposition[A, A2, A3, B, B2, B3] _),
      "Bifunctor leftMap Identity" -> forAll(laws.bifunctorLeftMapIdentity[A, B] _),
      "Bifunctor leftMap associativity" -> forAll(laws.bifunctorLeftMapComposition[A, B, A2, A3] _)
    )
}

object BifunctorTests {
  def apply[F[_, _]: Bifunctor]: BifunctorTests[F] =
    new BifunctorTests[F] {
      def laws: BifunctorLaws[F] = BifunctorLaws[F]
    }
}
