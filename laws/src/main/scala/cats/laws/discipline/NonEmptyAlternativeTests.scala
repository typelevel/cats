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

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop.*

trait NonEmptyAlternativeTests[F[_]] extends ApplicativeTests[F] with SemigroupKTests[F] {
  def laws: NonEmptyAlternativeLaws[F]

  def nonEmptyAlternative[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name: String = "nonEmptyAlternative"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(semigroupK[A], applicative[A, B, C])
      val props: Seq[(String, Prop)] = Seq(
        "left distributivity" -> forAll(laws.nonEmptyAlternativeLeftDistributivity[A, B] _),
        "right distributivity" -> forAll(laws.nonEmptyAlternativeRightDistributivity[A, B] _),
        "prependK consistent with pure and combineK" ->
          forAll(laws.nonEmptyAlternativePrependKConsitentWithPureAndCombineK[A] _),
        "appendK consistent with pure and combineK" ->
          forAll(laws.nonEmptyAlternativeAppendKConsitentWithPureAndCombineK[A] _)
      )
    }
}

object NonEmptyAlternativeTests {
  def apply[F[_]: NonEmptyAlternative]: NonEmptyAlternativeTests[F] =
    new NonEmptyAlternativeTests[F] { def laws: NonEmptyAlternativeLaws[F] = NonEmptyAlternativeLaws[F] }

  def composed[F[_]: NonEmptyAlternative, G[_]: Applicative]: NonEmptyAlternativeTests[λ[α => F[G[α]]]] =
    new NonEmptyAlternativeTests[λ[α => F[G[α]]]] {
      def laws: NonEmptyAlternativeLaws[λ[α => F[G[α]]]] = NonEmptyAlternativeLaws.composed[F, G]
    }
}
