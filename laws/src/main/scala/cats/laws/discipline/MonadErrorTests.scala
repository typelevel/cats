/*
 * Copyright (c) 2022 Typelevel
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

import cats.data.EitherT
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop.forAll

trait MonadErrorTests[F[_], E] extends ApplicativeErrorTests[F, E] with MonadTests[F] {
  def laws: MonadErrorLaws[F, E]

  def monadError[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFU: Arbitrary[F[Unit]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    ArbE: Arbitrary[E],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenE: Cogen[E],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqE: Eq[E],
    EqFEitherEU: Eq[F[Either[E, Unit]]],
    EqFEitherEA: Eq[F[Either[E, A]]],
    EqEitherTFEA: Eq[EitherT[F, E, A]],
    EqFABC: Eq[F[(A, B, C)]],
    EqFInt: Eq[F[Int]],
    iso: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      def name: String = "monadError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicativeError[A, B, C], monad[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "monadError left zero" -> forAll(laws.monadErrorLeftZero[A, B] _),
          "monadError ensure consistency" -> forAll(laws.monadErrorEnsureConsistency[A] _),
          "monadError ensureOr consistency" -> forAll(laws.monadErrorEnsureOrConsistency[A] _),
          "monadError rethrow attempt" -> forAll(laws.rethrowAttempt[A] _),
          "monadError redeemWith is derived from attempt and flatMap" -> forAll(
            laws.redeemWithDerivedFromAttemptFlatMap[A, B] _
          )
        )
    }
}

object MonadErrorTests {
  def apply[F[_], E](implicit FE: MonadError[F, E]): MonadErrorTests[F, E] =
    new MonadErrorTests[F, E] {
      def laws: MonadErrorLaws[F, E] = MonadErrorLaws[F, E]
    }
}
