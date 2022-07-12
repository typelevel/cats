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
import org.scalacheck.Prop._

trait DecidableTests[F[_]] extends ContravariantMonoidalTests[F] {
  def laws: DecidableLaws[F]

  def decidable[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    EqCC: Eq[F[Either[C, C]]],
    EqFEitABC: Eq[F[Either[Either[A, B], C]]],
    EqFTupAEitBC: Eq[F[(A, Either[B, C])]],
    iso: SemigroupalTests.Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name = "decideable"
      val parents = Seq(contravariantMonoidal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "decide consistency" ->
          forAll(laws.decideConsistency[A, B, C] _),
        "decide right identity (zero)" ->
          forAll(laws.decideRightIdentity[A] _),
        "decide left identity (zero)" ->
          forAll(laws.decideLeftIdentity[A] _),
        "decidable left identity" ->
          forAll(laws.decidableDecideLeftIdentity[A] _),
        "decidable right identity" ->
          forAll(laws.decidableDecideRightIdentity[A] _),
        "decidable sum associativity" ->
          forAll(laws.decidableSumAssociativity[A, B, C] _),
        "decidable right distributivity" ->
          forAll(laws.decidableRightDistributivity[A, B, C] _),
        "decidable right sum distributivity" ->
          forAll(laws.decidableRightDistributivitySum[A, B, C] _)
      )
    }
}

object DecidableTests {
  def apply[F[_]: Decidable]: DecidableTests[F] =
    new DecidableTests[F] { def laws: DecidableLaws[F] = DecidableLaws[F] }
}
