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

import cats.instances.eq._

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait FlatMapTests[F[_]] extends ApplyTests[F] {
  def laws: FlatMapLaws[F]

  def flatMap[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
  ): RuleSet = {
    implicit def functorF: Functor[F] = laws.F
    implicit val EqFAB: Eq[F[(A, B)]] =
      ContravariantSemigroupal[Eq].composeFunctor[F].product(EqFA, EqFB)

    new DefaultRuleSet(
      name = "flatMap",
      parent = Some(apply[A, B, C]),
      "flatMap associativity" -> forAll(laws.flatMapAssociativity[A, B, C] _),
      "flatMap consistent apply" -> forAll(laws.flatMapConsistentApply[A, B] _),
      "flatMap from tailRecM consistency" -> forAll(laws.flatMapFromTailRecMConsistency[A, B] _),
      "mproduct consistent flatMap" -> forAll(laws.mproductConsistency[A, B] _),
      "tailRecM consistent flatMap" -> forAll(laws.tailRecMConsistentFlatMap[A] _)
    )
  }
}

object FlatMapTests {
  def apply[F[_]: FlatMap]: FlatMapTests[F] =
    new FlatMapTests[F] { def laws: FlatMapLaws[F] = FlatMapLaws[F] }
}
