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

import cats.ContravariantMonoidal
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

trait ContravariantMonoidalTests[F[_]] extends ContravariantSemigroupalTests[F] {
  def laws: ContravariantMonoidalLaws[F]

  def contravariantMonoidal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
    iso: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name = "contravariantMonoidal"
      val parents = Seq(contravariantSemigroupal[A, B, C])
      val bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      val props = Seq(
        "contravariantMonoidal right unit" ->
          forAll(laws.contravariantMonoidalUnitRight[A] _),
        "contravariantMonoidal left unit" ->
          forAll(laws.contravariantMonoidalUnitLeft[A] _),
        "contravariantMonoidal contramap2 compatible contramap left" ->
          forAll(laws.contravariantMonoidalContramap2CompatibleContramapLeft[A, B, C] _),
        "contravariantMonoidal contramap2 compatible contramap right" ->
          forAll(laws.contravariantMonoidalContramap2CompatibleContramapRight[A, B, C] _)
      )
    }
}

object ContravariantMonoidalTests {
  def apply[F[_]: ContravariantMonoidal]: ContravariantMonoidalTests[F] =
    new ContravariantMonoidalTests[F] { def laws: ContravariantMonoidalLaws[F] = ContravariantMonoidalLaws[F] }
}
