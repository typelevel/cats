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

import cats.ContravariantSemigroupal
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

trait ContravariantSemigroupalTests[F[_]] extends ContravariantTests[F] with SemigroupalTests[F] {
  def laws: ContravariantSemigroupalLaws[F]

  def contravariantSemigroupal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
      val name = "contravariantSemigroupal"
      val parents = Seq(contravariant[A, B, C], semigroupal[A, B, C])
      val bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      val props = Seq(
        "contravariantSemigroupal contramap2 delta associates" ->
          forAll(laws.contravariantSemigroupalContramap2DiagonalAssociates[A] _)
      )
    }
}

object ContravariantSemigroupalTests {
  def apply[F[_]: ContravariantSemigroupal]: ContravariantSemigroupalTests[F] =
    new ContravariantSemigroupalTests[F] { def laws: ContravariantSemigroupalLaws[F] = ContravariantSemigroupalLaws[F] }
}
