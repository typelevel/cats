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

trait ComonadTests[F[_]] extends CoflatMapTests[F] {

  def laws: ComonadLaws[F]

  def comonad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenFA: Cogen[F[A]],
    CogenFB: Cogen[F[B]],
    EqFA: Eq[F[A]],
    EqFFA: Eq[F[F[A]]],
    EqFFFA: Eq[F[F[F[A]]]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "comonad",
      parent = Some(coflatMap[A, B, C]),
      "extractCoflattenIdentity" -> forAll(laws.extractCoflattenIdentity[A] _),
      "mapCoflattenIdentity" -> forAll(laws.mapCoflattenIdentity[A] _),
      "coflattenThroughMap" -> forAll(laws.coflattenThroughMap[A] _),
      "coflattenCoherence" -> forAll(laws.coflattenCoherence[A, B] _),
      "coflatMapIdentity" -> forAll(laws.coflatMapIdentity[A, B] _),
      "mapCoflatMapCoherence" -> forAll(laws.mapCoflatMapCoherence[A, B] _),
      "comonad left identity" -> forAll(laws.comonadLeftIdentity[A] _),
      "comonad right identity" -> forAll(laws.comonadRightIdentity[A, B] _)
    )
}

object ComonadTests {
  def apply[F[_]: Comonad]: ComonadTests[F] =
    new ComonadTests[F] {
      def laws: ComonadLaws[F] = ComonadLaws[F]
    }
}
