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

import cats.arrow.Arrow
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop.*

trait ArrowTests[F[_, _]] extends CategoryTests[F] with StrongTests[F] {
  def laws: ArrowLaws[F]

  def arrow[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFAC: Arbitrary[F[A, C]],
    ArbFCD: Arbitrary[F[C, D]],
    ArbFDE: Arbitrary[F[D, E]],
    ArbFEG: Arbitrary[F[E, G]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenD: Cogen[D],
    CogenE: Cogen[E],
    EqFAA: Eq[F[A, A]],
    EqFAB: Eq[F[A, B]],
    EqFAC: Eq[F[A, C]],
    EqFAD: Eq[F[A, D]],
    EqFAG: Eq[F[A, G]],
    EqFACB: Eq[F[(A, C), B]],
    EqFACBC: Eq[F[(A, C), (B, C)]],
    EqFACBD: Eq[F[(A, C), (B, D)]],
    EqFADCD: Eq[F[(A, D), (C, D)]],
    EqFADCG: Eq[F[(A, D), (C, G)]],
    EqFDADB: Eq[F[(D, A), (D, B)]],
    EqFCADB: Eq[F[(C, A), (D, B)]],
    EqFABC: Eq[F[A, (B, C)]],
    EqFCAB: Eq[F[(C, A), B]],
    EqFACDBCD: Eq[F[((A, C), D), (B, (C, D))]],
    EqFACDBCD2: Eq[F[((A, C), D), ((B, C), D)]],
    EqFDCADCB: Eq[F[(D, (C, A)), (D, (C, B))]]
  ): RuleSet =
    new RuleSet {
      def name: String = "arrow"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] =
        Seq(
          category[A, B, C, D],
          strong[A, B, C, D, E, G]
        )
      def props: Seq[(String, Prop)] =
        Seq(
          "arrow identity" -> laws.arrowIdentity[A],
          "arrow composition" -> forAll(laws.arrowComposition[A, B, C] _),
          "arrow extension" -> forAll(laws.arrowExtension[A, B, C] _),
          "arrow functor" -> forAll(laws.arrowFunctor[A, B, C, D] _),
          "arrow exchange" -> forAll(laws.arrowExchange[A, B, C, D] _),
          "arrow unit" -> forAll(laws.arrowUnit[A, B, C] _),
          "arrow association" -> forAll(laws.arrowAssociation[A, B, C, D] _),
          "split consistent with andThen" -> forAll(laws.splitConsistentWithAndThen[A, B, C, D] _),
          "merge consistent with andThen" -> forAll(laws.mergeConsistentWithAndThen[A, B, C] _)
        )
    }
}

object ArrowTests {
  def apply[F[_, _]: Arrow]: ArrowTests[F] =
    new ArrowTests[F] { def laws: ArrowLaws[F] = ArrowLaws[F] }
}
