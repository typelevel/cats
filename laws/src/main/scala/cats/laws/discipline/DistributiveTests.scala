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

trait DistributiveTests[F[_]] extends FunctorTests[F] {
  def laws: DistributiveLaws[F]

  def distributive[A: Arbitrary, B: Arbitrary, C: Arbitrary, X[_]: Functor, Y[_]: Distributive](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbXA: Arbitrary[X[A]],
    ArbYC: Arbitrary[Y[C]],
    ArbFYA: Arbitrary[F[Y[A]]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqA: Eq[A],
    EqFYXC: Eq[F[Y[X[C]]]],
    EqFYA: Eq[F[Y[A]]],
    EqYFB: Eq[Y[F[B]]]
  ): RuleSet =
    new RuleSet {
      def name: String = "distributive"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(functor[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "distributive distribute identity" -> forAll(laws.distributeIdentity[A, B] _),
          "distributive identity" -> forAll(laws.cosequenceIdentity[A] _),
          "distributive composition" -> forAll(laws.composition[A, B, C, X, Y] _),
          "distributive double cosequence identity" -> forAll(laws.cosequenceTwiceIsId[A, Y] _)
        )
    }
}

object DistributiveTests {
  def apply[F[_]: Distributive]: DistributiveTests[F] =
    new DistributiveTests[F] { def laws: DistributiveLaws[F] = DistributiveLaws[F] }
}
