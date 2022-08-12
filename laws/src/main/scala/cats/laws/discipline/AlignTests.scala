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
import Prop._

import cats.data.Ior
import org.typelevel.discipline.Laws

trait AlignTests[F[_]] extends Laws {
  def laws: AlignLaws[F]

  def align[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[A => C],
    ArbFBtoC: Arbitrary[B => D],
    ArbIorABtoC: Arbitrary[A Ior B => C],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFIorAA: Eq[F[A Ior A]],
    EqFIorAB: Eq[F[A Ior B]],
    EqFIorCD: Eq[F[C Ior D]],
    EqFAssoc: Eq[F[Ior[Ior[A, B], C]]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "align",
      parent = None,
      "align associativity" -> forAll(laws.alignAssociativity[A, B, C] _),
      "align homomorphism" -> forAll { (fa: F[A], fb: F[B], f: A => C, g: B => D) =>
        laws.alignHomomorphism[A, B, C, D](fa, fb, f, g)
      },
      "alignWith consistent" -> forAll { (fa: F[A], fb: F[B], f: A Ior B => C) =>
        laws.alignWithConsistent[A, B, C](fa, fb, f)
      }
    )
}

object AlignTests {
  def apply[F[_]: Align]: AlignTests[F] =
    new AlignTests[F] { def laws: AlignLaws[F] = AlignLaws[F] }
}
