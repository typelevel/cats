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
import cats.arrow.Strong

trait StrongTests[F[_, _]] extends ProfunctorTests[F] {
  def laws: StrongLaws[F]

  def strong[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFCD: Arbitrary[F[C, D]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenD: Cogen[D],
    CogenE: Cogen[E],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]],
    EqFAG: Eq[F[A, G]],
    EqFACBC: Eq[F[(A, C), (B, C)]],
    EqFDADB: Eq[F[(D, A), (D, B)]],
    EqFACBD: Eq[F[(A, C), (B, D)]],
    EqFCADB: Eq[F[(C, A), (D, B)]],
    EqFACB: Eq[F[(A, C), B]],
    EqFCAB: Eq[F[(C, A), B]],
    EqFACDBCD: Eq[F[((A, C), D), ((B, C), D)]],
    EqFDCADCB: Eq[F[(D, (C, A)), (D, (C, B))]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "strong",
      parent = Some(profunctor[A, B, C, D, E, G]),
      "first is swapped second" -> forAll(laws.firstIsSwappedSecond[A, B, C] _),
      "second is swapped first" -> forAll(laws.secondIsSwappedFirst[A, B, D] _),
      "lmap equals first and then rmap" -> forAll(laws.lmapEqualsFirstAndThenRmap[A, B, C] _),
      "lmap equals second and then rmap" -> forAll(laws.lmapEqualsSecondAndThenRmap[A, B, C] _),
      "dinaturality of first" -> forAll(laws.dinaturalityFirst[A, B, C, D] _),
      "dinaturality of second" -> forAll(laws.dinaturalitySecond[A, B, C, D] _),
      "first first is dimap" -> forAll(laws.firstFirstIsDimap[A, B, C, D] _),
      "second second is dimap" -> forAll(laws.secondSecondIsDimap[A, B, C, D] _)
    )
}

object StrongTests {
  def apply[F[_, _]: Strong]: StrongTests[F] =
    new StrongTests[F] { def laws: StrongLaws[F] = StrongLaws[F] }
}
