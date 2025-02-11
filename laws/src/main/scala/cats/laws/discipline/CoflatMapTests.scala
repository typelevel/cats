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
import org.typelevel.discipline.Laws

trait CoflatMapTests[F[_]] extends Laws with FunctorTests[F] {
  def laws: CoflatMapLaws[F]

  def coflatMap[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenFA: Cogen[F[A]],
    CogenFB: Cogen[F[B]],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqFFA: Eq[F[F[A]]],
    EqFB: Eq[F[B]],
    EqFFFA: Eq[F[F[F[A]]]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "coflatMap",
      parent = Some(functor[A, B, C]),
      "coflatMap associativity" -> forAll(laws.coflatMapAssociativity[A, B, C] _),
      "coflatMap identity" -> forAll(laws.coflatMapIdentity[A, B] _),
      "coflatten coherence" -> forAll(laws.coflattenCoherence[A, B] _),
      "coflatten throughMap" -> forAll(laws.coflattenThroughMap[A] _)
    )
}

object CoflatMapTests {
  def apply[F[_]: CoflatMap]: CoflatMapTests[F] =
    new CoflatMapTests[F] { def laws: CoflatMapLaws[F] = CoflatMapLaws[F] }
}
