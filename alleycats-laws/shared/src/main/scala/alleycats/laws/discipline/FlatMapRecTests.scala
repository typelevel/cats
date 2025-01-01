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

package alleycats.laws.discipline

import cats.*
import cats.laws.FlatMapLaws
import cats.laws.discipline.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

trait FlatMapRecTests[F[_]] extends Laws {
  def laws: FlatMapLaws[F]

  def tailRecM[A: Arbitrary](implicit ArbFA: Arbitrary[F[A]], ArbAFA: Arbitrary[A => F[A]], EqFA: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      name = "flatMapTailRec",
      parent = None,
      "tailRecM consistent flatMap" -> forAll(laws.tailRecMConsistentFlatMap[A] _)
    )
}

object FlatMapRecTests {
  def apply[F[_]: FlatMap]: FlatMapRecTests[F] =
    new FlatMapRecTests[F] { def laws: FlatMapLaws[F] = FlatMapLaws[F] }
}
