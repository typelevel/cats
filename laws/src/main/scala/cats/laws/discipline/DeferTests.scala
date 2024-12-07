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

import org.scalacheck.{Arbitrary, Prop}
import Prop.*
import org.typelevel.discipline.Laws

trait DeferTests[F[_]] extends Laws {
  def laws: DeferLaws[F]

  def defer[A: Arbitrary](implicit ArbFA: Arbitrary[F[A]], EqFA: Eq[F[A]], EqBool: Eq[Boolean]): RuleSet =
    new DefaultRuleSet(
      name = "defer",
      parent = None,
      "defer Identity" -> forAll(laws.deferIdentity[A] _),
      "defer does not evaluate" -> forAll(laws.deferDoesNotEvaluate[A] _),
      "defer is stack safe" -> forAll(laws.deferIsStackSafe[A] _),
      "defer matches fix" -> forAll(laws.deferMatchesFix[A] _)
    )
}

object DeferTests {
  def apply[F[_]: Defer]: DeferTests[F] =
    new DeferTests[F] { def laws: DeferLaws[F] = DeferLaws[F] }
}
