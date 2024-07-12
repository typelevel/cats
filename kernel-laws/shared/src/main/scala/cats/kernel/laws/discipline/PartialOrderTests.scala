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
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait PartialOrderTests[A] extends EqTests[A] {

  def laws: PartialOrderLaws[A]

  def partialOrder(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A]): RuleSet =
    new DefaultRuleSet(
      "partialOrder",
      Some(eqv),
      "transitivity" -> forAll(laws.transitivity _),
      "reflexivity lt" -> forAll(laws.reflexivityLt _),
      "reflexivity gt" -> forAll(laws.reflexivityGt _),
      "antisymmetry" -> forAll(laws.antisymmetry _),
      "gt" -> forAll(laws.gt _),
      "gteqv" -> forAll(laws.gteqv _),
      "lt" -> forAll(laws.lt _),
      "partialCompare" -> forAll(laws.partialCompare _),
      "pmax" -> forAll(laws.pmax _),
      "pmin" -> forAll(laws.pmin _)
    )

  @deprecated("use `partialOrder` without `Eq` parameters", "2.12.1")
  def partialOrder(arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    partialOrder(arbA, arbF)
}

object PartialOrderTests {
  def apply[A: PartialOrder]: PartialOrderTests[A] =
    new PartialOrderTests[A] { def laws: PartialOrderLaws[A] = PartialOrderLaws[A] }
}
