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

trait OrderTests[A] extends PartialOrderTests[A] {

  def laws: OrderLaws[A]

  def order(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A]): RuleSet =
    new DefaultRuleSet(
      "order",
      Some(partialOrder),
      "totality" -> forAll(laws.totality _),
      "compare" -> forAll(laws.compare _),
      "max" -> forAll(laws.max _),
      "min" -> forAll(laws.min _)
    )

  @deprecated("use `order` without `Eq` parameters", "2.12.1")
  def order(arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    order(arbA, arbF)
}

object OrderTests {
  def apply[A: Order]: OrderTests[A] =
    new OrderTests[A] { def laws: OrderLaws[A] = OrderLaws[A] }
}
