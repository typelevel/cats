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

trait LowerBoundedTests[A] extends PartialOrderTests[A] {
  def laws: LowerBoundedLaws[A]

  def lowerBounded(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A]): RuleSet =
    new DefaultRuleSet(
      "lowerBounded",
      Some(partialOrder),
      "bound is less than or equals" -> forAll(laws.boundLteqv _)
    )

  @deprecated("use `lowerBounded` without `Eq` parameters", "2.12.1")
  def lowerBounded(arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    lowerBounded(arbA, arbF)
}

object LowerBoundedTests {
  def apply[A: LowerBounded]: LowerBoundedTests[A] =
    new LowerBoundedTests[A] { def laws: LowerBoundedLaws[A] = LowerBoundedLaws[A] }
}

trait UpperBoundedTests[A] extends PartialOrderTests[A] {
  def laws: UpperBoundedLaws[A]

  def upperBounded(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A]): RuleSet =
    new DefaultRuleSet(
      "upperBounded",
      Some(partialOrder),
      "bound is greater than or equals" -> forAll(laws.boundGteqv _)
    )

  @deprecated("use `upperBounded` without `Eq` parameters", "2.12.1")
  def upperBounded(arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    upperBounded(arbA, arbF)
}

object UpperBoundedTests {
  def apply[A: UpperBounded]: UpperBoundedTests[A] =
    new UpperBoundedTests[A] { def laws: UpperBoundedLaws[A] = UpperBoundedLaws[A] }
}
