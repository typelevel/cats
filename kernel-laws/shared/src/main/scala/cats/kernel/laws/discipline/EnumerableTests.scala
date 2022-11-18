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
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait EnumerableTests[A] extends PartialNextTests[A] with PartialPreviousTests[A] {
  def laws: EnumerableLaws[A]

  def enumerable(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new RuleSet {
      override val name: String = "enumerable"
      override val bases: Seq[(String, RuleSet)] = Nil
      override val parents: Seq[RuleSet] = Seq(partialNext, partialPrevious, partialOrder)
      override val props: Seq[(String, Prop)] = Seq(
        "injective to the natural numbers" -> forAll(laws.injectiveToNaturalNumbers _)
      )
    }
}

object BoundlessEnumerableTests {
  def apply[A: BoundlessEnumerable]: BoundlessEnumerableTests[A] =
    new BoundlessEnumerableTests[A] { def laws: BoundlessEnumerableLaws[A] = BoundlessEnumerableLaws[A] }
}

trait BoundlessEnumerableTests[A] extends EnumerableTests[A] {
  def laws: BoundlessEnumerableLaws[A]

  def boundlessEnumerable(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new RuleSet {
      override val name: String = "boundlessEnumerable"
      override val bases: Seq[(String, RuleSet)] = Nil
      override val parents: Seq[RuleSet] = Seq(partialNext, partialPrevious, partialOrder, enumerable)
      override val props: Seq[(String, Prop)] = Seq(
        "bijective to the natural numbers" -> forAll(laws.bijectiveToNaturalNumbers _)
      )
    }
}

object EnumerableTests {
  def apply[A: Enumerable]: EnumerableTests[A] =
    new EnumerableTests[A] { def laws: EnumerableLaws[A] = EnumerableLaws[A] }
}

trait PartialNextTests[A] extends PartialOrderTests[A] {

  def laws: PartialNextLaws[A]

  def partialNext(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "partialNext",
      Some(partialOrder),
      "next(a) > a" -> forAll(laws.nextOrderWeak _),
      "forall a, b. if a < b. next(a) <= b" -> forAll(laws.nextOrderStrong _)
    )

}

trait PartialPreviousTests[A] extends PartialOrderTests[A] {

  def laws: PartialPreviousLaws[A]

  def partialPrevious(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "partialPrevious",
      Some(partialOrder),
      "next(a) > a" -> forAll(laws.previousOrderWeak _),
      "forall a, b. if a < b. next(a) <= b" -> forAll(laws.previousOrderStrong _)
    )

}

trait BoundedEnumerableTests[A] extends OrderTests[A] with PartialNextTests[A] with PartialPreviousTests[A] {

  def laws: BoundedEnumerableLaws[A]

  def boundedEnumerable(implicit
    arbA: Arbitrary[A],
    arbF: Arbitrary[A => A],
    eqOA: Eq[Option[A]],
    eqA: Eq[A]
  ): RuleSet =
    new RuleSet {
      val name: String = "boundedEnumerable"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(partialNext, partialPrevious, order)
      val props: Seq[(String, Prop)] = Seq(
        "min bound is terminal" -> laws.minBoundTerminal,
        "max bound is terminal" -> laws.maxBoundTerminal,
        "partial right identity" -> forAll(laws.partialRightIdentity _),
        "partial left identity" -> forAll(laws.partialLeftIdentity _)
      )
    }

}

object BoundedEnumerableTests {
  def apply[A: BoundedEnumerable]: BoundedEnumerableTests[A] =
    new BoundedEnumerableTests[A] { def laws: BoundedEnumerableLaws[A] = BoundedEnumerableLaws[A] }
}
