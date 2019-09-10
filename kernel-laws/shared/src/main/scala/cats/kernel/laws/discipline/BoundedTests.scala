package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait LowerBoundedTests[A] extends PartialOrderTests[A] {
  def laws: LowerBoundedLaws[A]

  def lowerBounded(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "lowerBounded",
      Some(partialOrder),
      "bound is less than or equals" -> forAll(laws.boundLteqv _)
    )
}

object LowerBoundedTests {
  def apply[A: LowerBounded]: LowerBoundedTests[A] =
    new LowerBoundedTests[A] { def laws: LowerBoundedLaws[A] = LowerBoundedLaws[A] }
}

trait UpperBoundedTests[A] extends PartialOrderTests[A] {
  def laws: UpperBoundedLaws[A]

  def upperBounded(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "upperBounded",
      Some(partialOrder),
      "bound is greater than or equals" -> forAll(laws.boundGteqv _)
    )
}

object UpperBoundedTests {
  def apply[A: UpperBounded]: UpperBoundedTests[A] =
    new UpperBoundedTests[A] { def laws: UpperBoundedLaws[A] = UpperBoundedLaws[A] }
}
