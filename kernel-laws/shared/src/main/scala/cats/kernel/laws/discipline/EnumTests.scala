package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait PartialNextTests[A] extends UpperBoundedTests[A] {
  def laws: PartialNextLaws[A]

  def partialNext(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "partialNext",
      Some(upperBounded),
      "maximum is terminal object" -> laws.maximum,
      "next(a) > a" -> forAll(laws.nextWeak _),
      "forall a, b. if a > b. next(a) >= b" -> forAll(laws.nextStrong _)
    )

}

object PartialNextTests {
  def apply[A : PartialNext]: PartialNextTests[A] =
    new PartialNextTests[A] { def laws: PartialNextLaws[A] = PartialNextLaws[A] }
}
